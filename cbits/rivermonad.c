/* Small C helpers for rivermonad: compiling an xkb keymap with libxkbcommon
 * (exactly as the system resolves layout/variant/options) and sending
 * Wayland messages with file descriptors attached (SCM_RIGHTS). */
#define _GNU_SOURCE
#include <errno.h>
#include <math.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <unistd.h>
#include <xkbcommon/xkbcommon.h>

static const char *nz(const char *s) { return (s && *s) ? s : NULL; }

/* Compile a keymap from rules/model/layout/variant/options (empty strings
 * mean "default") into a sealed memfd holding it as text, NUL-terminated.
 * Returns the fd, or -1 if the names do not compile. */
int rivermonad_keymap_fd(const char *rules, const char *model, const char *layout,
                         const char *variant, const char *options)
{
    struct xkb_context *ctx = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    if (!ctx) return -1;
    struct xkb_rule_names names = {
        .rules = nz(rules), .model = nz(model), .layout = nz(layout),
        .variant = nz(variant), .options = nz(options),
    };
    struct xkb_keymap *keymap = xkb_keymap_new_from_names(ctx, &names, XKB_KEYMAP_COMPILE_NO_FLAGS);
    xkb_context_unref(ctx);
    if (!keymap) return -1;
    char *text = xkb_keymap_get_as_string(keymap, XKB_KEYMAP_FORMAT_TEXT_V1);
    xkb_keymap_unref(keymap);
    if (!text) return -1;

    size_t size = strlen(text) + 1;
    int fd = memfd_create("rivermonad-keymap", MFD_CLOEXEC | MFD_ALLOW_SEALING);
    if (fd < 0) { free(text); return -1; }
    size_t off = 0;
    while (off < size) {
        ssize_t n = write(fd, text + off, size - off);
        if (n <= 0) { if (errno == EINTR) continue; close(fd); free(text); return -1; }
        off += (size_t)n;
    }
    free(text);
    fcntl(fd, F_ADD_SEALS, F_SEAL_SHRINK | F_SEAL_GROW | F_SEAL_WRITE | F_SEAL_SEAL);
    return fd;
}

/* Send len bytes on a socket with nfds file descriptors attached to the
 * first chunk. Returns 0, or -1 with errno set. */
int rivermonad_send_fds(int sock, const char *buf, size_t len, const int *fds, int nfds)
{
    size_t sent = 0;
    while (sent < len) {
        struct iovec iov = { .iov_base = (void *)(buf + sent), .iov_len = len - sent };
        struct msghdr msg = { .msg_iov = &iov, .msg_iovlen = 1 };
        char cbuf[CMSG_SPACE(sizeof(int) * 28)];
        if (sent == 0 && nfds > 0) {
            if (nfds > 28) { errno = EINVAL; return -1; }
            memset(cbuf, 0, sizeof cbuf);
            msg.msg_control = cbuf;
            msg.msg_controllen = CMSG_SPACE(sizeof(int) * nfds);
            struct cmsghdr *c = CMSG_FIRSTHDR(&msg);
            c->cmsg_level = SOL_SOCKET;
            c->cmsg_type = SCM_RIGHTS;
            c->cmsg_len = CMSG_LEN(sizeof(int) * nfds);
            memcpy(CMSG_DATA(c), fds, sizeof(int) * nfds);
        }
        ssize_t n = sendmsg(sock, &msg, MSG_NOSIGNAL);
        if (n < 0) { if (errno == EINTR) continue; return -1; }
        sent += (size_t)n;
    }
    return 0;
}

/* Draw a soft drop shadow into a new shared-memory file, for a wl_shm
 * buffer (ARGB8888, premultiplied, little endian = B,G,R,A in memory).
 *
 * The buffer is (box_w + 2*radius) x (box_h + 2*radius): the shadow of a
 * box_w x box_h rectangle sitting at (radius, radius), fading out over
 * `radius` pixels with a Gaussian profile. The window itself covers the
 * box shifted by (-dx, -dy) (the shadow is drawn offset by dx, dy); that
 * area is left transparent so translucent windows are not darkened.
 *
 * Returns the fd (its size is width*height*4), or -1. */
int rivermonad_shadow_fd(int box_w, int box_h, int radius, int dx, int dy,
                         double r, double g, double b, double opacity)
{
    if (box_w <= 0 || box_h <= 0 || radius < 0) return -1;
    int w = box_w + 2 * radius, h = box_h + 2 * radius;
    size_t size = (size_t)w * (size_t)h * 4;

    int fd = memfd_create("rivermonad-shadow", MFD_CLOEXEC | MFD_ALLOW_SEALING);
    if (fd < 0) return -1;
    if (ftruncate(fd, (off_t)size) < 0) { close(fd); return -1; }
    unsigned char *px = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (px == MAP_FAILED) { close(fd); return -1; }

    /* alpha by squared distance to the box, for d^2 in [0, radius^2] */
    int r2max = radius * radius;
    unsigned char *lut = malloc((size_t)r2max + 1);
    if (!lut) { munmap(px, size); close(fd); return -1; }
    double sigma = radius > 0 ? radius / 2.5 : 1.0;
    for (int d2 = 0; d2 <= r2max; d2++) {
        double a = opacity * exp(-(double)d2 / (2.0 * sigma * sigma));
        lut[d2] = (unsigned char)(a * 255.0 + 0.5);
    }

    int x0 = radius, x1 = radius + box_w - 1, y0 = radius, y1 = radius + box_h - 1;
    /* the window, in buffer coordinates */
    int wx0 = radius - dx, wx1 = wx0 + box_w - 1, wy0 = radius - dy, wy1 = wy0 + box_h - 1;
    for (int y = 0; y < h; y++) {
        int ddy = y < y0 ? y0 - y : (y > y1 ? y - y1 : 0);
        unsigned char *row = px + (size_t)y * (size_t)w * 4;
        for (int x = 0; x < w; x++) {
            int ddx = x < x0 ? x0 - x : (x > x1 ? x - x1 : 0);
            int d2 = ddx * ddx + ddy * ddy;
            unsigned char a = (d2 > r2max) ? 0 : lut[d2];
            if (x >= wx0 && x <= wx1 && y >= wy0 && y <= wy1) a = 0;
            row[4 * x + 0] = (unsigned char)(b * a + 0.5);
            row[4 * x + 1] = (unsigned char)(g * a + 0.5);
            row[4 * x + 2] = (unsigned char)(r * a + 0.5);
            row[4 * x + 3] = a;
        }
    }
    free(lut);
    munmap(px, size);
    return fd;
}
