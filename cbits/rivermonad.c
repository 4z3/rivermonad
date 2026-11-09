/* Small C helpers for rivermonad: compiling an xkb keymap with libxkbcommon
 * (exactly as the system resolves layout/variant/options) and sending
 * Wayland messages with file descriptors attached (SCM_RIGHTS). */
#define _GNU_SOURCE
#include <errno.h>
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
