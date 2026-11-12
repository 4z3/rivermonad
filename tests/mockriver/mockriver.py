"""A stand-in for river, for testing rivermonad without a compositor.

It speaks the server side of the Wayland wire protocol for the interfaces
rivermonad binds, generated from protocol/*.xml: it announces outputs, a
seat and windows, drives manage and render sequences, and records every
request the window manager makes so a test can check them.

It checks what rivermonad sends. It does not check that river would accept
it: ordering rules and protocol errors are river's and are not modelled.
"""
import glob
import os
import select
import socket
import struct
import time
import xml.etree.ElementTree as ET

SERVER_ID_BASE = 0xFF000000


class Protocol:
    def __init__(self, directory):
        self.interfaces = {}
        for path in sorted(glob.glob(os.path.join(directory, "*.xml"))):
            for iface in ET.parse(path).getroot().iter("interface"):
                def messages(tag):
                    return [(m.get("name"),
                             [(a.get("name"), a.get("type"), a.get("interface")) for a in m.findall("arg")],
                             m.get("type") == "destructor")
                            for m in iface.findall(tag)]
                self.interfaces[iface.get("name")] = {
                    "version": int(iface.get("version")),
                    "requests": messages("request"),
                    "events": messages("event"),
                }


def _pad(n):
    return (4 - n % 4) % 4


class Request:
    def __init__(self, iface, obj, name, args):
        self.iface, self.obj, self.name, self.args = iface, obj, name, args

    def __repr__(self):
        return "%s@%d.%s(%s)" % (self.iface, self.obj, self.name,
                                 ", ".join("%s=%r" % kv for kv in self.args.items()))


class MockRiver:
    def __init__(self, runtime_dir, display, protocol_dir, wm_version=4, xkb_version=2):
        self.proto = Protocol(protocol_dir)
        self.path = os.path.join(runtime_dir, display)
        if os.path.exists(self.path):
            os.unlink(self.path)
        self.listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.listener.bind(self.path)
        self.listener.listen(1)
        self.globals = [(1, "river_window_manager_v1", wm_version),
                        (2, "river_xkb_bindings_v1", xkb_version)]
        self.conn = None
        self.log = []
        self._reset()

    # -- connection ---------------------------------------------------------

    def _reset(self):
        self.objects = {1: "wl_display"}
        self.buf = b""
        self.next_server_id = SERVER_ID_BASE
        self.closed = False
        self.dirty = False
        self.stopped = False
        self.wm = None
        self.xkb = None
        self.bindings = {}   # binding object -> (keysym, modifiers)
        self.windows = {}    # window object -> identifier
        self.nodes = {}      # node object -> window object

    def accept(self, timeout=10):
        self.listener.settimeout(timeout)
        self.conn, _ = self.listener.accept()
        self._reset()
        self.pump(lambda: self.wm is not None and self.xkb is not None, timeout)
        self.pump(idle=0.2)

    def new_id(self, iface):
        i = self.next_server_id
        self.next_server_id += 1
        self.objects[i] = iface
        return i

    # -- wire ---------------------------------------------------------------

    def send(self, obj, event, *values):
        iface = self.objects[obj]
        events = self.proto.interfaces[iface]["events"]
        opcode = [e[0] for e in events].index(event)
        sig = events[opcode][1]
        assert len(sig) == len(values), (iface, event, sig, values)
        body = b""
        for (_, typ, _), v in zip(sig, values):
            if typ in ("int", "fixed"):
                body += struct.pack("<i", v)
            elif typ in ("uint", "object", "new_id"):
                body += struct.pack("<I", v)
            elif typ == "string":
                if v is None:
                    body += struct.pack("<I", 0)
                else:
                    raw = v.encode() + b"\0"
                    body += struct.pack("<I", len(raw)) + raw + b"\0" * _pad(len(raw))
            else:
                raise NotImplementedError(typ)
        self.conn.sendall(struct.pack("<II", obj, ((8 + len(body)) << 16) | opcode) + body)

    def _parse(self):
        while len(self.buf) >= 8:
            obj, word = struct.unpack_from("<II", self.buf)
            size, opcode = word >> 16, word & 0xFFFF
            if len(self.buf) < size:
                return
            body, self.buf = self.buf[8:size], self.buf[size:]
            iface = self.objects.get(obj)
            if iface is None:
                raise AssertionError("request on unknown object %d" % obj)
            name, sig, destructor = self.proto.interfaces[iface]["requests"][opcode]
            args, off = {}, 0
            for aname, typ, aiface in sig:
                if typ in ("int", "fixed"):
                    args[aname] = struct.unpack_from("<i", body, off)[0]
                    off += 4
                elif typ in ("uint", "object"):
                    args[aname] = struct.unpack_from("<I", body, off)[0]
                    off += 4
                elif typ == "string":
                    n = struct.unpack_from("<I", body, off)[0]
                    off += 4
                    args[aname] = None if n == 0 else body[off:off + n - 1].decode()
                    off += n + _pad(n)
                elif typ == "new_id":
                    if aiface is None:   # wl_registry.bind
                        n = struct.unpack_from("<I", body, off)[0]
                        off += 4
                        aiface = body[off:off + n - 1].decode()
                        off += n + _pad(n)
                        args["version"] = struct.unpack_from("<I", body, off)[0]
                        off += 4
                    new = struct.unpack_from("<I", body, off)[0]
                    off += 4
                    self.objects[new] = aiface
                    args[aname] = new
                    args["interface"] = aiface
                elif typ == "fd":
                    pass
                else:
                    raise NotImplementedError(typ)
            assert off == len(body), (iface, name, off, len(body))
            req = Request(iface, obj, name, args)
            self.log.append(req)
            self._handle(req)
            if destructor:
                del self.objects[obj]
                self.send(1, "delete_id", obj)

    def _handle(self, r):
        key = (r.iface, r.name)
        if key == ("wl_display", "get_registry"):
            for name, iface, version in self.globals:
                self.send(r.args["registry"], "global", name, iface, version)
        elif key == ("wl_display", "sync"):
            self.send(r.args["callback"], "done", 0)
            del self.objects[r.args["callback"]]
            self.send(1, "delete_id", r.args["callback"])
        elif key == ("wl_registry", "bind"):
            if r.args["interface"] == "river_window_manager_v1":
                self.wm = r.args["id"]
            elif r.args["interface"] == "river_xkb_bindings_v1":
                self.xkb = r.args["id"]
        elif key == ("river_xkb_bindings_v1", "get_xkb_binding"):
            self.bindings[r.args["id"]] = (r.args["keysym"], r.args["modifiers"])
        elif key == ("river_window_v1", "get_node"):
            self.nodes[r.args["id"]] = r.obj
        elif key == ("river_window_manager_v1", "manage_dirty"):
            self.dirty = True
        elif key == ("river_window_manager_v1", "stop"):
            self.stopped = True
            self.send(self.wm, "finished")

    def pump(self, until=None, timeout=10, idle=None):
        """Read requests until `until()` holds, or until nothing arrives for
        `idle` seconds. Returns False on timeout or when the peer is gone."""
        deadline = time.time() + timeout
        while True:
            if until is not None and until():
                return True
            wait = idle if idle is not None else max(0, deadline - time.time())
            ready, _, _ = select.select([self.conn], [], [], wait)
            if not ready:
                if idle is not None:
                    return until is None
                return False
            try:
                data = self.conn.recv(65536)
            except ConnectionResetError:
                data = b""
            if not data:
                self.closed = True
                return until is not None and until()
            self.buf += data
            self._parse()

    # -- scenario helpers ---------------------------------------------------

    def add_output(self, x, y, width, height):
        o = self.new_id("river_output_v1")
        self.send(self.wm, "output", o)
        self.send(o, "position", x, y)
        self.send(o, "dimensions", width, height)
        return o

    def add_seat(self):
        s = self.new_id("river_seat_v1")
        self.send(self.wm, "seat", s)
        return s

    def add_window(self, identifier, app_id, title):
        w = self.new_id("river_window_v1")
        self.send(self.wm, "window", w)
        self.send(w, "dimensions_hint", 0, 0, 0, 0)
        self.send(w, "app_id", app_id)
        self.send(w, "title", title)
        self.send(w, "identifier", identifier)
        self.windows[w] = identifier
        return w

    def close_window(self, w):
        self.send(w, "closed")
        del self.windows[w]

    def cycle(self):
        """One manage sequence and one render sequence. Returns the requests
        made during them."""
        start = len(self.log)
        self.dirty = False
        self.send(self.wm, "manage_start")
        ok = self.pump(lambda: any(r.name == "manage_finish" for r in self.log[start:]))
        assert ok, "no manage_finish"
        self.pump(idle=0.05)
        if self.stopped:
            # The window manager is leaving: no more sequences for it.
            self.pump(lambda: self.closed, timeout=5)
            return self.log[start:]
        for r in self.log[start:]:
            if (r.iface, r.name) == ("river_window_v1", "propose_dimensions") and r.obj in self.windows:
                self.send(r.obj, "dimensions", r.args["width"] or 640, r.args["height"] or 480)
        mark = len(self.log)
        self.send(self.wm, "render_start")
        ok = self.pump(lambda: any(r.name == "render_finish" for r in self.log[mark:]))
        assert ok, "no render_finish"
        self.pump(idle=0.05)
        return self.log[start:]

    def settle(self, rounds=20):
        """Run sequences for as long as the window manager asks for them."""
        out = []
        for _ in range(rounds):
            self.pump(idle=0.15)
            if self.closed or self.stopped or not self.dirty:
                return out
            out += self.cycle()
        raise AssertionError("the window manager keeps asking for manage sequences")

    def press(self, keysym, modifiers):
        """A bound key goes down and up. Returns the requests that followed."""
        found = [b for b, km in self.bindings.items() if km == (keysym, modifiers)]
        assert len(found) == 1, "binding %#x with modifiers %d registered %d times" % (keysym, modifiers, len(found))
        self.send(found[0], "pressed")
        out = self.cycle()
        self.send(found[0], "released")
        out += self.cycle()
        return out + self.settle()

    def geometry(self, requests):
        """Window identifier -> (x, y, width, height) of the content, from
        the proposals and positions in some requests."""
        pos, dims = {}, {}
        for r in requests:
            if (r.iface, r.name) == ("river_node_v1", "set_position") and self.nodes.get(r.obj) in self.windows:
                pos[self.windows[self.nodes[r.obj]]] = (r.args["x"], r.args["y"])
            if (r.iface, r.name) == ("river_window_v1", "propose_dimensions") and r.obj in self.windows:
                dims[self.windows[r.obj]] = (r.args["width"], r.args["height"])
        return {k: pos.get(k, (None, None)) + dims.get(k, (None, None)) for k in set(pos) | set(dims)}
