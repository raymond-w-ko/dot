import re
import os
from plasmaplace_exiter import exit_plasmaplace


def bencode(value):
    if isinstance(value, int):
        return "i" + value + "e"
    elif isinstance(value, str):
        return str(len(value.encode("utf-8"))) + ":" + value
    elif isinstance(value, list):
        return "l" + "".join(map(bencode, value)) + "e"
    elif isinstance(value, dict):
        enc = ["d"]
        keys = list(value.keys())
        keys.sort()
        for k in keys:
            enc.append(bencode(k))
            enc.append(bencode(value[k]))
        enc.append("e")
        return "".join(enc)
    else:
        raise TypeError("can't bencode " + value)


def read_socket(sock, n):
    data = b""
    rem = n
    while True:
        new_data = sock.recv(rem)
        data += new_data
        if len(new_data) == 0:
            if len(data) == 0:
                exit_plasmaplace(1)
            return data
        else:
            rem -= len(new_data)


def bdecode(sock, char=None):
    if char is None:
        char = read_socket(sock, 1)
    if char == b"l":
        _list = []
        while True:
            char = read_socket(sock, 1)
            if char == b"e":
                return _list
            _list.append(bdecode(sock, char))
    elif char == b"d":
        d = {}
        while True:
            char = read_socket(sock, 1)
            if char == b"e":
                return d
            key = bdecode(sock, char)
            d[key] = bdecode(sock)
    elif char == b"i":
        i = b""
        while True:
            char = read_socket(sock, 1)
            if char == b"e":
                return int(i.decode("utf-8"))
            i += char
    elif char.isdigit():
        i = int(char)
        while True:
            char = read_socket(sock, 1)
            if char == b":":
                return read_socket(sock, i).decode("utf-8")
            i = 10 * i + int(char)
    elif char == "":
        raise EOFError("unexpected end of bdecode data")
    else:
        raise TypeError("unexpected type " + char + "in bdecode data")


def get_shadow_browser_target(project_path):
    path = os.path.join(project_path, "shadow-cljs.edn")
    with open(path, "r") as f:
        code = f.read()
    code = code.replace("\n", " ")
    idx = code.index(":builds")
    code = code[idx:]
    m = re.search(r"\s*(\:\w+)\s*\{\:target\s+\:browser.*", code)
    if m is None:
        return None
    else:
        return m.group(1)


class StreamBuffer:
    def __init__(self, header):
        self.header = header
        self.appended_header = False
        self.value = None
        self.buf = []

    def append(self, msg):
        self.buf.append(msg)

    def get_lines(self):
        if len(self.buf) == 0:
            return []
        ret = []
        ret.append(self.header)
        lines = "".join(self.buf).split("\n")
        ret += lines
        return ret

    def get_value(self):
        if self.value:
            return self.value
        ret = "".join(self.buf).strip()

        self.value = ret
        return self.value
