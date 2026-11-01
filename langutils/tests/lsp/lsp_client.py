"""A minimal LSP client over stdio, for testing sc_lsp."""
import json
import subprocess
import threading
import queue
import time


class LspClient:
    def __init__(self, command):
        self.proc = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE)
        self.next_id = 0
        self.messages = queue.Queue()
        self.stderr = []
        threading.Thread(target=self._read, daemon=True).start()
        threading.Thread(target=self._read_err, daemon=True).start()
        self.notifications = []

    def _read_err(self):
        for line in self.proc.stderr:
            self.stderr.append(line.decode(errors='replace'))

    def _read(self):
        out = self.proc.stdout
        while True:
            length = None
            while True:
                line = out.readline()
                if not line:
                    return
                line = line.strip()
                if not line:
                    break
                if line.lower().startswith(b'content-length:'):
                    length = int(line.split(b':')[1])
            self.messages.put(json.loads(out.read(length)))

    def send(self, message):
        body = json.dumps(message).encode()
        self.proc.stdin.write(b'Content-Length: %d\r\n\r\n' % len(body) + body)
        self.proc.stdin.flush()

    def notify(self, method, params):
        self.send({'jsonrpc': '2.0', 'method': method, 'params': params})

    def request(self, method, params, timeout=60):
        self.next_id += 1
        rid = self.next_id
        self.send({'jsonrpc': '2.0', 'id': rid, 'method': method, 'params': params})
        deadline = time.time() + timeout
        while time.time() < deadline:
            m = self.messages.get(timeout=deadline - time.time())
            if m.get('id') == rid and 'method' not in m:
                if 'error' in m:
                    raise RuntimeError(m['error'])
                return m['result']
            self.notifications.append(m)
        raise TimeoutError(method)

    def wait_for(self, predicate, timeout=60):
        for m in self.notifications:
            if predicate(m):
                self.notifications.remove(m)
                return m
        deadline = time.time() + timeout
        while time.time() < deadline:
            try:
                m = self.messages.get(timeout=max(0.01, deadline - time.time()))
            except queue.Empty:
                break
            if predicate(m):
                return m
            self.notifications.append(m)
        raise TimeoutError('notification')

    def diagnostics(self, uri, version=None, timeout=60, after_library=False):
        """The diagnostics last published for the document, waiting for a publish of at least that version."""
        def matches(m):
            if m.get('method') != 'textDocument/publishDiagnostics' or m['params']['uri'] != uri:
                return False
            return version is None or m['params'].get('version', 0) >= version
        found = self.wait_for(matches, timeout)
        # Later publishes for the same version, e.g., after the library was rebuilt.
        while True:
            try:
                found = self.wait_for(matches, 0.3)
            except TimeoutError:
                return found['params']['diagnostics']

    def library_built(self, timeout=120):
        return self.wait_for(lambda m: m.get('method') == 'window/logMessage'
                             and 'compiled' in m['params']['message'], timeout)['params']['message']

    def close(self):
        try:
            self.request('shutdown', None, timeout=10)
            self.notify('exit', None)
            self.proc.wait(timeout=10)
        except Exception:
            self.proc.kill()
        return self.proc.returncode
