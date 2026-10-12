import json
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

# POST /sum with {"xs": [...]} answers {"sum": n}: decode a small JSON
# body, add it up, and encode the answer.


class Handler(BaseHTTPRequestHandler):
    def reply(self, status, body):
        data = body.encode()
        self.send_response(status)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        self.reply(404, "not found")

    def do_POST(self):
        if self.path != "/sum":
            self.reply(404, "not found")
            return
        length = int(self.headers.get("Content-Length", "0"))
        try:
            xs = json.loads(self.rfile.read(length))["xs"]
        except (ValueError, KeyError, TypeError) as e:
            self.reply(400, str(e))
            return
        self.reply(200, json.dumps({"sum": sum(xs)}, separators=(",", ":")))

    def log_message(self, *args):
        pass


ThreadingHTTPServer(("127.0.0.1", int(sys.argv[1])), Handler).serve_forever()
