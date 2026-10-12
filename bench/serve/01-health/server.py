import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

# GET /health answers 200 and "ok": the connection and dispatch path, with
# no work in the handler.


class Handler(BaseHTTPRequestHandler):
    def reply(self, status, body):
        data = body.encode()
        self.send_response(status)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        if self.path == "/health":
            self.reply(200, "ok")
        else:
            self.reply(404, "not found")

    def log_message(self, *args):
        pass


ThreadingHTTPServer(("127.0.0.1", int(sys.argv[1])), Handler).serve_forever()
