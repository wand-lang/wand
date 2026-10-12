import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

# GET /lines/N answers N lines of generated text: the response-writing
# path.


class Handler(BaseHTTPRequestHandler):
    def reply(self, status, body):
        data = body.encode()
        self.send_response(status)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        parts = self.path.split("/")
        if len(parts) == 3 and parts[1] == "lines" and parts[2].isdigit():
            count = int(parts[2])
            self.reply(
                200,
                "".join(
                    f"{i} the quick brown fox jumps over the lazy dog\n"
                    for i in range(1, count + 1)
                ),
            )
        else:
            self.reply(404, "not found")

    def log_message(self, *args):
        pass


ThreadingHTTPServer(("127.0.0.1", int(sys.argv[1])), Handler).serve_forever()
