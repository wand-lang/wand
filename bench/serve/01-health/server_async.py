import asyncio
import sys

# GET /health answers 200 and "ok", on asyncio with a small HTTP/1.1
# parser of its own. One connection, one request, as the other servers do.


REASONS = {200: "OK", 400: "Bad Request", 404: "Not Found"}


def answer(method, path, body):
    if method == "GET" and path == "/health":
        return 200, "ok"
    return 404, "not found"


async def handle(reader, writer):
    try:
        head = await reader.readuntil(b"\r\n\r\n")
        lines = head.decode("latin-1").split("\r\n")
        method, path, _ = lines[0].split(" ")
        length = 0
        for line in lines[1:]:
            name, _, value = line.partition(":")
            if name.strip().lower() == "content-length":
                length = int(value)
        body = await reader.readexactly(length)
        status, text = answer(method, path, body)
        data = text.encode()
        writer.write(
            b"HTTP/1.1 %d %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n"
            % (status, REASONS[status].encode(), len(data))
            + data
        )
        await writer.drain()
    except (asyncio.IncompleteReadError, ValueError, ConnectionError):
        pass
    finally:
        writer.close()


async def main():
    server = await asyncio.start_server(handle, "127.0.0.1", int(sys.argv[1]))
    await server.serve_forever()


asyncio.run(main())
