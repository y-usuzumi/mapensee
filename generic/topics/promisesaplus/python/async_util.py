import io


class AsyncIO:
    @classmethod
    def read_file(cls, event_loop, filename, handler):
        f = io.open(filename)
        fno = f.fileno()

        def _handler(content):
            handler(content)
            f.close()

        event_loop.add_handler(fno, None, event_loop.READ)
