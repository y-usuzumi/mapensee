class Event:
    def __init__(self, event_loop):
        self._event_loop = event_loop
        self._handlers = []

    @property
    def event_loop(self):
        return self._event_loop

    def add_handler(self, handler):
        self._handlers.append(handler)

    def handle(self):
        for handler in self._handlers:
            handler(self, self.event_loop)
