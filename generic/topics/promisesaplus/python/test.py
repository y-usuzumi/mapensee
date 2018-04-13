import time
from threading import Thread
from event_loop import EPollEventLoop
from async_util import AsyncIO


def control(loop):
    print("Hello, ", end='')

    def i_love_u():
        print("[%d]" % time.time(), end=' ')
        print("我爱你")

    def u_rock():
        print("[%d]" % time.time(), end=' ')
        print("小四姑娘")

    loop.set_interval(i_love_u, 1000)
    loop.set_timeout(u_rock, 5000)
    print("world!")
    AsyncIO.read_file(loop, '/home/kj/temp/fff', None)


if __name__ == '__main__':
    loop = EPollEventLoop()
    t = Thread(target=control, args=(loop, ))
    t.daemon = True
    t.start()
    loop.run()
