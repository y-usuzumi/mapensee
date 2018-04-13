import time
from threading import Thread
from event_loop import EventLoop


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


if __name__ == '__main__':
    loop = EventLoop()
    t = Thread(target=control, args=(loop, ))
    t.daemon = True
    t.start()
    loop.run()
