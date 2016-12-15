'''Verifies if `Divide and Conquer` algorithm is a legit memory-saving solution to matrix multiplication.

The memory consumption of splitted multiplication should be ~1/4 of native_multiplication.
The time consumption should be almost the same.
'''
import numpy as npy
import time


def native_multiplication(a, b):
    return npy.dot(a, b)


def _split(m):
    size = m.shape[0]
    subsize = int(size ** 0.5)
    return [
        [m[:subsize, :subsize], m[:subsize, subsize:]],
        [m[subsize:, :subsize], m[subsize:, subsize:]]
    ]


def splitted_multiplication(a, b):
    sub_a = _split(a)
    sub_b = _split(b)
    sub = [
        [
            npy.dot(sub_a[0][0], sub_b[0][0]) + npy.dot(sub_a[0][1], sub_b[1][0]),
            npy.dot(sub_a[0][0], sub_b[0][1]) + npy.dot(sub_a[0][1], sub_b[1][1])
        ],
        [
            npy.dot(sub_a[1][0], sub_b[0][0]) + npy.dot(sub_a[1][1], sub_b[1][0]),
            npy.dot(sub_a[1][0], sub_b[0][1]) + npy.dot(sub_a[1][1], sub_b[1][1])
        ]
    ]
    return npy.concatenate([npy.concatenate(sub[0], axis=1), npy.concatenate(sub[1], axis=1)], axis=0)


def test(func, l, r):
    start_time = time.time()
    for i in xrange(10):
        ret = func(l, r)
        print("Done %d" % i)
    end_time = time.time()
    return end_time - start_time


if __name__ == '__main__':
    l = npy.random.rand(1000, 1000)
    r = npy.random.rand(1000, 1000)
    print("Starting native_multiplication...")
    total_time = test(native_multiplication, l, r)
    print("Total time(s): %s" % total_time)

    print("Starting splitted_multiplication...")
    total_time = test(splitted_multiplication, l, r)
    print("Total time(s): %s" % total_time)
