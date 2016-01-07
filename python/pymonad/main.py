from operator import __add__
from functools import reduce
from pymonad import *


# Python version

def expl_sum(n):
    return list(
        reduce(
            __add__,
            [_expl_sum(n, x) for x in range(1, n+1)],
            []
        )
    )

def _expl_sum(m, n):
    if m == n:
        return [[m]]
    return [
        [n] + x
        for x in reduce(
            __add__,
            [_expl_sum(m-n, y) for y in range(n, m-n+1)],
            []
        )
    ]


# Haskell version:

# module ExplosiveSum where
#
# explSum :: Int -> [[Int]]
# explSum n = concatMap (_explSum n) [1..n]
#
# _explSum :: Int -> Int -> [[Int]]
# _explSum m n
#   | m == n = [[m]]
#   | otherwise = map (n:) $ concatMap (_explSum (m-n)) [n..m-n]


# PyMonad version

# TODO:
