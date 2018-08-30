#!/usr/bin/env python3
print(
    (lambda fn:
        (lambda f: f(f))(lambda f:
            fn(lambda n: f(f)(n))
        ))(lambda g:
            lambda n: 1 if n in [1, 2] else g(n-1) + g(n-2)
        )(10)
)
