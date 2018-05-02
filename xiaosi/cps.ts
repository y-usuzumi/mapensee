/**
 * 后续传递风格（Continuation Passing Style，或CPS）是一种编程风格，
 * 其函数并不直接返回结果，而是通过传入一个“后续”来决定接下来要做的事情。
 */

/**
 * 什么是“后续”
 *
 * 先举个例子：
 */

let example1 = [a => 2*a, a => 4*a, a => 8*a].map(f => f(2));
console.log(example1);

/**
 * 好像只是一个更加晦涩版本的：
 *     [2, 4, 8].map(a => a*2)
 * 而已。前者是把值应用在函数上，而后者是把函数应用在值上。
 * 然而这个关系的反转就是CPS的精髓。
 *
 * 从CPS的角度来说，`f => f(2)`是一个“悬挂计算”，其通用类型是：
 *     (A => R) => R
 * 当接收一个类型为A => R的函数时，返回类型为R的最终结果。
 *
 *
 * 使用“后续”有哪些好处？
 * TODO:
 */

/**
 * 后续的传递
 *
 * pythagoras
 */

/** 无后续版本 */
function add(x: number, y: number): number {
    return x + y;
}

function square(x: number): number {
    return x * x;
}

function pythagoras(x: number, y: number): number {
    return add(square(x), square(y));
}

/** 有后续版本 */
function addCPS<R>(x: number, y: number): ((f: (a: number) => R) => R) {
    return (f: (a: number) => R): R => {
        return f(x + y);
    }
}

function squareCPS<R>(x: number): ((f: (a: number) => R) => R) {
    return (f: (a: number) => R): R => {
        return f(x * x);
    }
}

function pythagorasCPS<R>(x: number, y: number): ((f: (a: number) => R) => R) {
    // 小四姑娘，你可以用addCPS和squareCPS来实现我嘛？
    // 答案：
    return (f: (a: number) => R): R =>
        squareCPS<R>(x)(xSquared =>
            squareCPS<R>(y)(ySquared =>
                addCPS<R>(xSquared, ySquared)(result =>
                    f(result)
                )
            )
        );
}

function log(a: number): any {
    console.log(a);
}

pythagorasCPS(3, 4)(log);
