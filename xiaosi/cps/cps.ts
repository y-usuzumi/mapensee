/**
 * 之后会有用的一些东西：
 */

function log(a: number): any {
    console.log(a);
}

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
    // TODO: 小四姑娘，你可以用addCPS和squareCPS来实现我嘛？
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

/** TEST */
pythagorasCPS(3, 4)(log);  // 25


/**
 * CPS的串联太繁琐，函数越套越深，如果能无痛串联该多好。。。
 * 原本想用TypeScript的interface，然而好像没有办法
 * 创建一个类，直接实现带有function的hybrid type接口，
 * 所以还是老老实实用组合方式做。。。
 */

class CPS<a, R> {
    _f: (cont: (a: a) => R) => R

    constructor(f: (cont: (a: a) => R) => R) {
        this._f = f
    }

    get f() {
        return this._f;
    }

    public chain<b>(transformer: (a: a) => CPS<b, R>): CPS<b, R> {
        // TODO: 小四姑娘，你可以实现我嘛？
        // 答案：
        return new CPS<b, R>(f => this._f(aResult => transformer(aResult).run(f)));
    }

    public run(f: (a: a) => R): R {
        return this._f(f);
    }
}

/**
 * 于是我们可以把addCPS和squareCPS都用CPS包装一下：
 */

function addCPS2<R>(x: number, y: number): CPS<number, R> {
    return new CPS<number, R>(addCPS(x, y));
}

function squareCPS2<R>(x: number): CPS<number, R> {
    return new CPS<number, R>(squareCPS(x));
}

/**
 * 这样，可以把pythagorasCPS简化为：
 */

function pythagorasCPS2<R>(x: number, y: number): CPS<number, R> {
    // TODO: 小四姑娘，你可以用addCPS2, squareCPS2和chain实现我嘛？
    // 答案：
    return squareCPS2<R>(x).chain(
        xSquared => squareCPS2<R>(y).chain(
            ySquared => addCPS2<R>(xSquared, ySquared)
        )
    );
}

/** TEST */
pythagorasCPS2(3, 4).run(log);  // 25


/**
 * 没办法，由于x^2需要留到y^2算完之后才能使用，所以一定要创建闭包。。
 * 两层，不能再少了。。。
 *
 * 相比之下，如果运算都是串联关系的话，那带来的可读性提升是非常明显的。
 * 。。。
 *
 * 我们再写两个helper函数：
 */

/** 这个函数把一个普通函数变成CPS1那种形式 */
function suspend<a, R>(f: (...args) => a): (...args) => ((cont: (aResult: a) => R) => R) {
    // TODO: 小四姑娘，你可以实现我嘛？
    // 答案：
    return (...args): ((cont: (aResult: a) => R) => R) => {
        return (cont: (aResult: a) => R): R => {
            return cont(f(...args));
        };
    };
}

/** 这个函数更进一步地把一个普通函数封装成CPS类实例的形式 */
function cps<a, R>(f: (...args) => a): (...args) => CPS<a, R> {
    // TODO：小四姑娘，你可以用上面的suspend来实现我嘛，这个很简单。。。
    return (...args): CPS<a, R> => {
        return new CPS<a, R>(suspend<a, R>(f)(...args));
    };
}

/**
 * 生活不要太美好。。。
 */

// 妈的我一定要定义成函数才能指定泛型参数。。。我tm不如直接any。。。
// 生活一点也不美好。。。
function addCPS3<R>(x: number, y: number): CPS<number, R> {
    return cps<number, R>((x, y) => x + y)(x, y);
}

function squareCPS3<R>(x: number): CPS<number, R> {
    return cps<number, R>(x => x * x)(x);
}

function pythagorasCPS3<R>(x: number, y: number): CPS<number, R> {
    return squareCPS3<R>(x).chain(
        xSquared => squareCPS3<R>(y).chain(
            ySquared => addCPS3<R>(xSquared, ySquared)
        )
    );
}

// TODO: 小四姑娘，体验一下自己的成果！
addCPS3(1, 2).chain(three => squareCPS3(three)).chain(
    nine => addCPS3(2, 2).chain(four => squareCPS3(four)).chain(sixteen => pythagorasCPS3(nine, sixteen))
).run(log)  // ((1+2)^2)^2 + ((2+2)^2^2) == 337

// TODO: 小四姑娘，高阶思考题：
/**
 * chain函数可以无限串联嘛？
 * 如果是，请解释原理。。。
 * 如果不是，会发生什么？怎么解决？
 *
 * 提示1：TCO (Tail-Call Optimization)
 * 提示2：setTimeout
 */
