# coding: utf-8

'''
Pretty syntax for Pedal mini-language.

Pedal is preserved for possible future use.
'''

import parsley

_pedal_syntax = r'''
id = <(letter|'_'|'$')(letter|digit|'_')*>
identifier = id:identifier -> Identifier(identifier)
spc = ' '
newline = '\n'
tab = '\t'
s = (spc | newline | tab)+
in_s = (spc | tab)+
opt_s = ws
opt_in_s = (spc | tab)*
hexdigit = :x ?(x in '0123456789abcdefABCDEF') -> x
escapedChar = '\\' (('"' -> '"')    |('\\' -> '\\')
                   |('/' -> '/')    |('b' -> '\b')
                   |('f' -> '\f')   |('n' -> '\n')
                   |('r' -> '\r')   |('t' -> '\t')
                   |('\'' -> '\'')  | escapedUnicode)
escapedUnicode = 'u' <hexdigit{4}>:hs -> unichr(int(hs, 16))

# Useless
does_not_exist_yet = ?(False)

# All sorts of expressions
expr = expr_0p
expr_100 = atomic_expr
expr_100p = expr_100
expr_10 = func_call_expr
expr_10p = expr_10 | expr_100p
expr_9 = exp_expr
expr_9p = expr_9 | expr_10p
expr_8 = positive_expr | negative_expr
expr_8p = expr_8 | expr_9p
expr_7 = mul_expr | div_expr | mod_expr
expr_7p = expr_7 | expr_8p
expr_6 = plus_expr | minus_expr
expr_6p = expr_6 | expr_7p
expr_5 = does_not_exist_yet
expr_5p = expr_5 | expr_6p
expr_4 = does_not_exist_yet
expr_4p = expr_4 | expr_5p
expr_3 = does_not_exist_yet
expr_3p = expr_3 | expr_4p
expr_2 = eq_expr | lt_expr | lte_expr | gt_expr | gte_expr
expr_2p = expr_2 | expr_3p
expr_1 = ifelse_expr
expr_1p = expr_1 | expr_2p
expr_0 = does_not_exist_yet
expr_0p = expr_0 | expr_1p

# TODO: For god's sake!
sgl_quot_str = "'" (escapedChar | ~"'" anything)*:c "'" -> ''.join(c)
dbl_quot_str = '"' (escapedChar | ~'"' anything)*:c '"' -> ''.join(c)
str = (sgl_quot_str | dbl_quot_str)
str_expr = (sgl_quot_str | dbl_quot_str):s -> String(s)
float = <('+'|'-')? (digit+ '.' digit* | '.' digit+):x>
integer = <('+'|'-')? digit+>
num = integer | float
number_expr = num:num -> Num(num)
bool_expr = ('true' | 'false'):b -> Bool(b)
null_expr = 'null':n -> Null(n)
enclosed_expr = '(' opt_s expr:expr opt_s ')' -> expr
atomic_expr = enclosed_expr | number_expr | str_expr | identifier
arguments = (expr?:arg0) opt_s (',' opt_s expr:expr opt_s -> expr)*:args -> ([arg0] if arg0 is not None else []) + args
func_call_expr = identifier:func_name opt_s '(' opt_s arguments:arguments opt_s ')' -> FuncCall(func_name, arguments)
exp_expr = expr_10p:base opt_s '**' opt_s expr_10p:exp -> Pow(base, exp)
positive_expr = '+' opt_s expr_9p:operand -> Positive(operand)
negative_expr = '-' opt_s expr_9p:operand -> Negative(operand)
mul_expr = expr_8p:left opt_s '*' opt_s expr_8p:right -> Mul(left, right)
div_expr = expr_8p:left opt_s '/' opt_s expr_8p:right -> Div(left, right)
mod_expr = expr_8p:left opt_s '%' opt_s expr_8p:right -> Mod(left, right)
plus_expr = expr_7p:left opt_s '+' opt_s expr_7p:right -> Plus(left, right)
minus_expr = expr_7p:left opt_s '-' opt_s expr_7p:right -> Minus(left, right)
eq_expr = expr_3p:left opt_s '==' opt_s expr_3p:right -> Equal(left, right)
lt_expr = expr_3p:left opt_s '<' opt_s expr_3p:right -> LessThan(left, right)
lte_expr = expr_3p:left opt_s '<=' opt_s expr_3p:right -> LessThanEqual(left, right)
gt_expr = expr_3p:left opt_s '>' opt_s expr_3p:right -> GreaterThan(left, right)
gte_expr = expr_3p:left opt_s '>=' opt_s expr_3p:right -> GreaterThanEqual(left, right)
ifelse_expr = 'if' s expr_2p:pred s 'then' expr_2p:yes_expr 'else' s expr_2p:no_expr -> IfElseExpr(pred, yes_expr, no_expr)


stmt = (assign_stmt | if_stmt | (expr:expr -> ExprStmt(expr))):stmt -> stmt
br_stmts = '{' opt_s (stmt?:stmt0) (newline (opt_s stmt:stmt opt_in_s -> stmt):stmt -> stmt)*:stmts opt_s '}' -> ([stmt0] if stmt0 is not None else []) + stmts
assign_stmt = 'let' s (identifier:varname) opt_s '=' opt_s (expr:val) -> Assign(varname, val)
if = 'if' s expr:pred opt_s br_stmts:stmts -> If(pred, stmts)
elif = 'elif' s expr:pred opt_s br_stmts:stmts -> Elif(pred, stmts)
else = 'else' opt_s br_stmts:stmts -> Else(stmts)
if_stmt = if:if_ opt_s elif*:elifs_ opt_s else?:else_ -> IfStmt(if_, elifs_, else_)

section_name = (id | str):name -> name

var_decl = identifier:varname (opt_s ':' opt_s identifier:vartype -> vartype)?:vartype -> Var(varname, vartype)
br_var_decls = '{' opt_s (var_decl?:var0) (newline (opt_s var_decl:var opt_in_s -> var):var -> var)*:vars opt_s '}' -> ([var0] if var0 is not None else []) + vars
title = 'title' s str:s -> s

input = 'input' opt_s br_var_decls:vars -> vars
tweaks = 'tweaks' opt_s br_var_decls:vars -> vars

section = section_name:name ?(name != 'triggers') opt_s br_stmts:stmts -> Section(name, stmts)

trigger_interval = '@ every' s integer:value s ('minute' 's'? -> 'minute' | 'second' 's'? -> 'second'):unit -> Schedule(value, unit)
trigger = 'trigger' s section_name:name (s trigger_interval)?:sched opt_s br_stmts:stmts -> Trigger(name, sched, stmts)

pedal = opt_s title:title opt_s input?:input opt_s tweaks?:tweaks opt_s (section:section opt_s -> section)*:sections trigger:trigger opt_s -> Pedal(title, input, tweaks, sections, trigger)
'''

__all__ = [
    'parse',
    'Node',
    'Pedal',
    'Var',
    'Section',
    'Trigger',
    'Schedule',
    'Identifier',
    'Num',
    'String',
    'Expr',
    'UnaryOp',
    'Positive',
    'Negative',
    'BinaryOp',
    'Pow',
    'Mul',
    'Div',
    'Mod',
    'Plus',
    'Minus',
    'Compare',
    'Equal',
    'LessThan',
    'LessThanEqual',
    'GreaterThan',
    'GreaterThanEqual',
    'IfElseExpr',
    'Assign',
    'If',
    'Elif',
    'Else',
    'IfStmt',
]


class _UnexpectedTokenException(Exception):
    pass


class Node(object):
    _type = None

    @property
    def type(self):
        return self._type

    def __init__(self):
        pass

    @staticmethod
    def _ubiquitous_dictify(val):
        if isinstance(val, list):
            return [Node._ubiquitous_dictify(elem) for elem in val]
        if isinstance(val, Node):
            return val.dictify()
        else:
            return val

    def dictify(self):
        return dict({
            k: Node._ubiquitous_dictify(getattr(self, k))
            for k in self._fields
        }, type=self.type)


class Pedal(Node):
    _type = 'pedal'
    _fields = ['title', 'input', 'tweaks', 'sections', 'trigger']

    def __init__(self, title, input, tweaks, sections, trigger):
        self.title = title
        self.input = input
        self.tweaks = tweaks
        self.sections = sections
        self.trigger = trigger


class Input(Node):
    _type = 'input'
    _fields = ['vars']

    def __init__(self, vars):
        self.vars = vars


class Tweaks(Node):
    _type = 'tweaks'
    _fields = ['vars']

    def __init__(self, vars):
        self.vars = vars


class Var(Node):
    _type = 'var'
    _fields = ['varname', 'vartype']

    def __init__(self, varname, vartype):
        self.varname = varname
        self.vartype = vartype


class Section(Node):
    _type = 'section'
    _fields = ['name', 'stmts']

    def __init__(self, name, stmts):
        self.name = name
        self.stmts = stmts


class Trigger(Node):
    _type = 'trigger'
    _fields = ['name', 'sched', 'stmts']

    def __init__(self, name, sched, stmts):
        self.name = name
        self.sched = sched
        self.stmts = stmts

class Schedule(Node):
    _type = 'schedule'
    _fields = ['value', 'unit']

    def __init__(self, value, unit):
        self.value = value
        self.unit = unit


class Expr(Node):
    _type = 'expr'


class ExprStmt(Expr):
    _type = 'expr_stmt'
    _fields = ['value']

    def __init__(self, value):
        self.value = value


class Atom(Expr):
    _type = 'atom'


class Identifier(Atom):
    _type = 'identifier'
    _fields = ['identifier']

    def __init__(self, identifier):
        self.identifier = identifier


class Num(Atom):
    _type = 'num'
    _fields = ['val']

    def __init__(self, num):
        import ipdb; ipdb.set_trace()

        if '.' in num:
            self.val = float(num)
        else:
            self.val = int(num)


class String(Atom):
    _type = 'string'
    _fields = ['val']

    def __init__(self, s):
        self.val = s


class Bool(Atom):
    _type = 'bool'
    _fields = ['val']

    def __init__(self, b):
        if b == 'true':
            self.val = True
        else:
            self.val = False


class Null(Atom):
    _type = 'null'
    _fields = ['val']

    def __init__(self, n):
        if n == 'null':
            self.val = None
        else:
            raise UnexpectedTokenException("%s is not a valid nullary value" % n)


class UnaryOp(Expr):
    _type = 'unary_op'
    _op = None
    _fields = ['operand']

    def __init__(self, operand):
        self.operand = operand

    @property
    def op(self):
        return self._op


class Positive(UnaryOp):
    _type = 'positive'
    _op = '+'


class Negative(UnaryOp):
    _type = 'negative'
    _op = '-'


class BinaryOp(Expr):
    _type = 'binary_op'
    _op = None
    _fields = ['left', 'right']

    def __init__(self, left, right):
        self.left = left
        self.right = right

    @property
    def op(self):
        return self._op


class Pow(BinaryOp):
    _type = 'pow'
    _op = '**'


class Mul(BinaryOp):
    _type = 'mul'
    _op = '*'


class Div(BinaryOp):
    _type = 'div'
    _op = '/'


class Mod(BinaryOp):
    _type = 'mod'
    _op = '%'


class Plus(BinaryOp):
    _type = 'plus'
    _op = '+'


class Minus(BinaryOp):
    _type = 'minus'
    _op = '-'


class Compare(BinaryOp):
    _type = 'compare'


class Equal(Compare):
    _type = 'equal'
    _op = '=='


class LessThan(Compare):
    _type = 'less_than'
    _op = '<'


class LessThanEqual(Compare):
    _type = 'less_than_equal'
    _op = '<='


class GreaterThan(Compare):
    _type = 'greater_than'
    _op = '>'


class GreaterThanEqual(Compare):
    _type = 'greater_than_equal'
    _op = '>='


class IfElseExpr(Expr):
    _type = 'if_else_expr'
    _fields = ['pred', 'yes_expr', 'no_expr']

    def __init__(self, pred, yes_expr, no_expr):
        self.pred = pred
        self.yes_expr = yes_expr
        self.no_expr = no_expr


class Stmt(Node):
    _type = 'stmt'


class Assign(Stmt):
    _type = 'assign'
    _fields = ['varname', 'val']

    def __init__(self, varname, val):
        self.varname = varname
        self.val = val


class If(Node):
    _type = 'if'
    _fields = ['pred', 'stmts']

    def __init__(self, pred, stmts):
        self.pred = pred
        self.stmts = stmts


class Elif(Node):
    _type = 'elif'
    _fields = ['pred', 'stmts']

    def __init__(self, pred, stmts):
        self.pred = pred
        self.stmts = stmts


class Else(Node):
    _type = 'else'
    _fields = ['stmts']

    def __init__(self, stmts):
        self.stmts = stmts


class IfStmt(Stmt):
    _type = 'if_stmt'
    _fields = ['if_', 'elifs_', 'else_']

    def __init__(self, if_, elifs_=None, else_=None):
        self.if_ = if_
        self.elifs_ = elifs_
        self.else_ = else_


class FuncCall(Expr, Stmt):
    _type = 'func_call'
    _fields = ['func_name', 'arguments']

    def __init__(self, func_name, arguments):
        self.func_name = func_name
        self.arguments = arguments


def parse(code):
    '''Parse pedal source code into pedal AST
    '''
    pedal = parsley.makeGrammar(_pedal_syntax, globals())
    ret = pedal(code).pedal()
    return ret
