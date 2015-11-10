# coding: utf-8

'''
Transcript Pedal AST into Python AST.
'''

import ast
from syntax import *


__all__ = ['transcript']


class _Transcript(object):
    _unary_op_mapping = {
        'positive': ast.UAdd,
        'negative': ast.USub
    }

    _binary_op_mapping = {
        'pow': ast.Pow,
        'mul': ast.Mult,
        'div': ast.Div,
        'mod': ast.Mod,
        'plus': ast.Add,
        'minus': ast.Sub,
        # Compare ops
        'equal': ast.Eq,
        'less_than': ast.Lt,
        'less_than_equal': ast.LtE,
        'greater_than': ast.Gt,
        'greater_than_equal': ast.GtE,
    }

    @classmethod
    def _trans(cls, node, *args, **kwargs):
        if isinstance(node, UnaryOp):
            new_node = cls._trans_unaryop(node, node.operand)
        elif isinstance(node, BinaryOp):
            new_node = cls._trans_binop(node, node.left, node.right)
        elif isinstance(node, Section):
            new_node = cls._trans_section(node)
        else:
            new_node = getattr(cls, '_trans_' + node.type)(node, *args, **kwargs)
        return new_node

    @classmethod
    def _trans_section(cls, node):
        name = node.name
        args = ast.arguments(args=[], defaults=[])
        body = [cls._trans(stmt) for stmt in node.stmts]
        decorator_list = []
        return ast.FunctionDef(name, args, body, decorator_list)

    @classmethod
    def _trans_identifier(cls, node, ctx='load'):
        if ctx == 'load':
            ctx = ast.Load()
        elif ctx == 'store':
            ctx = ast.Store()
        elif ctx == 'del':
            ctx = ast.Del()

        return ast.Name(id=node.identifier, ctx=ctx)

    @classmethod
    def _trans_unaryop(cls, node, operand):
        op_obj = cls._unary_op_mapping[node.type]()
        return ast.UnaryOp(op_obj, cls_trans(operand))

    @classmethod
    def _trans_binop(cls, node, left, right):
        op_obj = cls._binary_op_mapping[node.type]()
        if isinstance(node, Compare):
            return ast.Compare(cls._trans(left), [op_obj], [cls._trans(right)])
        return ast.BinOp(cls._trans(left), op_obj, cls._trans(right))

    @classmethod
    def _trans_num(cls, node):
        return ast.Num(node.val)

    @classmethod
    def _trans_string(cls, node):
        return ast.Str(node.val)

    @classmethod
    def _trans_pow(cls, node):
        return ast.Pow(cls._trans(node.left), cls._trans(node.right))

    @classmethod
    def _trans_mul(cls, node):
        return ast.Mult(cls._trans)

    @classmethod
    def _trans_if_else_expr(cls, node):
        pred, yes_expr, no_expr = node.pred, node.yes_expr, node.no_expr
        return ast.IfExpr(cls._trans(pred), cls._trans(yes_expr), cls._trans(no_expr))

    @classmethod
    def _trans_func_call(cls, node):
        args = [cls._trans(arg) for arg in node.arguments]
        return ast.Call(
            func=cls._trans_identifier(node.func_name),
            args=args,
            keywords=[]
        )

    @classmethod
    def _trans_expr_stmt(cls, node):
        value = node.value
        return ast.Expr(value=cls._trans(node.value))

    @classmethod
    def _trans_assign(cls, node):
        varname = cls._trans_identifier(node.varname, ctx='store')
        assign_targets = [varname]
        assign_value = cls._trans(node.val)
        return ast.Assign(assign_targets, assign_value)

    @classmethod
    def _trans_if_stmt(cls, node):
        def _nested_if(orelse, node):
            test = node.pred
            body = node.stmts
            nested_orelse = []
            orelse.append(ast.If(test, body, nested_orelse))
            return nested_orelse

        if_ = node.if_
        test = cls._trans(if_.pred)
        body = [cls._trans(yes_stmt) for yes_stmt in if_.stmts]
        orig_orelse = orelse = []
        for elif_ in node.elifs_:
            orelse = _nested_if(orelse, elif_)

        else_ = node.else_
        orelse.append([cls._trans(stmt) for stmt in else_.stmts])

        return ast.If(test, body, orig_orelse)

def transcript(pedal_source):
    pedal_ast = parse(pedal_source)
    import ipdb; ipdb.set_trace()

    py_ast = _Transcript._trans(pedal_ast.sections[1])
    mod = ast.Module(body=[py_ast])
    mod = ast.fix_missing_locations(mod)
    trans_to = {}
    exec(compile(mod, '<string>', 'exec'), globals(), trans_to)
    import ipdb; ipdb.set_trace()
    return trans_to
