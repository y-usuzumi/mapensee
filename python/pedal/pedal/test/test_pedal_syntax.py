# coding: utf-8
from pedal import (
    parse,
    transcript,
    _pedal
)
import unittest


class PedalParseTest(TestCase):
    _sample_code = r'''
    title "No trans"

    input {
        historical_samplings: int
        recent_n_min_no_trans_func: func
    }

    tweaks {
        sample_days: int
        daily_sample_points: int
        check_interval: int
        hs_u: int
        hs_l: int
        n_u: int
        n_l: int
    }

    "Historical samplings" {

    }

    trigger "Alert trigger condition" {
        if historical_samplings > hs_u {
            N = n_u
        } elif historical_samplings > hs_l {
            N = n_l - historical_samplings
        } else
    }
    '''

    def test_parse(self):
        ast = parse(self._sample_code)
        assert len(ast) == 3

        # Test hello section
        assert ast[0].name == 'hello'
        stmts = ast[0].stmts
        num_stmt = stmts[0]
        assert num_stmt.varname.identifier == 'num'
        assert num_stmt.val.val == 123
        str_stmt = stmts[1]
        assert str_stmt.varname.identifier == 'str'
        assert str_stmt.val.val == 'Hello"World!'
        if_stmt = stmts[2]
        assert isinstance(if_stmt.if_.expr, _pedal.LessThan)
        assert if_stmt.if_.expr.left.identifier == 'num'
        assert if_stmt.if_.expr.right.val == 250
        if_stmt_if_inner = if_stmt.if_.stmts
        # Fast forward
        assert if_stmt_if_inner[1].if_.stmts[0].val.val == "Yes really hell"
        assert if_stmt_if_inner[1].else_.stmts[0].val.val == "Not really"
        if_stmt_elif_inner = if_stmt.elif_[0].stmts
        assert if_stmt_elif_inner[0].val.val == 1
        if_stmt_else_inner = if_stmt.else_.stmts
        assert if_stmt_else_inner[0].val.val == 2

        # Test world section
        # Fast forward
        assert ast[1].name == 'world'
        assert ast[1].stmts[0].func_name.identifier == 'print'
        assert list(map(lambda x: getattr(x, 'val'), ast[1].stmts[0].arguments)) == ['Damn', 'hell']

    def test_transcript(self):
        transcript(self._sample_code)
