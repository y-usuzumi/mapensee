# -*- coding: utf-8
from crossutil.dotdictify import Dotdictify
from crossutil.dictionary import dictdeepmerge

__all__ = [
    'Ignored',
    'DisplayFormatter',
    'format_to_field',
    'format_to_display',
    'bulk_field_format',
    'bulk_display_format'
]


def dictdeepmerge(a, b):
    # TODO: implement this
    pass


class Dotdictify(dict):
    # TODO: implement this
    pass


class _Ignored(object):
    pass


Ignored = _Ignored()


class DisplayFormatterMeta(type):
    def __new__(t, name, bases, obj):
        formatters = []
        for v in obj.itervalues():
            if callable(v) and hasattr(v, '_formatter_func'):
                formatters.append(v)

        obj['_formatters'] = formatters
        kls = super(DisplayFormatterMeta, t).__new__(t, name, bases, obj)
        return kls


class DisplayFormatter(object):
    '''
    This thing is a generalized dict formatter that exports specified fields,
    and also optionally a `_display` field which contains further info that
    corresponds to the field in the original dict.

    e.g.
    class MyDisplayFormatter(DisplayFormatter):
        @bulk_field_format
        def a(self, record):
            return {"A1": record["a1"], "A2": record["a2"]}

        @bulk_field_format
        def b(self, record):
            return {"B1": record["b1"], "B2": record["b2"]}

        @bulk_display_format
        def a_display(self, record):
            return {"A1": "One", "A2": "Two"}

        @bulk_display_format
        def b_display(self, record):
            return {"B1": "Un", "B2": "Deux"}

        @format_to_field("c.d")
        def c_d(self, record):
            return 3

        @format_to_field("c.e")
        def c_e(self, record):
            return 4

        @format_to_display("C.D")
        def c_d_display(self, record):
            return "Three"

        @format_to_display("C.E")
        def c_e_display(self, record):
            return "Quatre"

    rec = {"a1": 1, "a2": 2, "b1": 1, "b2": 2}
    f = MyDisplayFormatter()
    f.format(rec)

    The the output is something like:
    {
        "A1": 1,
        "A2": 2,
        "B1": 1,
        "B2": 2,
        "c": {
            "d": 3
            "e": 4,
        },
        "_display": {
            "A1": "One",
            "A2": "Two",
            "B1": "Un",
            "B2": "Deux",
            "C": {
                "D": "Three"
                "E": "Quatre",
            },
        }
    }
    '''
    __metaclass__ = DisplayFormatterMeta

    def format(self, record):
        ret = Dotdictify()
        for formatter in self._formatters:
            result = formatter(self, record)
            if result == Ignored:
                continue
            if formatter._formatter_type == 'field':
                ret[formatter._formatter_key] = result
            elif formatter._formatter_type == 'display':
                ret['_display.%s' % formatter._formatter_key] = result
            elif formatter._formatter_type == 'bulk_field':
                if result is None:
                    continue
                ret = dictdeepmerge(ret, result)
            elif formatter._formatter_type == 'bulk_display':
                if result is None:
                    continue
                ret["_display"] = dictdeepmerge(ret.get('_display', {}), result)
        return ret


def format_to_field(key):
    def wrapper(func):
        func._formatter_func = True
        func._formatter_type = 'field'
        func._formatter_key = key
        return func
    return wrapper


def format_to_display(key):
    def wrapper(func):
        func._formatter_func = True
        func._formatter_type = 'display'
        func._formatter_key = key
        return func
    return wrapper


def bulk_field_format(func):
    func._formatter_func = True
    func._formatter_type = 'bulk_field'
    return func


def bulk_display_format(func):
    func._formatter_func = True
    func._formatter_type = 'bulk_display'
    return func

