import re
import parsley
from cStringIO import StringIO

_EXCL = [
    re.compile(rgx)
    for rgx in [
            r'yyyy-MM-dd',
    ]
]

_REPLACEMENT = "'?'"

keywords = [
    "and",
    "as",
    "asc",
    "between",
    "case",
    "collate nocase",
    "cross join",
    "desc",
    "else",
    "end",
    "from",
    "group by",
    "having",
    "in",
    "inner join",
    "is",
    "join",
    "limit",
    "like",
    "on",
    "or",
    "order by",
    "select",
    "then",
    "union",
    "when",
    "where",
    "with"
]

sql_grammar = parsley.makeGrammar(r'''
w = (' ' | '\t' | '\n')+
str = '\'' (escapedChar | ~'\'' anything)*:c '\'' -> ''.join(c)

hexdigit = :x ?(x in '0123456789abcdefABCDEF') -> x

escapedChar = '\\' (('"' -> '"')    |('\\' -> '\\')
                   |('/' -> '/')    |('b' -> '\b')
                   |('f' -> '\f')   |('n' -> '\n')
                   |('r' -> '\r')   |('t' -> '\t')
                   |('\'' -> '\'')  | escapedUnicode)

escapedUnicode = 'u' <hexdigit{4}>:hs -> unichr(int(hs, 16))

keyword = ( 'and' | 'as' | 'asc'
          | 'between' | 'case' | 'collate nocase'
          | 'cross join' | 'desc' | 'else'
          | 'end' | 'from' | 'group by' | 'having'
          | 'in' | 'inner join' | 'is' | 'join' | 'limit'
          | 'like' | 'on' | 'or' | 'order by' | 'select'
          | 'then' | 'union' | 'when' | 'where' | 'with'):k -> ('keyword', k)

operator = ( '||' | '*' | '/' | '+' | '-' | '<>' | '>'
           | '<' | '>=' | '<=' | '=' | '==' | '!='
           | 'in' | 'is' | 'like' | 'or' | 'and'):op -> ('operator', op)

digit = :x ?(x in '0123456789') -> x
digits = <digit*>
digit1_9 = :x ?(x in '123456789') -> x

intPart = (digit1_9:first digits:rest -> first + rest) | digit

floatPart :sign :ds = <('.' digits exponent?) | exponent>:tail
                     -> float(sign + ds + tail)

exponent = ('e' | 'E') ('+' | '-')? digits

number = spaces ('-' | -> ''):sign (intPart:ds (floatPart(sign ds)
                                               | -> int(sign + ds)))

identifier = letter:l0 (letterOrDigit)*:ls -> ('identifier', ''.join([l0] + ls))
quoted_expr = '(' ws sql:sql ws ')' -> ('quoted_expr', sql)
expr = (str | number | quoted_expr):e -> ('expr', e)

funcCall = identifier '(' expr*:exprs ')' -> ('funcCall', identifier, exprs)

garbage = (anything:x ?(x not in ' \n\t()') -> x)+:xs -> ('garbage', ''.join(xs))

part = (keyword:k | operator:o | funcCall:f | identifier | garbage:g | expr:e)
part0 = keyword

sql = ws part:p0 (ws part)*:ps ws -> [p0] + ps
''', {})


print sql_grammar('''select abc from gg where abc=123 and (def = 'skdljfl"\'"') ''').sql()



def dequote(s):
    sio = StringIO()
    in_str = False
    pending_sio = None

    for c in s:
        if not in_str:
            if c == "'":
                in_str = True
                pending_sio = StringIO()
            else:
                sio.write(c)
            continue
        if in_str:
            if c == "'":
                for rgx in _EXCL:
                    pending_value = pending_sio.getvalue()
                    if rgx.match(pending_value):
                        sio.write("'" + pending_value + "'")
                        break
                else:
                    sio.write(_REPLACEMENT)
                pending_sio.close()
                in_str = False
            else:
                pending_sio.write(c)

    return sio.getvalue()

q = "SQLSTT=delete ONLINE_DY_SEND where uuid in ('66985637') \nSQLSTT=update weix_temp_send_result set status='success',updatetime='2018-07-31 13:40:27' where msgid='392978717569515522' and openid='oCJPBjtxdO89ymowjN9ytSQDy7jk'\nSQLSTT=select parammodel0_.keyValue as col_0_0_ from public_param_manager parammodel0_ where parammodel0_.keyName='Currency'\nSQLSTT=delete ONLINE_DY_SEND where uuid in ('66986097','66986102','66986101','66986100','66986098')\nSQLSTT=delete from Batch_File_History where to_date(substr(savetime,1,10),'yyyy-MM-dd') < to_date('2018-7-24','yyyy-MM-dd')"
