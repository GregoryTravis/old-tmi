from collections import defaultdict
from lib import *
from lib import _, __
import itertools

def flat1(os):
  return [o for oss in os for o in oss]

gram = {
  'sentence': [ [ 'subject', 'predicate' ] ],
  'subject': [ [ 'noun' ], [ 'adjective', 'noun' ], [ 'noun_phrase' ] ],
  'noun_phrase': [ [ 'noun', 'that', 'verb', 'noun' ] ],
  'predicate': [ [ 'verb', 'noun' ], [ 'verb', 'adjective', 'noun' ], [ 'verb', 'adjective', 'adjective', 'noun' ] ],
}

fmemo = defaultdict(lambda: defaultdict(dict))

#@ctrace(lambda args: [args[1], srcish(args[2][args[3]:args[4]])])
#@trace
def parse(gram, nt, os, s, e):
  if e in fmemo[nt][s]:
    return fmemo[nt][s][e]
  if nt in gram:
    for rhs in gram[nt]:
      #print 'try', nt, '-->', rhs
      if len(rhs) == 1:
        p = parse(gram, rhs[0], os, s, e)
        if p != None:
          fmemo[nt][s][e] = [nt, p]
          return fmemo[nt][s][e]
      elif len(rhs) == 2:
        for x in xrange(s + 1, e):
          assert x != s and x != e
          lp = parse(gram, rhs[0], os, s, x)
          if lp != None:
            rp = parse(gram, rhs[1], os, x, e)
            if rp != None:
              fmemo[nt][s][e] = [nt, lp, rp]
              return fmemo[nt][s][e]
      else:
        assert False
    fmemo[nt][s][e] = None
    return None
  else:
    fmemo[nt][s][e] = [nt, os[s]] if (e - 1 == s and nt == os[s]['type']) else None
    return fmemo[nt][s][e] 

def is_token(o):
  return type(o) == dict and 'src' in o.keys()

def srcish(tree):
  if type(tree) == dict:
    assert is_token(tree)
    return tree['src']
  elif type(tree) == str:
    return tree
  else:
    assert type(tree) == list, tree
    return map(srcish, tree)

gensym_serial = 0
def gensym(prefix):
  global gensym_serial
  sym = prefix + '_' + str(gensym_serial)
  gensym_serial += 1
  return sym

def binarize_production(prod):
  nt, rhs = prod
  if len(rhs) > 2:
    s = gensym('_binarize')
    return [[nt, [rhs[0], s]]] + binarize_production([s, rhs[1:]])
  else:
    return [prod]

def binarize(gram):
  productions = [[nt, rhs] for nt, rhses in gram.iteritems() for rhs in rhses]
  #sp(productions)
  productions = flat1(map(binarize_production, productions))
  #sp(productions)
  gram = group_by(productions, lambda x: x[0])
  #sp(gram)
  gram = {nt: [rhs[1] for rhs in rhses] for nt, rhses in gram.iteritems()}
  #sp(gram)
  return gram

def split_by_0_indent(tokens):
  return listsplitinc(tokens, lambda t: t['column_number'] == 0)

def unbinarize(t):
  #return t
  if type(t) == list and type(t[-1]) == list and type(t[-1][0]) == str and t[-1][0].startswith('_binarize'):
    assert len(t[-1]) == 3, t[-1]
    return unbinarize(t[:-1] + t[-1][1:])
  elif type(t) == list:
    return map(unbinarize, t)
  else:
    return t

p2s_app_exp_rules = [
  [['app', ['exp', _], ['exp', ['app', _, _]]], lambda x, y, z: [x] + match(p2s_app_exp_rules, ['app', y, z])],
  [['app', ['exp', _], ['exp', _]], lambda x, y: [x, y]],
]

p2s_rules = [
  [['top', _], lambda x: p2s(x)],
  [['definition', ['exp', _], _, ['exp', _]], lambda pat, _, body: ['definition', p2s(pat), p2s(body)]],
  [['decls', _, ['semicolon', _], _], lambda df, _, dc: [p2s(df)] + p2s(dc)],
  [['decls', _], lambda df: [p2s(df)]],
  [['app', _, _], lambda x, y: ['app'] + match(p2s_app_exp_rules, ['app', x, y])],
  [['binopexp', _, _, _], lambda x, op, y: ['binopexp', p2s(x), op, p2s(y)]],
  [['exp', ['identifier', _]], lambda x: ['exp', ['identifier', x]]],
  [['exp', ['where', _, ['where_keyword', __], ['lcb', __], _, ['rcb', __]]], lambda e, d: ['where', p2s(e), p2s(d)]],
  [['exp', _], lambda x: ['exp', p2s(x)]],
  #[['exp', _], lambda x: p2s(x)],
  [['let', ['let_keyword', __], ['lcb', __], _, ['rcb', __], ['in_keyword', __], _], lambda ds, e: ['let', p2s(ds), p2s(e)]],
  [['case', ['case_keyword', __], _, ['of_keyword', __], ['lcb', __], _, ['rcb', __]], lambda e, d: ['case', p2s(e), p2s(d)]],
  [['case_clauses', _, ['semicolon', __], _], lambda df, dc: p2s(df) + [p2s(dc)]],
  [['case_clauses', _], lambda df: [p2s(df)]],
  [['case_clause', ['exp', _], __, ['exp', _]], lambda pat, body: ['case_clause', p2s(pat), p2s(body)]],
  [['identifier', _], lambda x: ['identifier', x]],
  [_, lambda x: ['???', x]],
]

#@trace
def p2s(t):
  #return t
  return match(p2s_rules, t)
  tt = match(p2s_rules, t)
  if tt != None:
    return p2s(tt)
  else:
    if type(t) == list:
      return map(p2s, t)
    else:
      return t
  return t
  if type(t) != list:
    return t
  elif t[0] == 'top':
    assert len(t) == 2
    return p2s(t[1])
  elif t[0] == 'decls' and len(t) == 4:
    return [p2s(t[1])] + p2s(t[3])
  elif t[0] == 'decls' and len(t) == 2:
    return [p2s(t[1])]
  elif t[0] == 'definition':
    assert len(t) == 4
    assert t[1][0] == 'defpat'
    assert t[3][0] == 'exp'
    return ['definition', t[1][1], p2s(t[3])]
  elif t[0] == 'let':
    assert len(t) == 7
    assert t[3][0] == 'decls'
    assert t[6][0] == 'exp'
    return ['let', p2s(t[3]), p2s(t[6])]
  elif t[0] == 'where':
    assert len(t) == 6
    assert t[1][0] == 'exp'
    assert t[4][0] == 'decls'
    return ['where', p2s(t[1][1]), p2s(t[4])]
  elif t[0] == 'exp':
    if len(t) == 2:
      return p2s(t[1])
    elif t[1]['type'] == 'lparen':
      return p2s(t[2])
    else:
      assert False, t
  else:
    return t

def parse_top(gram, nt, tokens):
  gram = binarize(gram)
  #tlds = [parse(gram, nt, tokens, 0, len(tokens)) for tokens in split_by_0_indent(tokens)]
  return p2s(unbinarize(parse(gram, nt, tokens, 0, len(tokens))))

tokens = 'noun that verb noun verb adjective noun'.split(' ')
nt = 'sentence'
#map(sp, all_partitions(tokens, 3))
#sp(tokens)
#sp(parse(nt, tokens))

"""
start = decls $;
decls = definition ';' decls | definition;
definition = defpat: {identifier}+ '=' body: exp;
exp = let | binopapp | app | identifier | integer;
let = 'let' '{' decls: decls '}' 'in' body: exp;
binopapp = left: identifier op: operator right: exp;
app = identifier {identifier}+;

identifier = /[a-zA-Z]+/;
_operator = /[+\-_!@$%^&*?]+/;
integer = /[0-9]+/;
operator = /[+]+/;
"""

gram = {
  'top': [['decls']],
  'app': [['exp', 'exp'], ['app', 'exp']],
  'let': [['let_keyword', 'lcb', 'decls', 'rcb', 'in_keyword', 'exp']],
  'where': [['exp', 'where_keyword', 'lcb', 'decls', 'rcb']],
  #'defpat': [['identifier'], ['app']],
  'definition': [['exp', 'equals', 'exp']],
  'decls': [['definition', 'semicolon', 'decls'], ['definition']],
  'binopexp': [['exp', 'operator', 'exp']],
  'exp': [['lparen', 'exp', 'rparen'], ['binopexp'], ['let'], ['where'], ['case'], ['app'], ['identifier']],
  'case': [['case_keyword', 'exp', 'of_keyword', 'lcb', 'case_clauses', 'rcb']],
  'case_clauses': [['case_clauses', 'semicolon', 'case_clause'], ['case_clause']],
  'case_clause': [['exp', 'rdbl_arrow', 'exp']],
}
#nt = 'top'
