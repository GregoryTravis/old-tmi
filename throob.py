from collections import defaultdict
from lib import *
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
    fmemo[nt][s][e] = os[s] if (e - 1 == s and nt == os[s]['type']) else None
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

def p2s(t):
  assert type(t) == list

def parse_top(gram, nt, tokens):
  gram = binarize(gram)
  #tlds = [parse(gram, nt, tokens, 0, len(tokens)) for tokens in split_by_0_indent(tokens)]
  return unbinarize(parse(gram, nt, tokens, 0, len(tokens)))

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
  'app': [['identifier', 'identifier'], ['app', 'identifier']],
  'let': [['let_keyword', 'lcb', 'decls', 'rcb', 'in_keyword', 'exp']],
  'defpat': [['identifier'], ['app']],
  'definition': [['defpat', 'equals', 'exp']],
  'decls': [['definition', 'semicolon', 'decls'], ['definition']],
  'exp': [['let'], ['app'], ['identifier']],
}
#nt = 'top'
