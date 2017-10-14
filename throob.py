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

# Return nt, line, column of first token, num tokens
def tokpos(args):
  return args[1]  + '-' + str(args[3]) + '-' + str(args[4])
  #return args[1] + '-' + str(args[2][0]['line_number']) + '-' + str(args[2][0]['column_number']) + '-' + str(len(args[2]))

#@ctrace(lambda args: [args[1], srcish(args[2][args[3]:args[4]])])
#@trace
@cmemoize(tokpos)
#@memoize
def parse(gram, nt, os, s, e):
  if nt in gram:
    for rhs in gram[nt]:
      #print 'try', nt, '-->', rhs
      if len(rhs) == 1:
        p = parse(gram, rhs[0], os, s, e)
        if p != None:
          return [nt, p]
      elif len(rhs) == 2:
        for x in xrange(s + 1, e):
          assert x != s and x != e
          lp = parse(gram, rhs[0], os, s, x)
          if lp != None:
            rp = parse(gram, rhs[1], os, x, e)
            if rp != None:
              return [nt, [lp, rp]]
      else:
        assert False
    return None
  else:
    #print 'BASE', nt, os, os[0]['type'], len(os), (len(os) == 1 and nt == os[0]['type']), len(os) == 1, nt == os[0]['type']
    #t, s, e = os
    return os[s] if (e - 1 == s and nt == os[s]['type']) else None
    #return os[0] if (len(os) == 1 and nt == os[0]['type']) else None

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

def parse_top(gram, nt, tokens):
  gram = binarize(gram)
  return parse(gram, nt, tokens, 0, len(tokens))

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
  'top': [['let']],
  'app': [['identifier', 'identifier'], ['app', 'identifier']],
  'let': [['let_keyword', 'lcb', 'decls', 'rcb', 'in_keyword', 'exp']],
  'defpat': [['identifier'], ['app']],
  'definition': [['defpat', 'equals', 'exp']],
  'decls': [['definition', 'semicolon', 'decls'], ['definition']],
  'exp': [['let'], ['app'], ['identifier']],
}
#nt = 'top'
