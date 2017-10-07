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

# Does not generate empty partitions
def all_partitions(os, n):
  if n == 1:
    return [[os]]
  else:
    return [[os[0:x]] + tail for x in xrange(1, len(os)+1-1) for tail in all_partitions(os[x:], n-1)]
    #return [[os[0:x]] + tail for x in xrange(0, len(os)+1) for tail in all_partitions(os[x:], n-1)]
tokens = 'a b c adjective noun'.split(' ')
assert all(map(lambda p: p == tokens, map(flat1, all_partitions(tokens, 3))))
tokens = None

def seqid(tokens):
  return [tokens[0]['line_number'], tokens[0]['column_number'], len(tokens)]

# Return nt, line, column of first token, num tokens
def tokpos(args):
  return ['tokpos', args[0], seqid(args[1])]

# Like tokpos, but second arg is a list of sequences
def tokposes(args):
  return ['tokpos', args[0], map(seqid, args[1])]

@cmemoize(tokposes)
#@ctrace(lambda args: [args[0], srcish(args[1])])
def subparse(nts, oses):
  assert len(nts) == len(oses)
  subparses = [parse(nt, os) for nt, os in zip(nts, oses)]
  return subparses if not any([p == None for p in subparses]) else None

#@ctrace(lambda args: [args[0], srcish(args[1])])
#@trace
@cmemoize(tokpos)
#@memoize
def parse(nt, os):
  if nt in gram:
    for rhs in gram[nt]:
      for partition in all_partitions(os, len(rhs)):
        sub = subparse(rhs, partition) 
        if sub != None:
          return [nt, sub]
    return None
  else:
    #print 'BASE', nt, os, os[0]['type'], len(os), (len(os) == 1 and nt == os[0]['type']), len(os) == 1, nt == os[0]['type']
    return os[0] if (len(os) == 1 and nt == os[0]['type']) else None

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
  'let': [['let_keyword', 'lcb', 'decls', 'rcb', 'in_keyword', 'identifier']],
  'defpat': [['app']],
  'definition': [['defpat', 'equals', 'exp']],
  'decls': [['definition', 'semicolon', 'decls'], ['definition']],
  'exp': [['let'], ['app']],
}
#nt = 'top'
