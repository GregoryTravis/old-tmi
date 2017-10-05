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

def all_partitions(os, n):
  if n == 1:
    return [[os]]
  else:
    return [[os[0:x]] + tail for x in xrange(0, len(os)+1) for tail in all_partitions(os[x:], n-1)]
tokens = 'a b c adjective noun'.split(' ')
assert all(map(lambda p: p == tokens, map(flat1, all_partitions(tokens, 3))))
tokens = None

@trace
def subparse(nts, oses):
  assert len(nts) == len(oses)
  subparses = [parse(nt, os) for nt, os in zip(nts, oses)]
  return subparses if not any([p == None for p in subparses]) else None

@trace
def parse(nt, os):
  if nt in gram:
    for rhs in gram[nt]:
      for partition in all_partitions(os, len(rhs)):
        sub = subparse(rhs, partition) 
        if sub != None:
          return [nt, sub]
    return None
  else:
    return os[0] if len(os) == 1 and nt == os[0] else None

tokens = 'noun that verb noun verb adjective noun'.split(' ')
nt = 'sentence'
#map(sp, all_partitions(tokens, 3))
sp(tokens)
sp(parse(nt, tokens))
