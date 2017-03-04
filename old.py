from tmi import *
from web import call

def foo(a, b):
  db = File('old.dat')
  player = Deref(db, 'player')
  card = Deref(db, 'card')
  game = Deref(db, 'game')
  hand = Deref(db, 'hand')
  return ss(Join(game, Join(player, Join(card, hand))))

def main():
  return 'OLDMAIN ' + call('fuu', foo, 'xx', 'yy')
