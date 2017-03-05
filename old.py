from tmi import *
from web import call, mkform
from tags import *

def hey(blah):
  return ['hey ', str(blah)]

def main():
  db = File('old.dat')
  player = Deref(db, 'player')
  card = Deref(db, 'card')
  game = Deref(db, 'game')
  hand = Deref(db, 'hand')
  return mkform(hey, D(a=1, b=2, c=3))
