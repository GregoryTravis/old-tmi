from tmi import *
from web import call
from tags import *

def main():
  db = File('old.dat')
  player = Deref(db, 'player')
  card = Deref(db, 'card')
  game = Deref(db, 'game')
  hand = Deref(db, 'hand')
  return ['con', b('sequences')]
