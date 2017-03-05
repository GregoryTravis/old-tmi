from tmi import *
from web import call, mkform, setCookie
from tags import *

def hey(blah):
  setCookie('tmilogin', blah['a'])
  return ['hey ', str(blah)]

def main():
  db = File('old.dat')
  player = Deref(db, 'player')
  card = Deref(db, 'card')
  game = Deref(db, 'game')
  hand = Deref(db, 'hand')
  return mkform(hey, D(a=1, b=2, c=3))
