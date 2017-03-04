from tmi import *
from web import call

def foo(a, b):
  return ss(Deref(File('old.dat'), 'player'))

def main():
  return 'OLDMAIN ' + call('fuu', foo, 'xx', 'yy')
