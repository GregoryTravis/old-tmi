from tmi import *
from web import call, mkform, Cookies
from tags import *

cookies = Cookies()
def IsLoggedIn():
  return HasField(Cookies(), 'tmilogin')

def PlayerMenu():
  return List('Hello, ', Deref(Cookies(), 'tmilogin'))

def loginRcv(formdata):
  write(Deref(cookies, 'tmilogin'), Deref(formdata, 'username'))
  return PlayerMenu()

def Login():
  return mkform(loginRcv, { 'username': '' })

def main():
  db = File('old.dat')
  player = Deref(db, 'player')
  card = Deref(db, 'card')
  game = Deref(db, 'game')
  hand = Deref(db, 'hand')

  return If(IsLoggedIn(), PlayerMenu(), Login())
