from tmi import *
from web import call, mkform, Cookies
from tags import *

cookies = Cookies()
def IsLoggedIn():
  return And(HasField(Cookies(), 'tmilogin'), Not(Equals(Deref(cookies, 'tmilogin'), '')))

def logout():
  write(Deref(cookies, 'tmilogin'), '')
  return main()

def Footer():
  return List(br(), call('logout', logout))

def PlayerMenu():
  return List('Hello, ', Deref(Cookies(), 'tmilogin'), Footer())

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
