from tmi import *
from web import link, mkform, Cookies, redirect
from tags import *

cookies = Cookies()
def IsLoggedIn():
  return And(HasField(Cookies(), 'login'), Not(Equals(Deref(cookies, 'login'), '')))

def logout():
  write(Deref(cookies, 'login'), '')
  return main()

def Footer():
  return List(br(), link('logout', logout))

def PlayerMenu():
  return List('Hello, ', Deref(Cookies(), 'login'), Footer())

def loginRcv(formdata):
  write(DerefOrNew(cookies, 'login'), Deref(formdata, 'username'))
  return redirect(PlayerMenu)

def Login():
  return mkform(loginRcv, { 'username': '' })

def main():
  db = File('old.dat')
  player = Deref(db, 'player')
  card = Deref(db, 'card')
  game = Deref(db, 'game')
  hand = Deref(db, 'hand')

  return If(IsLoggedIn(), PlayerMenu(), Login())
