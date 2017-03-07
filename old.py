import time
from tmi import *
from web import link, mkform, Cookies, redirect, ListJoin
from tags import *

db = File('old.dat')
player = Deref(db, 'player')
card = Deref(db, 'card')
game = Deref(db, 'game')
hand = Deref(db, 'hand')
player_game = Deref(db, 'player_game')
invitation = Deref(db, 'invitation')

cookies = Cookies()

def IsLoggedIn():
  return And(HasField(Cookies(), 'login'), Not(Equals(Deref(cookies, 'login'), '')))

def logout():
  write(Deref(cookies, 'login'), '')
  return main()

def Footer():
  return List(br(), link('logout', logout))

def next_id(rel, id_field):
  return Str(Max(Column(rel, id_field)))

def CreateGame():
  game_id = next_id(game, 'game_id')
  write(game, Union(game, Rel(AddField({'time': time.time()}, 'game_id', game_id))))
  return redirect(AddPlayersToGame, game_id)

def players_invited_to_game(game_id):
  return Column(Where(invitation, lambda rec: rec['game_id'] == game_id), 'player_id')

def AddPlayersToGame(game_id):
  return List('Add players to game ', game_id,
    br(),
    'Already added:', br(),
    ListJoin(players_invited_to_game(game_id), br()), 
    mkform(AddPlayersToGameRcv, { 'player_name': '' }, { 'game_id': game_id }),
    link('done', DoneAddingPlayersToGame, game_id),
    Footer())

def AddPlayersToGameRcv(rec):
  game_id = rec['game_id']
  player_name = rec['player_name']
  player_id = Deref(One(Where(player, lambda rec: rec['name'] == player_name)), 'player_id')
  write(invitation, Union(invitation, Rel(AddField({'game_id': game_id}, 'player_id', player_id))))
  return redirect(AddPlayersToGame, game_id)

def DoneAddingPlayersToGame(game_id):
  return 'Done'

def PlayerMenu():
  return List('Hello, ', Deref(Cookies(), 'login'), 
    '<br>',
    link('create game', CreateGame),
    Footer())

def loginRcv(formdata):
  write(DerefOrNew(cookies, 'login'), Deref(formdata, 'username'))
  return redirect(PlayerMenu)

def Login():
  return List('Login:', br(), mkform(loginRcv, { 'username': '' }))

def main():
  return If(IsLoggedIn(), PlayerMenu(), Login())
