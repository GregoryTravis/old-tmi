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

def currentPlayerName():
  return Deref(cookies, 'login')

def currentPlayerId():
  # TODO rid of this read.
  return Deref(One(Where(player, lambda rec: rec['name'] == read(currentPlayerName()))), 'player_id')

def IsLoggedIn():
  return And(HasField(Cookies(), 'login'), Not(Equals(currentPlayerName(), '')))

def logout():
  write(currentPlayerName(), '')
  return main()

def Footer():
  return List(br(), link('home', PlayerMenu), ' ', link('logout', logout), br(),
    'Switch: ',
    ListJoin(
      Map(lambda player: link(player['name'], loginAs, player['name']),
        player),
      ' ')
  )

def Header():
  return List('Hello, ', currentPlayerName(), br())

def next_id(rel, id_field):
  return Str(Add(Max(Column(rel, id_field)), 1))

def CreateGame():
  game_id = next_id(game, 'game_id')
  write(game, Union(game, Rel(AddField({'time': time.time()}, 'game_id', game_id))))
  return redirect(AddPlayersToGame, game_id)

def players_invited_to_game(game_id):
  return Column(Where(Join(player, invitation), lambda rec: rec['game_id'] == game_id), 'name')

def AddPlayersToGame(game_id):
  return List('Add players to game ', game_id,
    br(),
    'Already added:', br(),
    ListJoin(players_invited_to_game(game_id), br()), 
    mkform(AddPlayersToGameRcv, { 'player_name': '' }, { 'game_id': game_id }),
    link('done', DoneAddingPlayersToGame, game_id),
    Footer())

# Shouldn't be able to invite players after game has started.
def AddPlayersToGameRcv(rec):
  game_id = rec['game_id']
  player_name = rec['player_name']
  player_id = Deref(One(Where(player, lambda rec: rec['name'] == player_name)), 'player_id')
  write(invitation, Union(invitation, Rel(AddField({'game_id': game_id, 'accepted': False, 'inviter': read(currentPlayerId())}, 'player_id', player_id))))
  return redirect(AddPlayersToGame, game_id)

def DoneAddingPlayersToGame(game_id):
  return 'Done'

def acceptInvitation(_invitation):
  write(
    Deref(One(Where(invitation, lambda rec: rec['game_id'] == _invitation['game_id'] and rec['player_id'] == _invitation['player_id'])),
      'accepted'),
    True)
  return PlayerMenu()

# TODO
# Get rid of this read()
# Make predcate nodes
# Rel->fun operator!
def playerName(player_id):
  return Deref(One(Where(player, lambda rec: rec['player_id'] == read(player_id))), 'name')

def askToAcceptInvitation(invitation):
  return List(Header(),
    'Accept invitation from ', playerName(Deref(invitation, 'inviter')), ' to game ', Deref(invitation, 'game_id'), '? ',
    link('Yes', acceptInvitation, invitation), ' ',
    link('No', PlayerMenu), br(),
    Footer()
  )

def invitationList(player_id):
  #print >>sys.stderr, player_id, read(player_id)
  #print >>sys.stderr, read(Column(Where(invitation, lambda rec: rec['player_id'] == read(player_id)), 'game_id'))
  return List(
    'Pending invitations: ',
    ListJoin(
      Map(lambda invitation: link(invitation['game_id'], askToAcceptInvitation, invitation),
        Where(invitation, lambda rec: rec['player_id'] == read(player_id) and rec['accepted'] == False)), ' ')
  )

def PlayerMenu():
  return List(Header(),
    link('create game', CreateGame), br(),
    invitationList(currentPlayerId()),
    Footer())

def loginAs(name):
  write(DerefOrNew(cookies, 'login'), name)
  return redirect(PlayerMenu)

def loginRcv(formdata):
  return loginAs(Deref(formdata, 'username'))

def Login():
  return List('Login:', br(), mkform(loginRcv, { 'username': '' }))

def main():
  return If(IsLoggedIn(), PlayerMenu(), Login())
