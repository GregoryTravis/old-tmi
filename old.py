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

player_name_to_id = RelFun(player, 'name', 'player_id')
player_id_to_name = RelFun(player, 'player_id', 'name')
games_invited_to = RelFunM(invitation, 'player_id', 'game_id')
games_with_unaccepted_invitations = RelFunM(invitation, 'accepted', 'game_id')(False)

cookies = Cookies()

def currentPlayerName():
  return Deref(cookies, 'login')

def currentPlayerId():
  return player_name_to_id(currentPlayerName())

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
  write(game, Union(game, Rel(Rec(time=0, game_id=game_id))))
  return redirect(AddPlayersToGame, game_id)

def players_invited_to_game(game_id):
  return Column(Where(Join(player, invitation), Feq('game_id', game_id)), 'name')

def AddPlayersToGame(game_id):
  return List('Add players to game ', game_id,
    br(),
    'Already added:', br(),
    ListJoin(players_invited_to_game(game_id), br()), 
    mkform(AddPlayersToGameRcv, { 'player_name': '' }, { 'game_id': game_id }),
    link('done', DoneAddingPlayersToGame, game_id),
    Footer())

# Shouldn't be able to invite players after game has started.
# Shouldn't be able to invite yourself to a game.
def AddPlayersToGameRcv(rec):
  game_id = rec['game_id']
  player_name = rec['player_name']
  player_id = player_name_to_id(player_name)
  write(invitation, Union(invitation, Rel(Rec(game_id=game_id, accepted=False, inviter=currentPlayerId(), player_id=player_id))))
  return redirect(AddPlayersToGame, game_id)

def DoneAddingPlayersToGame(game_id):
  return 'Done'

def acceptInvitation(_invitation):
  write(
    Deref(One(Where(invitation, Receq(Subrec(_invitation, ['game_id', 'player_id'])))),
      'accepted'),
    True)
  return PlayerMenu()

def playerName(player_id):
  return player_id_to_name(player_id)

def askToAcceptInvitation(invitation):
  return List(Header(),
    'Accept invitation from ', playerName(Deref(invitation, 'inviter')), ' to game ', Deref(invitation, 'game_id'), '? ',
    link('Yes', acceptInvitation, invitation), ' ',
    link('No', PlayerMenu), br(),
    Footer()
  )

def invitationList(player_id):
  #print >>sys.stderr, player_id, read(player_id)
  #print >>sys.stderr, read(Column(Where(invitation, Feq('player_id', player_id)), 'game_id'))
  return List(
    'Pending invitations: ',
    ListJoin(
      Map(lambda invitation: link(invitation['game_id'], askToAcceptInvitation, invitation),
        Where(invitation, Pand(Feq('player_id', player_id), Feq('accepted', False)))),
      '')
  )

def readyToPlay(game_id, player_id):
  pass

def inProgressGamesList(player_id):
  games_ready_to_go = Difference(games_invited_to(player_id), games_with_unaccepted_invitations)
  return List('Games in progress: ',
    ListJoin(
      Map(lambda game_id: link(game_id, readyToPlay, game_id, player_id), games_ready_to_go), ' '))

def PlayerMenu():
  return List(Header(),
    link('create game', CreateGame), br(),
    invitationList(currentPlayerId()), br(),
    inProgressGamesList(currentPlayerId()), br(),
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
