import random
import time
from tmi import *
from web import link, mkform, Cookies, redirect, ListJoin, Link
from tags import *

CARDS_IN_HAND = 5

# This is to allow testing to work.
random.seed(0)

db = File('old.dat')
player = Deref(db, 'player')
card = Deref(db, 'card')
game = Deref(db, 'game')
hand = Deref(db, 'hand')
roster = Deref(db, 'roster')
player_game = Deref(db, 'player_game')
invitation = Deref(db, 'invitation')
turn = Deref(db, 'turn')

player_name_to_id = RelFun(player, 'name', 'player_id')
player_id_to_name = RelFun(player, 'player_id', 'name')
games_invited_to = RelFunM(invitation, 'player_id', 'game_id')
games_with_unaccepted_invitations = RelFunM(invitation, 'accepted', 'game_id')(False)
invitees_of_game = RelFunM(invitation, 'game_id', 'player_id')
inviters_of_game = RelFunM(invitation, 'game_id', 'inviter')
players_of_game = RelFunM(roster, 'game_id', 'player_id')
game_next = RelFun(turn, 'game_id', 'next')
games_of_player = RelFunM(roster, 'player_id', 'game_id')
cards_of_g_p = RelFunSM(hand, ['game_id', 'player_id'], ['card_id'])
card_name = RelFun(card, 'card_id', 'card_name')

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
  return Str(Add(Max(SomeOr(Column(rel, id_field), [0])), 1))

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

def allInvitationsAccepted(game_id):
  return Equals(0, Len(Where(invitation, Pand(Feq('accepted', False), Feq('game_id', game_id)))))

def inviterOfGame(game_id):
  return SameGet(inviters_of_game(game_id))

def generateCardsFor(game_id, player_id):
  # Don't like this read() but this is the rng problem.
  ncards = read(Len(card))
  # This assumes the card ids are 0..N
  return Map(lambda card_id: Rec(game_id=game_id, player_id=player_id, card_id=card_id),
             Ntimes(lambda: random.randrange(0, ncards), CARDS_IN_HAND))

def dealCards(game_id):
  write(hand, Union(hand,
    Flatten1(Map(lambda player_id: generateCardsFor(game_id, player_id),
                 players_of_game(game_id)))))

def createRoster(game_id):
  players = Union(invitees_of_game(game_id), List(inviterOfGame(game_id)))
  newRoster = Map(lambda rec, order: Rec(order=order, **rec),
    Map(lambda player_id: Rec(player_id=player_id, game_id=game_id, score=30), players),
    Sequence(0, Len(players)))
  write(roster, Union(roster, newRoster))
  commit()
  dealCards(game_id)
  commit()
  write(turn, Union(turn, Rel(Rec(game_id=game_id, next=0))))

def acceptInvitation(_invitation):
  write(
    Deref(One(Where(invitation, Receq(Subrec(_invitation, ['game_id', 'player_id'])))),
      'accepted'),
    True)
  commit() # TODO ugh
  if read(allInvitationsAccepted(_invitation['game_id'])):
    createRoster(_invitation['game_id'])
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
  return List(
    'Pending invitations: ',
    ListJoin(
      Map(lambda invitation: link(invitation['game_id'], askToAcceptInvitation, invitation),
        Where(invitation, Pand(Feq('player_id', player_id), Feq('accepted', False)))),
      '')
  )

def whoIsNext(game_id):
  next_ord = game_next(game_id)
  return Deref(
    RelFunS(roster, ['game_id', 'order'], ['player_id'])(Rec(game_id=game_id, order=next_ord)),
    'player_id')

def playerIsNext(player_id, game_id):
  return Equals(player_id, whoIsNext(game_id))

def readyToPlay(game_id, player_id):
  return If(playerIsNext(player_id, game_id),
    yourTurn(game_id, player_id),
    'Not your turn')

def updatePlayerScore(game_id, player_id, card_id):
  other_player_score = Deref(One(Where(roster, Receq(Rec(game_id=game_id, player_id=player_id)))), 'score')
  card_points = Deref(One(Where(card, Feq('card_id', card_id))), 'points')
  write(other_player_score, Add(other_player_score, card_points))

def advanceTurn(game_id):
  num_players_in_roster = Len(players_of_game(game_id))
  next = Deref(One(Where(turn, Feq('game_id', game_id))), 'next')
  write(next, Mod(Add(next, 1), num_players_in_roster))
  commit()

def removeCardFromHand(game_id, player_id, card_id):
  # This really terrifies me.
  write(hand, Where(hand, Pnot(Receq(Rec(game_id=game_id, card_id=card_id, player_id=player_id)))))

def playCardOn(game_id, player_id, card_id, other_player_id):
  updatePlayerScore(game_id, other_player_id, card_id)
  commit()
  advanceTurn(game_id)
  commit()
  removeCardFromHand(game_id, player_id, card_id)
  return List(Header(),
    'You played "', card_name(card_id) , '" on ', player_id_to_name(other_player_id), '.', br(),
    Footer())

def playCardPickWho(game_id, player_id, card_id):
  return List(Header(),
    'It is your turn.', br(),
    'Play card "', card_name(card_id), '" on:', br(),
    ListJoin(
      Map(
        lambda other_player_id: Link(player_id_to_name(other_player_id), playCardOn, game_id, player_id, card_id, other_player_id),
        Difference(players_of_game(game_id), List(player_id))),
      br()),
    Footer())

def yourTurn(game_id, player_id):
  return List(Header(),
    'It is your turn.', br(),
    'Pick a card to play:', br(),
    ListJoin(
      Map(
        lambda card_id: Link(card_name(card_id), playCardPickWho, game_id, player_id, card_id),
        Map(lambda rec: Deref(rec, 'card_id'), cards_of_g_p(Rec(game_id=game_id, player_id=player_id)))),
      br()),
    Footer())

def inProgressGamesList(player_id):
  #games_ready_to_go = Difference(games_invited_to(player_id), games_with_unaccepted_invitations)

  # This does not check if the game is done
  games_ready_to_go = games_of_player(player_id)
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
