# scratchpad


buyIn = 5
nPlayers = 3

accounts = list(POT=0, PLAYERS=rep(buyIn,nPlayers), LAST.PLAYER=NA, LAST.SPIN='-')

dummyRule <- function(accounts, player){
  return(accounts)
}

standardVictory <- function(accounts, player){
  # check if any play has the whole pot.
  if(accounts$POT == 0 & sum(accounts$PLAYERS > 0) == 1){
    return(which(accounts$PLAYERS > 0))
  } else {
    # return NULL if no victory acheived
    return(NULL)
  }
}

anteOnEmpty <- function(accounts, player){
  if(accounts$POT == 0){
    # print("ante")
    # print(accounts$PLAYERS)
    # increment by one for each player still in the game
    accounts$POT = accounts$POT + sum(accounts$PLAYERS > 0)
    # decrement each player's pool
    accounts$PLAYERS[accounts$PLAYERS > 0] = accounts$PLAYERS[accounts$PLAYERS > 0] - 1
  }
  return(accounts)
}

standardSpin <- function(accounts, player){
  sides = c('g', 'h', 'n', 's')
  spin = sample(sides, 1)
  # print(spin)
  if(spin == 'g'){
    # ORDER MATTERS HERE
    accounts$PLAYERS[player] = accounts$PLAYERS[player] + accounts$POT
    accounts$POT = 0
  } else if(spin == 'h'){
    # round up
    amnt = ceiling(accounts$POT/2)
    accounts$PLAYERS[player] = accounts$PLAYERS[player] + amnt
    accounts$POT = accounts$POT - amnt
  }  else if(spin == 's'){
    # decrement player by one and increment pot by one
    accounts$POT = accounts$POT + 1
    accounts$PLAYERS[player] = accounts$PLAYERS[player] - 1
  }
  #else if(spin == 'n'){
  # do nothing
  #}
  accounts$LAST.SPIN=spin
  return(accounts)
}

eatNone = dummyRule

#this function causes an infinite loop somehow
eatOneOnGimel <- function(accounts, player){
  if(accounts$LAST.SPIN == 'g'){
    accounts$PLAYERS[player] = accounts$PLAYERS[player] - 1
  }
  return(accounts)
}

turnSequence <- list(anteOnEmpty, standardSpin) #, eatOneOnGimel)

runGame <- function(accounts, rules, victoryCondition, maxTurns=700){
  turns = 0
  winner = NULL
  player = 0
  while(is.null(winner) & turns <= maxTurns){
    if(player == length(accounts$PLAYERS)){
      player = 1
    } else {
      player = player + 1
    }
    if(accounts$PLAYERS[player] <= 0){
      next
    }
    turns = turns + 1
    for(stepfunc in rules){
      if(accounts$PLAYERS[player] <= 0) break
      accounts = stepfunc(accounts, player)
    }
    winner = victoryCondition(accounts)
    accounts$LAST.PLAYER = player
    #print(accounts)
  }
  if(is.null(winner)) winner = NA
  return(list(winner=winner, turns=turns))
}

#runGame(accounts, turnSequence, standardVictory)

turnSequence <- list(anteOnEmpty, standardSpin, eatOneOnGimel)

runManyGames <- function(buyIn=5, nPlayers=3, nGames=2000){
  accounts = list(POT=0, PLAYERS=rep(buyIn,nPlayers))
  
  # run the games
  manyGames = list()
  for(g in c(1:nGames)){
    print(g)
    outcome = runGame(accounts, turnSequence, standardVictory)
    manyGames[[g]] = as.data.frame(list(GAME=g, WINNER=outcome$winner, TURNS=outcome$turns))
  }
  results = do.call(rbind, manyGames)
  return(results)
}

results = runManyGames(buyIn=7, nPlayers=3)
median(results$TURNS)
hist(results$TURNS)
hist(results$WINNER)
