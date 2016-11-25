#Funkcja make.move() symuluje jeden ruch w grze.
make.move <- function(game, strategy = strategy1(vector)) {
  
  farmvector <- strategy
  game  <- convert.farm.vector(farmvector)
  
  if (!game.finished(game))
    game <- throw.dice(game, die1, die2)
  
  game
}

#Funkcja play() symuluje całą grę i zlicza liczbę rund.
play <- function(strategyname) {
  current.round = 1
  while (!game.finished(game)) {
    farmvector <- convert.game.table(game)
    game <- make.move(game, strategyname(farmvector) )
    game[,"count"]
    current.round = current.round + 1
  }
  current.round
}
