#Funkcja make.move() symuluje jeden ruch w grze.
make.move <- function(game, strategy) {
  
  game <- throw.dice(game, die1, die2)
  
  farmvector <- convert.game.table(game)
  farmvector <- strategy(farmvector)
  game  <- convert.farm.vector(farmvector)
  
  
  game
}

#Funkcja play() symuluje całą grę i zlicza liczbę rund.
play <- function(strategyname) {
  current.round = 1
  while (!game.finished(game)) {
    game <- make.move(game, strategyname)
    game[,"count"]
    current.round = current.round + 1
  }
  current.round
}
