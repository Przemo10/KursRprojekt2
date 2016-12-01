#' Pojedyńczy ruch w grze SuperFarmer
#'
#' @description 
#' Funkcja \code{make.move} wykonuje jeden ruch w grze SuperFarmer.
#' Jako wartość zwraca tabelę game po wykonaniu jednego ruchu w grze.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param strategy Nazwa wykonywanej strategii.
#'
#' @examples
#' make.move(game,strategia_DKA)
#'
#' @export
#' 
make.move <- function(game, strategy) {
  
  game <- throw.dice(game, die1, die2)
  
  farmvector <- convert.game.table(game)
  farmvector <- strategy(farmvector)
  game  <- convert.farm.vector(farmvector)
  
  
  game
}

#' Jednokrotne wykonanie symulacji  gry SuperFarmer
#'
#' @description 
#' Funkcja \code{play} symuluje całą grę jeden raz.
#' Jako wartość zwraca liczbę rund potrzebnych do ukończenia gry.
#'
#' @param strategyname Nazwa wykonywanej strategii.
#'
#'
#' @examples
#' play(strategy.PDMS)
#'
#' @export
#' 

play <- function(strategyname) {
  current.round = 1
  while (!game.finished(game)) {
    game <- make.move(game, strategyname)
    game[,"count"]
    current.round = current.round + 1
  }
  current.round
}
