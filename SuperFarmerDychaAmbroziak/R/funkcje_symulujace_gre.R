#' Pojedyńczy ruch w grze SuperFarmer
#'
#' @description 
#' Funkcja \code{make.move} wykonuje jeden ruch w grze SuperFarmer.
#' Jako wartość zwraca tabelę game po wykonaniu jednego ruchu w grze.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param strategy Nazwa wykonywanej strategii.
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#'
#' @examples
#' make.move(game,strategia_DKA,die1,die2)
#'
#' @export
#' 
make.move <- function(game, strategy,die1,die2) {
  
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
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#'
#' @examples
#' play(strategy_PDMS,die1,die2)
#'
#' @export
#' 

play <- function(strategyname,die1,die2) {
  current.round = 1
  while (!game.finished(game)) {
    game <- make.move(game, strategyname,die1,die2)
    game[,"count"]
    current.round = current.round + 1
  }
  current.round
}
