#' Badanie gry SuperFarmer
#'
#' Funkcja \code{examine.game} bada rozkład czasu gry SuperFarmer.
#'
#' @param rounds Liczba wykonanych powtórzeń danej gry.
#' @param namestrategy Nazwa wykonywanej strategii.
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#'
#' @examples
#' examine.game(strategy_PDMS,100,die1,die2)
#'
#' @export
#' 
examine.game <- function(namestrategy,rounds,die1,die2){
  results <- 1:rounds
  for (i in 1:rounds) {
    results[i] = play(namestrategy,die1,die2)
  }
  summary(results)
}

