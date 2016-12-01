#' Badanie gry SuperFarmer
#'
#' Funkcja \code{examine.game} bada rozkład czasu gry SuperFarmer.
#'
#' @param rounds Liczba wykonanych powtórzeń danej gry.
#' @param namestrategy Nazwa wykonywanej strategii.
#'
#'
#' @examples
#' examine.game(100,strategy_PDMS)
#'
#' @export
#' 
examine.game <- function(rounds, namestrategy){
  results <- 1:rounds
  for (i in 1:rounds) {
    results[i] = play(namestrategy)
  }
  summary(results)
}

