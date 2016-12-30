#' Badanie gry SuperFarmer
#'
#' Funkcja \code{examine.game} bada rozkład czasu gry SuperFarmer.
#'
#' @param rounds Liczba wykonanych powtórzeń danej gry.
#' @param namestrategy Nazwa wykonywanej strategii.
#' @param fox_leaves_rabbit  Parametr mówiący czy lis zostawia jednego królika. Domyślna wartość 0 (nie zostawia).
#' @param wolf_eats_dog Parametr mówiący czy wilk zjada małego psa. Domyślna wartość 0 (nie zostawia).
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#'
#' @return Podstawowe statystyki rozkładu długości gry dla zadanej strategii.
#'
#' @examples
#' examine.game(strategy_PDMS,100,0,0,die1,die2)
#'
#' @export
#'
examine.game <- function(namestrategy,rounds,fox_leaves_rabbit =0, wolf_eats_dog = 0,die1 = die1,die2 = die2){
  
  
  results <- 1:rounds
  for (i in 1:rounds) {
    results[i] = play(namestrategy,fox_leaves_rabbit,wolf_eats_dog,die1,die2)
  }
  list(summary(results),ggplot2::qplot(results, binwidth=1, col=I("lightgreen")))

  
}


