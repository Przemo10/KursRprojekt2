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
#' @return Lista dwuelementowa. Pierwszym elementem jest wynik funkcji summary od wektora
#' długości kolejnych gier (podstawowe statystyki rozkładu), drugim - wykres rozkładu czasu gry.
#'
#' @examples
#' examine.game(strategy_PDMS,100,0,0, dice1, dice2)
#'
#' @export
#'
examine.game <-
  function(namestrategy,
           rounds,
           fox_leaves_rabbit = 0,
           wolf_eats_dog = 0,
           die1 = dice1,
           die2 = dice2) {
    results <- 1:rounds
    for (i in 1:rounds) {
      results[i] = play(namestrategy, fox_leaves_rabbit, wolf_eats_dog, die1, die2)
    }
    list(results, summary(results), ggplot2::qplot(results, binwidth = 1, col = I("lightgreen")))

  }


