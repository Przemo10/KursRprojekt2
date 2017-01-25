#' Badanie gry SuperFarmer
#'
#' Funkcja \code{badaj_gre} bada rozkład czasu gry SuperFarmer.
#'
#' @param rounds Liczba wykonanych powtórzeń danej gry.
#' @param namestrategy Nazwa wykonywanej strategii.
#' @param fox_leaves_rabbit  Parametr mówiący czy lis zostawia jednego królika. Domyślna wartość 0 (nie zostawia).
#' @param wolf_eats_dog Parametr mówiący czy wilk zjada małego psa. Domyślna wartość 0 (nie zostawia).
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#'
#' @return Lista trzyelementowa. Pierwszym elementem jest wektor długości kolejnych
#'  gier, drugim - podstawowe statystyki rozkładu czasu gry (wynik funkcji summary),
#'  trzecim elementem jest histogram przedstawiający rozkład otrzymanych rezultatów.
#'
#' @examples
#' badaj_gre(strategia_PDMS, 1000, 0, 0, dice1, dice2)
#' badaj_gre(strategia_PDMS, 1000)
#'
#' @export
#'
badaj_gre <- function(namestrategy,
           rounds,
           fox_leaves_rabbit = 0,
           wolf_eats_dog = 0,
           die1 = SuperFarmerDA::dice1,
           die2 = SuperFarmerDA::dice2) {
    results <- 1:rounds
    for (i in 1:rounds) {
      results[i] = gra(namestrategy, fox_leaves_rabbit, wolf_eats_dog, die1, die2)
    }
    gg <- ggplot2::qplot(results, binwidth = 1, col = I("lightgreen"))
    gg <- gg + ggplot2::labs(title=paste0("Strategy: ",as.character(substitute(namestrategy))))
    list(results, summary(results), gg )

  }


