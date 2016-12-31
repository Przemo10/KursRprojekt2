#' Pojedyńczy ruch w grze SuperFarmer
#'
#' @description
#' Funkcja \code{make.move} wykonuje jeden ruch w grze SuperFarmer.
#' Jako wartość zwraca tabelę game po wykonaniu jednego ruchu w grze.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param strategy Nazwa wykonywanej strategii.
#' @param fox_leaves_rabbit Parametr mówiący czy lis zostawia jednego królika. Domyślna wartość 0 (nie zostawia).
#' @param wolf_eats_dog Parametr mówiący czy wilk zjada małego psa. Domyślna wartość 0 (nie zostawia).
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#'
#' @examples
#' make.move(game,strategia_DKA,0,0,dice1,dice2)
#'
#' @export
#'
make.move <-
  function(game, strategy,
           fox_leaves_rabbit  = 0,
           wolf_eats_dog = 0, die1 = dice1, die2 = dice2) {
    game <- throw.dice(game, fox_leaves_rabbit, wolf_eats_dog, die1, die2)

    farmvector <- convert.game.table(game)
    farmvector <- strategy(farmvector)
    game  <- convert.farm.vector(farmvector)


    game
  }

#' Jednokrotne wykonanie symulacji  gry SuperFarmer
#'
#' @description
#' Funkcja \code{gra} symuluje całą grę jeden raz.
#' Jako wartość zwraca liczbę rund potrzebnych do ukończenia gry.
#'
#' @param strategyname Nazwa wykonywanej strategii.
#' @param fox_leaves_rabbit Parametr mówiący czy lis zostawia jednego królika. Domyślna wartość 0 (nie zostawia).
#' @param wolf_eats_dog Parametr mówiący czy wilk zjada małego psa. Domyślna wartość 0 (nie zostawia).
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#'
#' @examples
#' gra(strategia_PDMS,0,1,dice1,dice2)
#'
#' @export
#'

gra <- function(strategyname,
           fox_leaves_rabbit = 0,
           wolf_eats_dog = 0,
           die1 = dice1,
           die2 = dice2) {
    current.round = 1
    while (!game.finished(game)) {
      game <-
        make.move(game,
                  strategyname,
                  fox_leaves_rabbit,
                  wolf_eats_dog,
                  die1,
                  die2)
      game[, "count"]
      current.round = current.round + 1
    }
    current.round
  }
