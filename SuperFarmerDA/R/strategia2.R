#' Strategia testowa
#'
#' Funkcja \code{strategy_test} 
#' @param vector Wektor zawierający informację dotyczącego posiadanego stada w chwili obecnej.
#' 
#'
#' @details Strategia jest strategią jeden  na wiele. Można wymieniać króliki tylko na zwierzęta o wyższej wartości.
#' \itemize{
#'  \item Konwersji wektora do tabeli gra - możliwość uruchomienia  strategii zewnętrznych w naszym pakiecie.
#'  \item Wymiany nadmiarowych królików na brakujące zwierzęta.
#'  \item Wymiany zwierząt na konia.
#'  \item Wymiany nadmiarowego konia na zwierzęta - zakończenie gry.
#'  \item Konwersji do wektora - możliwość uruchomienia w innych pakietach.
#' }
#'
#' @examples
#' strategy_test(c(8,1,3,0,0,0,1))
#'
#' @export
#'

strategy_test <- function(vector){
  game<- convert.farm.vector(farm = vector)
  #nazwa strategii
  game<- exchange.horse(game)
  game <- get.horse(game)  
  game <- exchange.rabbits.for.animals(game)
  
  vector <- convert.game.table(game)
  vector
}
