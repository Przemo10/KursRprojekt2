#' Faza rzutu kostką
#'
#' Funkcja \code{throw.dice} symuluje fazę gry związaną z rzutem kostką.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param foxparam Parametr mówiący czy lis zostawia jednego królika. Domyślna wartość 0 (nie zostawia).
#' @param wolfparam Parametr mówiący czy wilk zjada małego psa. Domyślna wartość 0 (nie zostawia).
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#' 
#' @return Tablica game po wykonaniu rzutu kostką.
#' 
#' @examples
#' throw.dice(game,0,1,die1,die2)
#'
#' @export
#'
throw.dice <- function(game, foxparam = 0, wolfparam = 0 ,die1, die2) {
  
  result1 = sample(die1, 1)
  result2 = sample(die2, 1)
  
  game <- wolf.reaction(game,result1,foxparam)
  game <- fox.reaction(game,result2,wolfparam)
  game <- multiply.animals(game, result1, result2)
  
  game
}


#' Reakcja na wilka
#'
#' Funkcja \code{wolf.reaction} symuluje zmianę stada po wylosowaniu wilka.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param die1 Zwierzę otrzymane w wyniku losowania na kostce nr 1.
#' @param keepsmalldog Parametr dodatkowy mówiący czy wilk zjada małego psa. 
#' W przypadku gdy wilk zjada małego psa należy wstawić 1. W przeciwnym przypadku 0.
#' 
#' @return Tablica game po wykonaniu rzutu kostką.
#' 
#' @examples
#' wolf.reaction(game,"rabbit",0)
#' wolf.reaction(game,"wolf",1)
#'
#' @export
#'
wolf.reaction <- function(game, die1, keepsmalldog = 0) {
  if (die1 == "wolf") {
    if (get.count(game, "big_dog") > 0)
      game = change.count(game, "big_dog",-1)
    else {
      for (animal in row.names(game)) {
        if (animal != "horse" & animal != "small_dog")
          game = clear.count(game, animal)
      }
      if (keepsmalldog == 1)
        game = change.count(game, "small_dog", -1)
    }
  }
  game
}

#' Reakcja na lisa
#'
#' Funkcja \code{fox.reaction} symuluje zmianę stada po wylosowaniu lisa.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param die2 Zwierzę otrzymane w wyniku losowania na kostce nr 2.
#' @param keeprabbit Parametr dodatkowy mówiący, czy list zostawia jednego królika.
#' Za parametr należy przyjąć 1 gdy mały pies zostawia jednego królika oraz 0 w przeciwnym przypadku.
#' @return Tablica game po wykonaniu rzutu kostką.
#' 
#' @examples
#' fox.reaction(game,"rabbit",1)
#' fox.reaction(game,"small_dog",0)
#'
#' @export
#'
fox.reaction <- function(game, die2, keeprabbit = 0){
  
  if (die2 == "fox"){
    if (get.count(game, "small_dog") > 0)
      game = change.count(game, "small_dog", -1)
    else
      game["rabbit","count"] <- min(keeprabbit,get.count(game,"rabbit"))
      
    }
  game
}

#' Dodaj po wylosowaniu
#'
#' Funkcja \code{multiply.animals} dodaje zwierzęta do stada po wykonaniu rzutu kostką.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param animal1 Zwierzę otrzymane w wyniku losowania na kostce nr 1.
#' @param animal2 Zwierzę otrzymane w wyniku losowania na kostce nr 2.
#' 
#' @return Tablica game po wykonaniu rzutu kostką.
#' 
#' @examples
#' multiply.animals(game,"rabbit","sheep")
#' multiply.animals(game,"pig","pig")
#' multiply.animals(game,"cow","pig")
#'
#' @export
#'
multiply.animals <- function(game, animal1, animal2) {
  
  
  if (animal1 == animal2)
    game = change.count(game, animal1, (get.count(game, animal1) + 2) %/% 2)
  else {
    
    if (animal1 %in% row.names(game))
      game = change.count(game, animal1, (get.count(game, animal1) + 1) %/% 2)
    
    if (animal2 %in% row.names(game))
      game = change.count(game, animal2, (get.count(game, animal2) + 1) %/% 2)
  }
  
  game
}




