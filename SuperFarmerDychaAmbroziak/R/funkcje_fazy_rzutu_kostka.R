#' Faza rzutu kostką
#'
#' Funkcja \code{throw.dice} symuluje fazę gry związaną z rzutem kostką.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param die1 Kostka nr1.
#' @param die2 Kostka nr2.
#' 
#' @return Tablica game po wykonaniu rzutu kostką.
#' 
#' @examples
#' throw.dice(game,die1,die2)
#'
#' @export
#'
throw.dice <- function(game, die1, die2) {
  
  result1 = sample(die1, 1)
  result2 = sample(die2, 1)
  
  game <- wolf.reaction(game,result1)
  game <- fox.reaction(game,result2)
  game <- multiply.animals(game, result1, result2)
  
  game
}


#' Reakcja na wilka
#'
#' Funkcja \code{wolf.reaction} symuluje zmianę stada po wylosowaniu wilka.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param die1 Zwierzę otrzymane w wyniku losowania na kostce nr 1.
#' 
#' @return Tablica game po wykonaniu rzutu kostką.
#' 
#' @examples
#' wolf.reaction(game,"rabbit")
#' wolf.reaction(game,"wolf")
#'
#' @export
#'
wolf.reaction <- function(game, die1){
  if (die1 == "wolf") {
    if (get.count(game, "big_dog") > 0)
      game = change.count(game, "big_dog", -1)
    else 
      for (animal in row.names(game)) 
        
        if (animal != "horse" & animal!= "small_dog")
          game = clear.count(game, animal)
  }
  game 
}

#' Reakcja na lisa
#'
#' Funkcja \code{fox.reaction} symuluje zmianę stada po wylosowaniu lisa.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#' @param die2 Zwierzę otrzymane w wyniku losowania na kostce nr 2.
#' 
#' @return Tablica game po wykonaniu rzutu kostką.
#' 
#' @examples
#' fox.reaction(game,"rabbit")
#' fox.reaction(game,"small_dog")
#'
#' @export
#'
fox.reaction <- function(game,die2){
  if (die2 == "fox"){
    if (get.count(game, "small_dog") > 0)
      game = change.count(game, "small_dog", -1)
    else
      game = clear.count(game, "rabbit")
  }
  game
}

#' Dodaj po wylosowaniu
#'
#' Funkcja \code{multiply.animals} dodaje zwierzęta do stada po wykonaniu rzutu kostką..
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




