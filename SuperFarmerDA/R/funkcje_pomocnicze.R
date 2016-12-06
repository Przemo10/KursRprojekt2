#' Zmiana liczby danego zwierzęcia w stadzie
#'
#' Funkcja \code{change.count} zmienia liczbę danego zwierzęcia w stadzie o wskazaną wartość.
#'
#' @param game Tabela gry.
#' @param animal Nazwa zwierzęcia którego stan będziemy zwiększać.
#' @param change Wartość o którą zwiększamy liczebność danego zwierzęcia.
#'
#' @examples
#' change.count(game,"rabbit",10)
#' change.count(game,"big_dog",1)
#'
#' @export
#'
change.count <- function(game, animal, change) {
  
  change = change + game[animal, "count"]
  change = max(c(0, change))
  change = min(c(change, game[animal,"max.count"]))
  game[animal, "count"] = change
  game
}


#' Zmiana liczebności podzbioru stada o wskazaną wartość
#'
#' Funkcja \code{exchange.farm}zmienia liczebność wskazanego podzbioru stada
#'
#' @param game Tabela gry zawierająca informacje dotyczące gry.
#' @param farm Wskazany podzbiór zwierząt, których liczebność chcemy zmieniać o wskazaną wartość.
#' @param count Wartość o którą zmieniamy.
#'
#' @examples
#' exchange.farm(game,farm = c("rabbit","sheep","pig"),1)
#' exchange.farm(game,"big_dog",1)
#'
#' @export
#'
exchange.farm <- function(game, farm, count){
  for (animal in farm){
    game<-change.count(game,animal, count)
    
  }
  game
}

#' Wartość danego zwierzęcia liczona w królikach
#'
#' Funkcja \code{get.value}  zwraca wartość danego zwierzęcia liczoną w królikach.
#' 
#'
#' @param game Tabela gry.
#' @param animal Nazwa zwierzęcia.
#'
#' @examples
#' get.value(game,"horse")
#' get.value(game,"pig")
#' get.value(game,"rabbit")
#'
#' @export
#'
get.value <- function(game, animal) {
  
  game[animal, "value"]
}

#' Stan obecny danego zwierzęcia
#'
#' Funkcja \code{get.count} zwraca liczbę zwierząt danego gatunku, posiadanych w stadzie.
#' 
#'
#' @param game Tabela gry.
#' @param animal Nazwa zwierzęcia.
#'
#' @examples
#' get.count(game,"horse")
#' get.count(game,"pig")
#' get.count(game,"rabbit")
#'
#' @export
#'
get.count <- function(game, animal) {
  
  game[animal, "count"]
}

#' Zmiana stanu danego zwierzęcia na zerowy
#'
#' Funkcja \code{clear.count}  zeruje liczbę zwierząt danego gatunku.
#' 
#'
#' @param game Tabela gry SuperFarmer.
#' @param animal Nazwa zwierzęcia.
#'
#' @examples
#' clear.count(game,"horse")
#' clear.count(game,"pig")
#' clear.count(game,"rabbit")
#'
#' @export
#'

clear.count <- function(game, animal) {
  
 change.count(game, animal, -game[animal, "max.count"])

}

#' Zmiana liczebności całego stada na zerowy
#'
#' Funkcja \code{clear.all.counts}  zeruje liczbę wszystkich zwierząt.
#' 
#'
#' @param game Tabela gry SuperFarmer.
#'
#' @examples
#' clear.all.counts(game)
#'
#' @export
#'

clear.all.counts <- function(game) {
  
  game[,"count"] <- 0
  game
  
}


#' Warunek kończący grę
#'
#' Funkcja \code{game.finished} czy warunek końca rozgrywki jest spełniony.
#'
#' @param game Tabela gry SuperFarmer.
#' 
#' @examples
#' game.finished(game)
#'
#' @export
#'
game.finished <- function(game) {
  
  all(game[,"count"] >= game[,"win.condition"])
}


#' Wymiana dwojga zwierząt
#'
#' Funkcja \code{exchange.two.animals} zwierzę nr 1 na zwierzę nr 2.
#'
#' @param game Tabela gry SuperFarmer.
#' @param animal1 Zwierzę które chcemy zamienić.
#' @param animal1count Liczba zwierząt które zamieniamy.
#' @param animal2 Zwierzę które chcemy dostać.
#' @param animal2count Liczba zwierząt które powinniśmy dostać jeśli nie przekroczyliśmy limitu.
#' 
#' @examples
#' exchange.two.animals(game,"pig","rabbit",1,12)
#' exchange.two.animals(game,"pig","rabbit",1,get.value(game,"pig"))
#'
#' @export
#'
exchange.two.animals <- function(game, animal1, animal2, animal1count, animal2count ){
  game<- change.count(game,animal1, -animal1count)
  game<- change.count(game,animal2, animal2count)
  game
}

#' Przetwarzanie wektora do tablicy game
#'
#' Funkcja \code{convert.farm.vector} konwertuje wektor do tabeli game.
#' Służy ona do umożliwienia uruchomienia innych  strategii w naszym pakiecie.
#' 
#' @param farm Wektor zawierający stan stada w chwili obecnej.
#'
#' @details Zgodnie z ustaleniami wektor, który ma być przekazywany jest innej postaci niż ustalony.
#' Nasza tabela game ma zwierzęta w kolejności królik, mały pies, owca, świnia, duży pies, krowa, koń.
#' W związku z tym konieczna jest funkcja przekazująca wektor do tablicy game.
#' 
#' @examples
#' convert.farm.vector(c(7:1))
#' convert.farm.vector(c(10,2,1,1,0,0,0))
#'
#' @export
#' 
#
convert.farm.vector <- function(farm){
  game[,"count"] <- c( farm[1],farm[6],farm[2],farm[3],farm[7],farm[4],farm[5])
  game
}

#' Przetwarzanie tablicy game do wektora
#'
#' Funkcja \code{convert.game.table} konwertuje tabelę game do wektora farm.
#' Służy ona do umożliwienia uruchomienia naszej strategii w innych pakietach gry.
#' 
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#'
#' @details Zgodnie z ustaleniami wektor, który ma być przekazywany jest innej postaci niż ustalony.
#' Nasza tabela game ma zwierzęta w kolejności królik, mały pies, owca, świnia, duży pies, krowa, koń.
#' W związku z tym konieczna jest funkcja konwertująca.
#' 
#' @examples
#' convert.game.table(game)
#'
#' @export
#' 
#'
convert.game.table <- function(game){ 
  farm <- game[c("rabbit","sheep","pig","cow","horse","small_dog","big_dog"),"count"]
  farm
}
