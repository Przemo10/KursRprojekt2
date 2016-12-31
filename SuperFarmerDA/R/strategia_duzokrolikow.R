#' Wartość stada w królikach
#'
#' Funkcja \code{value.in.rabbit} zwraca informację o wartości posiadanego stada.
#'
#' @param game Tabela gry.
#'
#' @examples
#' value.in.rabbit(game)
#'
#' @export
#'

value.in.rabbit <- function(game){

  sum(game[,"value"] *game[,"count"])

}

#' Wartość stada bez wskazanych zwierząt
#'
#' Funkcja \code{value.in.rabbit.without.animal} zwraca informację
#'  o wartości posiadanego statada bez wskazanego zwierzęcia.
#'
#' @param game Tabela gry.
#' @param animal Nazwa zwierzęcia.
#'
#' @examples
#' value.in.rabbit.without.animal(game,"rabbit")
#'
#' @export
#'

value.in.rabbit.without.animal <- function(game,animal){
  sum(game[row.names(game)!= animal,"value"] *game[row.names(game)!= animal,"count"])
}



#' Króliki startowe
#'
#' Funkcja \code{get.initial.rabbits} ma na celu wymianę zwierząt na króliki dopóki
#'  nie osiągniemy oczekiwanej przez nas liczby.
#'
#' @param game Tabela gry.
#' @param rabbits Liczba królików, którą chcemy zachować.
#'
#' @examples
#' get.initial.rabbits(game,12)
#'
#' @export
#'

 get.initial.rabbits <- function(game, rabbits){

   if( value.in.rabbit(game) <= rabbits && get.count(game,"sheep")!= 2){

     value <- value.in.rabbit(game)
     game <- clear.all.counts(game)
     game["rabbit","count"] <- value

   }

   game
 }


 #' Branie małych psów
 #'
 #' Funkcja \code{get.small.dog} wymienia króliki na małego psa.
 #'
 #' @param game Tabela gry.
 #' @param count1 Minimalna liczba królików przy których dokonujemy wymiany na małego psa
 #' @param count2 Liczba obecnie posiadanych małych psów
 #'
 #' @examples
 #' get.small.dog(game,41,1)
 #'
 #' @export
 #'

 get.small.dog <- function(game, count1, count2){

   if(get.count(game,"rabbit") >= count1 &&
      get.count(game,"rabbit") >= 6 &&
      get.count(game,"small_dog") == count2 ){

     game <-exchange.two.animals(game,"rabbit","small_dog",get.value(game,"small_dog"),1)

   }
 game
}

 #' Wymiana droższych zwierząt na króliki
 #'
 #' Funkcja \code{get.more.rabbits} wymienia (domyślnie) świnię lub owcę na króliki.
 #'
 #' @param game Tabela gry.
 #' @param maxrabbit Liczba królików, której nie chcemy przekroczyć. Wymiana,
 #' która dawałaby większą liczbę króliów nie wykona się.
 #' @param farm wektor zawierający nazwy zwierząt które chcialibyśmy wymienić na króliki.
 #'
 #' @examples
 #' get.more.rabbits(game, 40)
 #'
 #' @export
 #'


 get.more.rabbits <- function(game, maxrabbit, farm = c("sheep", "pig")){

     for (animal in rev(farm)){
       if(get.count(game,animal) > 0 && (get.value(game, animal) + get.count(game, "rabbit")) <= maxrabbit){
         game <- exchange.two.animals(game, animal1 = animal, animal2 = "rabbit", 1, get.value(game, animal = animal))
       }
     }

   game
 }


 #' Dodanie dodatkowego zwierzęcia
 #'
 #' Funkcja \code{get.extra.animal} zamienia króliki na zwierzęta w sytuacji
 #'  gdy mamy więcej królików od zadelkarowanej wartości.
 #'
 #' @param game Tabela gry.
 #' @param rabbitcount Liczba królików przy których dokonujemy wymiany.
 #' @param animal Nazwa zwierzęcia, które chcemy dostać.
 #' @param animalcount Liczba obecnie posiadanych zwierząt gatunku \code{animal}.
 #'
 #' @examples
 #' get.extra.animal(game,45,"sheep",2)
 #'
 #' @export
 #'

 get.extra.animal <-
   function(game, rabbitcount, animal, animalcount) {
     total <-  get.value(game, animal)

     if (get.count(game, "rabbit") > rabbitcount &&
         rabbitcount > total && animalcount == get.count(game, animal)) {
       game <- change.count(game, animal, 1)
       game <- change.count(game, "rabbit", -total)
     }

   game
 }

 #' Kupno konia
 #'
 #' Funkcja \code{buy.horse.for.animals} dodaje nam konia do stada.
 #'
 #' @param game Tabela gry.
 #' @param rabbitcount Minimalna liczba królików, które chcemy zostawić.
 #'
 #' @examples
 #' buy.horse.for.animals(game,12)
 #'
 #' @export
 #'

 buy.horse.for.animals <- function(game, rabbitcount){

   if(value.in.rabbit(game) >= (rabbitcount + get.value(game,"horse")) &&
      get.count(game,"horse") == 0){


     game <-  change.count(game,"rabbit",value.in.rabbit.without.animal(game,"rabbit") - get.value(game,"horse"))
     game <-  change.count(game, "horse",1)
     game <-  clear.count(game, c("small_dog","sheep","pig","cow","big_dog"))
   }

   if(value.in.rabbit.without.animal(game,"horse") >= get.value(game,"horse") && get.count(game,"horse") == 1 ){
     game <- clear.all.counts(game)
     game <- change.count(game,"horse",2)
   }
   game
 }

 #' Kupowanie ostatniego zwierzęcia potrzebnego do wygranej.
 #'
 #' Funkcja \code{get.all} kupuje krowę jeśli jest mamy wszystkie inne zwierzęta.
 #'
 #' @param game Tabela gry.
 #'
 #' @examples
 #' get.small.dog(game,41,1)
 #'
 #' @export
 #'


 get.all <- function(game){
   if(game["horse", "count"] == 1 &&
      value.in.rabbit(game) >= 127 &&
      get.count(game,"rabbit") > 0 &&
        get.count(game,"sheep") > 0 &&
        get.count(game,"pig") > 0){
     value.of.others <- sum(game[c("sheep", "pig", "cow", "horse"),"value"])
     curr.value.of.all <- value.in.rabbit(game)
     game <- clear.all.counts(game)
     game <- change.count(game, "rabbit", curr.value.of.all-value.of.others)
     game <- change.count(game, "sheep", 1)
     game <- change.count(game, "pig", 1)
     game <- change.count(game, "cow", 1)
     game <- change.count(game, "horse", 1)
   }
   game
 }

 #' Strategia maxrabbit
 #'
 #' Strategia mająca na celu otrzymanie jak największej liczby królików.
 #'
 #' @param vector Wektor z liczebnością stada.
 #'
 #' @return Wektor z zadaną liczebnością stada po wykonaniu strategii.
 #'
 #' @details Strategia jest strategią wiele na wiele. Składa się z następujących etapów:
 #' \itemize{
 #'  \item Konwersji wektora do tabeli gra - możliwość uruchomienia  strategii zewnętrznych w naszym pakiecie.
 #'  \item Zdobycie oczekiwanej przez nas liczby królików.
 #'  \item Zdobycie małego psa.
 #'  \item Zdobycie przynajmniej 40 królików.
 #'  \item Zdobycie drugiego małego psa.
 #'  \item Zamiany zwierząt na dodatkowe w przypadku gdy mamy więcej niż 40 królików.
 #'  \item Kupna konia.
 #'  \item Zamiany drugiego konia na pozostałe zwierzęta.
 #' }
 #'
 #' @author
 #' Przemysław Dycha, Dominik Ambroziak
 #' @examples
 #' strategia_maxrabbit(c(8,1,3,0,0,0,1))
 #'
 #' @export
 #'



 strategia_maxrabbit <- function(vector){


   game <- convert.farm.vector(farm = vector)
   prev.rab <- get.count(game, "rabbit")
   game <- get.initial.rabbits(game, 12)
   if(prev.rab == get.count(game, "rabbit"))
   game <- get.more.rabbits(game, 40)
   

   if (get.count(game, "rabbit") >= 25  && get.count(game,"small_dog") > 0) {
     if (get.count(game, "small_dog") == 1) {
       game <- get.small.dog(game, 25, 1)

     } else {
       if (get.count(game, "sheep") != 0) {
         game <- get.extra.animal(game, 36, "pig", 2)
         game <- get.extra.animal(game, 36, "pig", 1)
         game <- get.extra.animal(game, 36, "pig", 0)

       }

       else{
         game <- get.extra.animal(game, 27, "sheep", 0)
       }
     }
   }
   
   game <- get.small.dog(game, 12, 0)
   
   if (value.in.rabbit(game) >= 84) {
     prev.horse <- get.count(game, "horse")
     game <- buy.horse.for.animals(game, 7)
     game <- get.all(game)
     if( prev.horse == get.count(game, "horse"))
     game <- exchange.horse(game)
     
   }

   vector <- convert.game.table(game)
   vector
 }

