#' Wartość stada w królikach
#'
#' Funkcja \code{value.in.rabbit} zwraca informację o wartości posiadanego statada.
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
#' Funkcja \code{value.in.rabbit.without.animal} zwraca informację o wartości posiadanego statada bez wskazanego zwierzęcia.
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
  
  

#' Wartość stada bez wskazanych zwierząt
#'
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
 
 
 get.small.dog <- function(game,count1, count2){
   
   if(get.count(game,"rabbit") >= count1 && get.count(game,"rabbit") >6 && get.count(game,"small_dog") == count2 ){
     
     game <-exchange.two.animals(game,"rabbit","small_dog",get.value(game,"small_dog"),1)
     
   }
 game  
}
 
 get.more.rabbits <- function(game, maxrabbit, farm = c("sheep","pig", "big_dog")){
   
     for (animal in farm){
       if(get.count(game,animal)>0 && (get.value(game,animal) + get.count(game,"rabbit")) <= maxrabbit){
         game<-  exchange.two.animals(game,animal1 = animal, animal2 = "rabbit",1,get.value(game,animal = animal))
       }
       
     }
   
   game
 }
 
 get.extra.animal <- function(game, rabbitcount, animal, animalcount ){
   
  total <- animalcount * get.value(game,animal)
    
   if(get.count(game,"rabbit") > rabbitcount && rabbitcount > total){
    game <- change.count(game,animal,animalcount)
    game <- change.count(game,"rabbit",-total)
   }
   
   game
 }
 
 buy.horse.for.animals <- function(game, rabbitcount){
   
   if(get.count(game,"rabbit") >= rabbitcount && value.in.rabbit(game) > (rabbitcount + get.value(game,"horse")) && get.count(game,"horse") == 0){

     
     game <-  change.count(game,"rabbit",value.in.rabbit.without.animal(game,"rabbit") - get.value(game,"horse"))
     game <-  change.count(game, "horse",1)
     game <-  clear.count(game, c("small_dog","sheep","pig","cow","big_dog"))
   }
   
   if(value.in.rabbit.without.animal(game,"horse") >get.value(game,"horse") && get.count(game,"horse") == 1 ){
     game <- clear.all.counts(game)
     game <- change.count(game,"horse",2)
   }
   game
 }
 
 
 stragegy_maxrabbit <- function(vector){
   
   game <- convert.farm.vector(farm = vector)
   
   game <- get.initial.rabbits(game, 12)
   game <- get.small.dog(game, 12,0)
   game <- get.more.rabbits(game,40)
   game <- get.small.dog(game,41,1)
   game <- get.extra.animal(game,40,"pig",1)
   game <- buy.horse.for.animals(game,40)
   game <- exchange.horse(game)
   
   vector <- convert.game.table(game)
   vector
 }