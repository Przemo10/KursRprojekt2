## Plik zawiera funkcje pomocnicze mogą się przydać zarówno do strategii jak i rzutu kostką

# Funkcja change.count() zmienia nam liczbę danego zwierzęcia w stadzie.
change.count <- function(game, animal, change) {
  
  change = change + game[animal, "count"]
  change = max(c(0, change))
  change = min(c(change, game[animal,"max.count"]))
  game[animal, "count"] = change
  game
}


# Funkcja exchange.farm() zmienia liczebność wskazanego podzbioru stada
# Skraca zapis kodu w sytuacji gdy potrzebujemy użyć kilkakrotnie funkcji change.count()
exchange.farm <- function(game, farm, count){
  for (animal in farm){
    game<-change.count(game,animal, count)
    
  }
  game
}


# Funkcja get.valule() zwraca wartość wymiany królików na inne zwierzęta.
get.value <- function(game, animal) {
  
  game[animal, "value"]
}

# Funkcja get.count() zwraca liczbę zwierząt danego gatunku, którą posiadamy w stadzie.
get.count <- function(game, animal) {
  
  game[animal, "count"]
}

# Funkcja clear.count() zeruje liczbę zwierząt danego gatunku.
clear.count <- function(game, animal) {
  
  change.count(game, animal, -game[animal, "max.count"])
  game
}

# Funkcja game.finished() sprawdza, czy warunek końca rozgrywki jest spełniony.
game.finished <- function(game) {
  
  all(game[,"count"] >= game[,"win.condition"])
}

# Funkcja Funkcja exchange.two.animals zamienia zwierzę 1 na zwierzę nr 2.

exchange.two.animals <- function(game, animal1, animal2, animal1count, animal2count ){
  game<- change.count(game,animal1, -animal1count)
  game<- change.count(game,animal2, animal2count)
  game
}

# Funkcja convert.farm.vector() konwertuje wektor stada na konwersje zgodną z table game
convert.farm.vector <- function(farm){
  game[,"count"] <- c( farm[1],farm[6],farm[2:3],farm[7],farm[4:5])
  game
}

# Funkcja convert.game.table() wykonuje operację odwrotną do convert.farm.vector()
convert.game.table <- function(game){ 
  farm <- game[c("rabbit","sheep","pig","cow","horse","small_dog","big_dog"),"count"]
  farm
}
