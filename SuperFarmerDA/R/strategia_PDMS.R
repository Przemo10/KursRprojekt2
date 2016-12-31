#' Zamiana zwierząt na króliki
#'
#' Funkcja \code{exchange.animals.for.rabbits} zamienia zwięrzęta na króliki.
#' W przypadku gdy mamy więcej niż jedno zwierze innego gatunku niż króliki zamienia "nadmiarowe" zwierzęta na króliki.
#' W przypadku gdy nie mamy królików: bierze zwierzę o największej wartości (poza koniem) jakie posiadamy i zamienia je na króliki.
#'
#' @param game Tabela gry SuperFarmer.
#' @param farm Wektor z podzbiorem stada.
#'
#' @return Tablica game po wymianie.
#'
#' @examples
#' exchange.animals.for.rabbits(game)
#' exchange.animals.for.rabbits(game, farm = c("sheep","pig"))
#'
#' @export

exchange.animals.for.rabbits <- function(game,farm = c("small_dog","sheep","pig","big_dog","cow")){
  farm
  if (get.count(game,"rabbit")>0){
    for(animal in rev(farm)){
      if(get.count(game,animal)>1 && (get.count(game,"rabbit")+get.value(game,animal) <60)){
        game<-  exchange.two.animals(game,animal1 = animal, animal2 = "rabbit",1,get.value(game,animal = animal))
      }

    }
  } else{
    for(animal in rev(farm)){
      if(get.count(game,animal)>0 & get.count(game, animal = "rabbit")==0 ){
        game <-exchange.two.animals(game,animal1 = animal, animal2 = "rabbit",1,get.value(game,animal = animal))

      }
    }



  }
  game
}


#' Zamiana królików na inne zwierzęta
#'
#' Funkcja \code{exchange.rabbits.for.animals} zamienia króliki na zwierzęta.
#'
#' @param game Tabela gry SuperFarmer.
#' @param farm Wektor z podzbiorem stada.
#'
#' @details Działanie: Jeśli nie mamy zwierzęcia a możemy go otrzymać z królików to zamieniamy.
#'
#' @return Tablica game po wymianie.
#'
#' @examples
#' exchange.animals.for.rabbits(game)
#' exchange.animals.for.rabbits(game, farm = c("sheep","pig"))
#'
#' @export

exchange.rabbits.for.animals <- function(game, farm =c("small_dog","sheep","pig", "big_dog","cow")){
  for(animal in farm){
    if(get.count(game,animal)==0 & get.value(game,animal)<get.count(game,"rabbit")){
      game <- exchange.two.animals(game,animal1 = "rabbit",animal2 = animal,animal1count = get.value(game,animal),animal2count = 1)
    }
  }
  game
}

#' Wymień konia
#'
#' Funkcja \code{exchange.horse} zamienia zwięrzęta na pozostałe zwierzęta.
#' Wymiana dokonuje się  w sytuacji posiadania więcej niż jednego konia.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#'
#' @return Tablica game po sprawdzeniu warunku wymiany drugiego konia.
#'
#' @examples
#' exchange.horse(game)
#'
#' @export
#'
exchange.horse <- function(game){

  if (get.count(game,"horse") > 1){

    game <- exchange.farm(game, c("cow","pig","sheep","rabbit"), 1)
    game <- change.count(game, "horse", -1)

  }
  game
}
#' Zdobądź konia
#'
#' Funkcja \code{get.horse} sprawdza czy możemy już otrzymać konia.
#'
#' @param game Tabela game zawierająca informację dotyczącego posiadanego stada w chwili obecnej.
#'
#' @return Tabela game po sprawdzeniu warunku otrzymania konia.
#'
#'
#'
#' @examples
#' get.horse(game)
#' get.horse(game)
#'
#' @export
get.horse <- function(game){

  if(get.count(game,"rabbit")>12 & get.count(game,"small_dog")>0 & get.count(game,"sheep")>0& get.count(game,"pig")>0  &get.count(game,"big_dog")>0){
    game <-exchange.farm(game, farm = c("small_dog","sheep","pig", "big_dog"),count = -1)
    game <- change.count(game, animal = "horse",1)
    game <- change.count(game, animal = "rabbit",-12)
  }
  if(get.count(game,"cow")>0 & get.count(game,"big_dog") >0){
    game <-exchange.farm(game, farm = c("cow","big_dog"),count = -1)
    game <- change.count(game, animal = "horse",1)
  }

  game
}
#' Strategia PDMS
#'
#' Strategia z pierwszej fazy używana przez grupę złożoną z Przemysława Dychy i Mateusza Siwca.
#' Jest strategią wiele na wiele.
#' Funkcja \code{strategia_PDMS}
#' @param vector Wektor z liczebnością stada.
#'
#' @return Wektor z zadaną liczebnością stada po wykonaniu strategii.
#'
#' @details Strategia jest strategią wiele na wiele. Składa się z następujących etapów:
#' \itemize{
#'  \item Konwersji wektora do tabeli gra - możliwość uruchomienia  strategii zewnętrznych w naszym pakiecie.
#'  \item Zamiany nadmiarowych zwierząt na króliki.
#'  \item Wymiany nadmiarowych królików na brakujące zwierzęta.
#'  \item Wymiany zwierząt na konia.
#'  \item Wymiany nadmiarowego konia na zwierzęta - zakończenie gry.
#'  \item Konwersji do wektora - możliwość uruchomienia w innych pakietach.
#' }
#'
#' @author
#' Przemysław Dycha, Mateusz Siwiec
#' @examples
#' strategia_PDMS(c(8,1,3,0,0,0,1))
#'
#' @export
#'
strategia_PDMS <- function(vector){

  game <- convert.farm.vector(farm = vector)

  game <- exchange.animals.for.rabbits(game)
  game <- exchange.rabbits.for.animals(game)
  game <- get.horse(game)
  game <- exchange.horse(game)

  vector <- convert.game.table(game)
  vector
}
