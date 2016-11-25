exchange.two.animals <- function(game, animal1, animal2, animal1count, animal2count ){
  game<- change.count(game,animal1, -animal1count)
  game<- change.count(game,animal2, animal2count)
  game
}



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

exchange.horse <- function(game){
  
  if (get.count(game,"horse") > 1){
    
    game <- exchange.farm(game, c("cow","pig","sheep","rabbit"), 1)
    game <- change.count(game, "horse", -1)
    
  } 
  game
}


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



exchange.rabbits.for.animals <- function(game, farm =c("small_dog","sheep","pig", "big_dog","cow")){
  for(animal in farm){
    if(get.count(game,animal)==0 & get.value(game,animal)<get.count(game,"rabbit")){
      game <- exchange.two.animals(game,animal1 = "rabbit",animal2 = animal,animal1count = get.value(game,animal),animal2count = 1)
    }
  }
  game
}


exchange.animals <- function(game){
  
  
  game <- exchange.animals.for.rabbits(game)
  game <- exchange.rabbits.for.animals(game)
  game <- get.horse(game)  
  game<- exchange.horse(game)
  
  game
}

# Strategia w postaci skonwertowanej
strategy1 <- function(vector){
  game<- convert.farm.vector(farm = vector)
  #nazwa strategii
  game <- exchange.animals(game)
  vector <- convert.game.table(game)
  vector
}
