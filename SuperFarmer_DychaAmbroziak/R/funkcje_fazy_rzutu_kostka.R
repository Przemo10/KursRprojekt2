# Funkcja generuje faze zwiazana z rzutem jednorazowym kostka
throw.dice <- function(game, die1, die2) {
  
  result1 = sample(die1, 1)
  result2 = sample(die2, 1)
  
  game <- wolf.reaction(game,result1)
  game <- fox.reaction(game,result2)
  game <- multiply.animals(game, result1, result2)
  
  game
}

# Reakcja stada na wilka
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

# Reakcja na lisa
fox.reaction <- function(game,die2){
  if (die2 == "fox"){
    if (get.count(game, "small_dog") > 0)
      game = change.count(game, "small_dog", -1)
    else
      game = clear.count(game, "rabbit")
  }
  game
}

# Funkcja multiply.animals() dodaje zwierzęta do stada po wykonaniu rzutu kostką.
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




