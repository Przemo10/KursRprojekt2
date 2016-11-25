# examine.game() funkcja wywołująca strategię i liczbę rund ile chcemy powtórzyć
examine.game <- function(rounds, namestrategy){
  results <- 1:rounds
  for (i in 1:rounds) {
    results[i] = play(namestrategy)
  }
  summary(results)
}

