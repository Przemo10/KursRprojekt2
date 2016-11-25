# Strategia2 - dla testów wyrzuciłem jedną funkcję cofania w tył z exchange.animals
# Pozostałe funkcje są takie same jak wczesniej - plik_strategia_PDMS.R
# Ruchow wyszlo 42
# Jednak ona jest nieoptymalna .Trzymamy więcej niż jedną świnię i owcę i nic z niej nie robimy.
# Wszystkie wymiany robimy królikami. Jednak taka struktura gwarantuje ze krolik zostanie zamieniony tylko raz na wyzsze zwierzę.

exchange.animals2 <- function(game){
  
  game<- exchange.horse(game)
  game <- get.horse(game)  
  game <- exchange.rabbits.for.animals(game)
  
  
  
  game
}

strategy2 <- function(vector){
  game<- convert.farm.vector(farm = vector)
  #nazwa strategii
  game <- exchange.animals2(game)
  vector <- convert.game.table(game)
  vector
}
