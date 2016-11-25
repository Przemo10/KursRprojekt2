rm(list = ls())
source("generowanie_zmiennych.R")
source("funkcje_pomocnicze.R")
source("funkcje_fazy_rzutu_kostka.R")
source("funkcje_symulujace_gre.R")
source("badaj_gre.R")
source("strategia_PDMS.R")
source("strategia_DKA.R")
source("strategia2.R")

examine.game(100,strategy1)
examine.game(100,strategy2)
examine.game(1000,strategia_DKA)

# examine.game(1000,strategia_DKA)
# # Wykonuje sie jednak zwraca taki warning
# # Warning message:
# # In `[.data.frame`(tmp_zwierzeta, 2, nr_zwierzecia) :
# #   restarting interrupted promise evaluation
#Srednia wyszla u ciebie 39 a nie 55


