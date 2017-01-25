#' @title Wizytówka
#' @description Funkcja tworząca wizualne porównanie wydajności badanej strategii ze strategią strategia_maxrabbit.
#' W otrzymanym pliku pdf znajduje się plansza z nałożonymi na siebie wykresami rozkładów długości gry dla obu strategii,  plansza z wykresami słupkowymi obrazującymi pierwsze i ostatnie decyle tych rozkładów oraz tabela z ich najważniejszymi parametrami.
#'
#' @param strategia - funkcja zawierająca strategię, którą chcemy zbadać
#' @param nazwa_strategii - argument tekstowy, nazwa badanej strategii
#' @param ile_powtorzen - argument liczbowy mówiący ile razy ma być przeprowadzona symulacja gry
#' @return Ramka obiektów graficznych z wizualnym porównaniem wydajności badanej strategii obok strategii strategy_maxrabbit
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Dominik Ambroziak, \email{ambroziak.d2@gmail.com}
#'
#' @export

wizytowka_PA <- function(strategia , nazwa_strategii, ile_powtorzen = 10000){
  
  #generowanie kolorow
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  kolor = gg_color_hue(2)
  
  
  # funkcja badaj_gre zwraca liste z trzema elementami,
  # pierwszym jest wektor z rozkladem czasu
  
  
  rozklad_lista <- SuperFarmerDA::badaj_gre(strategia, ile_powtorzen)
  rozklad <- rozklad_lista[[1]]
  
  rozklad_najl_lista <- SuperFarmerDA::badaj_gre(strategia_maxrabbit, ile_powtorzen)
  rozklad_najl <- rozklad_najl_lista[[1]]
  
  
  
  # dwie gestosci + mediany
  wykres1 <- ggplot2::ggplot(as.data.frame(rozklad_najl), ggplot2::aes(rozklad_najl)) +
    ggplot2::geom_density(colour=kolor[1], fill = kolor[1], alpha = 0.3, bw = 4) +
    ggplot2::geom_density( ggplot2::aes(rozklad), colour=kolor[2], fill = kolor[2], alpha = 0.3, bw = 4) +
    ggplot2::ylim(0, 0.03) + ggplot2::xlab("liczba tur") + ggplot2::ylab(NULL) +
    ggplot2::geom_vline(xintercept = median(rozklad_najl), col = kolor[1], size = 1) +
    ggplot2::geom_vline(xintercept = median(rozklad), col = kolor[2], size = 1) +
    ggplot2::ggtitle("Rozkłady czasu działania strategii + mediany") + ggplot2::xlim(0, 200)
  
  
  #decyle
  decyle <- data.frame(Liczba_Tur=c(quantile(rozklad_najl, probs=c(0.1, 0.9)),
                                    quantile(rozklad, probs=c(0.1, 0.9))))
  decyle$decyle <- c(" pierwszy", "dziewiąty", " pierwszy", "dziewiąty")
  decyle$strategia <- c(" strategia_maxrabbit", " strategia_maxrabbit",
                        nazwa_strategii, nazwa_strategii )
  
  wykres2 <- ggplot2::ggplot(decyle, ggplot2::aes( x = decyle, y = Liczba_Tur, fill = strategia)) +
    ggplot2::ylim(0,150)+
    ggplot2::geom_col(ggplot2::aes(fill = strategia), position = "dodge", width = 0.5 ) + 
    ggplot2::ylab("liczba tur") + ggplot2::ggtitle("Wybrane decyle") +
    ggplot2::geom_text( ggplot2::aes( label = Liczba_Tur, Liczba_Tur = Liczba_Tur +1),
               position = ggplot2::position_dodge(0.5), vjust = 0)
  
  #tekst
  
  text1 <- grid::textGrob( paste(nazwa_strategii, "- wizytówka"), gp=grid::gpar(fontsize=20))
  text2 <- grid::textGrob( paste( "Statystyki strategii", nazwa_strategii, 
                            "są porównywane ze statystykami strategii strategia_maxrabbit." ))
  
  
  
  #tabelka
  
  tabelka <- as.data.frame(t(cbind(summary(rozklad_najl), summary(rozklad))))
  tabelka$Strategia <- c("strategia_maxrabbit", nazwa_strategii)
  tabelka <- tabelka[, c(7, 1:6)]
  
  t <- gridExtra::ttheme_minimal(core=list(bg_params = list(fill = kolor, col=NA)))
  
  tabelka <- gridExtra::tableGrob(tabelka, rows = NULL, theme = t)
  
  #Razem. Wykresy, tekst i tabelka na jednej stronie.
  
  
  lay <- rbind(c(1,1,1,1,1),
               c(5,5,5,5,5),
               c(3,3,3,2,2),
               c(3,3,3,2,2),
               c(3,3,3,2,2),
               c(3,3,3,2,2),
               c(3,3,3,2,2),
               c(3,3,3,2,2),
               c(4,4,4,4,4),
               c(4,4,4,4,4))
  gridExtra::grid.arrange(text1, wykres2, wykres1, tabelka, text2, layout_matrix = lay,
               bottom = "wykonali: Katarzyna Pyrtek, Dominik Ambroziak")
  
}