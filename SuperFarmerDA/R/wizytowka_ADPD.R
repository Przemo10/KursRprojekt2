#' Wizytówka
#'
#' Funkcja \code{wizytowka_ADPD} tworzy wizytówkę dla strategii.
#' Funkcja automatycznie wczytuje nazwę strategii zgodną ze wzorcem strategia_
#' 
#' @param strategia Tabela gry SuperFarmer.
#' @param N Liczba powtórzeń dla których powinna być wykonana.
#'
#' @return Plik pdf z wynikami strategii.
#'  
#' @examples 
#' wizytowka_ADPD(strategia_DKA,10)
#' 
#' @export


wizytowka_ADPD <- function( strategia,  N = 10000){
  
  
  wynik_strategii_max <- SuperFarmerDA::badaj_gre( SuperFarmerDA::strategia_maxrabbit , N )[[1]]
  wynik_strategii_badanej <- SuperFarmerDA::badaj_gre( strategia, N )[[1]]
  wynik_strategii_yolo <- SuperFarmerDA::badaj_gre( SuperFarmerRCNK::strategia_yolo, N )[[1]]

  strategia_max<- "maxrabbit"
  strategia_min <- "yolo"
  strategia_porownawcza <- deparse(substitute(strategia))
  strategia_porownawcza <- substr(strategia_porownawcza,nchar("strategia_")+regexpr("strategia_",strategia_porownawcza)[[1]],nchar(strategia_porownawcza))

  

  czasy_gier <- data.frame(wynik_strategii_yolo,wynik_strategii_max,wynik_strategii_badanej)
  colnames(czasy_gier) <- c(  strategia_min,strategia_max, strategia_porownawcza)



  dane_do_wykresow <- reshape2::melt(czasy_gier)

### WYKRES DYSTRYBUANTY
  wykres_dystrybuanty <-
    ggplot2::ggplot(dane_do_wykresow, ggplot2::aes(x = value)) +
    ggplot2::stat_ecdf(ggplot2::aes(colour = variable)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      list(
        colour = "Nazwa strategii",
        x = "Czas trwania gry (liczba rund)",
        y = "P(X<t)",
        title = "Dystrybuanty czasu gry"
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 260)) +
    ggplot2::theme(legend.position = "none")
  
### WYKRES SKRZYPCOWY 
  wykres_pudelkowy <-
    ggplot2::ggplot(dane_do_wykresow, ggplot2::aes(variable, value)) +
    ggplot2::geom_violin(ggplot2::aes(fill = variable)) +
    ggplot2::geom_boxplot(width = 0.15) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 260)) +
    ggplot2::labs(
      list(
        fill = "",
        x = "",
        y = "Czas trwania gry (liczba rund)",
        title = "Wykres skrzypcowo-pudełkowy"
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position = "none") 

### DANE DO TABELI
  library(dplyr)
  statystyki_strategii <-
    dane_do_wykresow %>% dplyr::group_by(variable) %>% dplyr::summarise(
      min = min(value),
      '10proc'= round(quantile(value,prob =0.1)),
      '30proc'= round(quantile(value,prob =0.1)),
      mediana = median(value),
      srednia = round(mean(value), 2),
      '90proc'= round(quantile(value,prob =0.9)),
      max = max(value)
    )

  colnames(statystyki_strategii)[1] <- "strategia"


### PRZEDZIAŁY CZASU GRY
  dyskretne_czasy_gier <- apply(czasy_gier, 1:2, function(x)
    cut(
      x,
      breaks = c(0, 15, 30, 45, 60, 75, 90,105,120, Inf),
      labels = c(
        " <0,15>",
        " <16,30>",
        " <31,45>",
        " <46-60>",
        " <61-75>",
        " <76-90>",
        " <91-105>",
        "<106-120>",
        "ponad120"
      )
    )) 
  
  
  d1 <- reshape2::melt(dyskretne_czasy_gier)[-1]
  d2 <- data.frame(prop.table(table(d1$Var2,d1$value),1))
  d2 <- as.data.frame(d2)
  
  

### WYKRES PRZEDZIAŁY 
  wykres_przedzialy <- ggplot2::ggplot(d2, ggplot2::aes(Var2,Freq,fill = Var1)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::scale_y_continuous(expand = c(0,0), labels = scales::percent ) + 
    ggplot2::theme(legend.position="none") + 
    ggplot2::labs(list(x="Czas trwania gry (liczba rund)", y = "Procent w przedziale", title="Porównanie wyników strategii dla przedziałów czasu gry")) + 
    ggplot2::theme_bw() + ggplot2::theme(legend.position="none") 
  

### UKŁAD NA STRONIE  
  t1 <- gridExtra::ttheme_default(core=list(
    bg_params = list(fill=c(rep(c("#F8766D","#5CF058", "#619CFF"))),
                     alpha = rep(c(1,0.5), each=3))
  ))
  

  wykres_tabela <- gridExtra::tableGrob(statystyki_strategii, rows = NULL, theme = t1)
 
  autorzy <- paste("Autorzy:", "Anna Dymowska" , "Przemyslaw Dycha", sep = "\n")
  autorzy <- grid::grid.text(autorzy, gp = grid::gpar(fontsize = 10), x = grid::unit(0.05, "npc"), just = "left" ) 
  
  tytul_plakatu <-  paste0("Strategia_", strategia_porownawcza, " - wizytówka")
  tytul <- grid::grid.text(tytul_plakatu, gp = grid::gpar(fontsize = 25, col = 'cornflowerblue', fontface = 'bold'),  vjust = 0.5, hjust = 0.6)
  
  opis_slowny <- paste0("Przedstawiamy statystyki strategii ", strategia_porownawcza ,
                        " - strategii gry SuperFarmer dla jednego gracza z liczbą powtórzeń ",N,". Dla porównania tej strategii na wykresach zamieściliśmy strategie: max_rabbit oraz yolo - najszybszą i najwolniejszą strategię znalezioną przez nas wśród strategii, które przygotowali studenci na zajęcia z Programowania w R i wizualizacji danych w roku 2016/17.")
  
  opis_slowny <- RGraphics::splitTextGrob(opis_slowny, gp = grid::gpar(fontsize = 12))
  

  gg2<-gridExtra::grid.arrange(
    autorzy,
    tytul,
    wykres_pudelkowy,
    opis_slowny,
    wykres_tabela,
    wykres_dystrybuanty,
    wykres_przedzialy,
    ncol = 2,
    layout_matrix = rbind(
                          c(1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3),
                          c(1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3),
                          
                          
                          c(NA,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3),
                          c(NA,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3),
                          c(NA,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3),
                          
                         
                          c(5,5,5,5,5,5,5,5,5,5,5,5,3,3,3,3,3,3,3,3,3,3,3,3),
                          c(5,5,5,5,5,5,5,5,5,5,5,5,3,3,3,3,3,3,3,3,3,3,3,3),
                          c(5,5,5,5,5,5,5,5,5,5,5,5,3,3,3,3,3,3,3,3,3,3,3,3),
                        
                          
                          c(6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7),
                          c(6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7),
                          c(6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7),
                          c(6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7),
                          c(6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7),
                          c(6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7),
                          c(6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7),
                          c(6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7))
    
    
  )

### zapis pliku  
  
 nazwa_pliku_pdf <- paste0("Strategia_",strategia_porownawcza, "_wizytowka.pdf")

 ggplot2::ggsave(nazwa_pliku_pdf,plot = gg2, width = 29.7, height = 21, units = "cm")

}



