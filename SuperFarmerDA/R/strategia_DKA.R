#' Strategia DKA
#'
#' Strategia używana w pierwszym etapie przez grupę, w której skład wchodzili:
#' Dominik Ambroziak, Anna Dymowska, Adam Król.
#' @param stan_zagrody Wektor zawierający informacje dotyczące posiadanego stada w chwili obecnej.
#'
#' @return Wektor liczebności stada po wykonaniu wymiany zgodnie z założeniami strategi.
#'
#' @details Podstawowe założenia startegii:
#' \itemize{
#'  \item Gdy jesteśmy w stanie wymienić tańsze zwierzęta na droższe - wymieniamy
#'  \item W drugą stronę wymieniamy tylko wtedy gdy jest to ruch kończący grę.
#'  \item Dużego psa kupujemy przed krową.
#'  \item Nie kupujemy małych psów.
#'  \item Staramy się zawsze mieć przynajmniej jednego królika. Nie dokonujemy
#'  wymian, które powodowałyby wyzerowanie stanu posiadanych królików.
#' }
#'
#' Czynności wykonywane przez funkcję strategy_DKA:
#' \itemize{
#'  \item Konwersji wektora do tabeli "zwierzeta"
#'  \item Jeśli posiadane zwierzęta mają wartość wystarczającą do wygrania,
#'   a nie mamy jeszcze każdego z wymaganych zwierząt to wymieniamy tak, by mieć już wszystkie
#'  \item W przeciwnym przypadku dokonujemy zamiany tańszych zwierząt na droższe, w miarę możliwości.
#'  \item Konwersji do wektora - możliwość uruchomienia w innych pakietach.
#' }
#' @author
#' Dominik Ambroziak, Anna Dymowska, Adam Król
#'
#' @examples
#' strategy_DKA(c(8,1,3,0,0,0,1))
#'
#' @export
#'

strategy_DKA <- function(stan_zagrody){
  zwierzeta <- data.frame(
      krolik=c(stan_zagrody[1], 1, 60-stan_zagrody[1]),
      owca=c(stan_zagrody[2], 6, 24-stan_zagrody[2]),
      swinia=c(stan_zagrody[3], 12, 20-stan_zagrody[3]),
      krowa=c(stan_zagrody[4], 36, 12-stan_zagrody[4]),
      duzy_pies=c(stan_zagrody[7], 36, 2-stan_zagrody[7]),
      kon=c(stan_zagrody[5], 72, 6-stan_zagrody[5]),
      maly_pies=c(stan_zagrody[6],6,4-stan_zagrody[6]),
      row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie")
      )
  if (!prod(zwierzeta[1,] == wymiana_na_tansze(zwierzeta)[1,])) #czy wymiana na tansze zachodzi
    zwierzeta <- wymiana_na_tansze(zwierzeta) else
    zwierzeta <- wymiana_na_drozsze(zwierzeta)

  stan_zagrody <- c(zwierzeta[1,1], zwierzeta[1,2], zwierzeta[1,3], zwierzeta[1,4], zwierzeta[1,6], zwierzeta[1,7], zwierzeta[1,5])
  names(stan_zagrody) <-
    c("krolik",
      "owca",
      "swinia",
      "krowa",
      "kon",
      "maly_pies",
      "duzy_pies")
  return(stan_zagrody)
}

#' Wymiana na tańsze
#'
#' Funkcja ta zamienia jedno zwierzę na kilka tańszych zwierząt. Jest wywoływana
#' w funkcji strategy_DKA tylko wtedy, gdy wartość posiadanych zwierząt jest
#' wystarczająca do otrzymania wszystkich zwierząt wymaganych do zakończenia gry.
#'
#' @param tmp_zwierzeta tabela zawierająca liczby posiadanych zwierząt,
#' liczby zwierząt pozostałych w stadzie oraz wartości zwierząt w przeliczeniu
#'  na króliki (dokładna postać tabeli w przykładzie).
#'
#' @return Tabela z uaktualnionymi wartościami posiadanych zwierząt.
#'
#' @examples
#' zwierzeta <- data.frame(
#' krolik=c(1, 1, 59),
#' owca=c(0, 6, 24),
#' swinia=c(0, 12, 20),
#' krowa=c(1, 36, 11),
#' duzy_pies=c(1, 36, 1),
#' kon=c(2, 72, 4),
#' maly_pies=c(0,6,4),
#' row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))
#' zwierzeta
#' wymiana_na_tansze(zwierzeta)
#'
#' @export
#'

wymiana_na_tansze<-function(tmp_zwierzeta){
  for (i in 6:2){   #przechodzimy kolejno od konia do owiec
    if (tmp_zwierzeta[1,i]>1){   #sprawdzamy, czy mamy wi?cej ni? jedno
      if (prod(tmp_zwierzeta[1,i:6])>0){   #je?li mamy wszystkie dro?sze zwierz?ta
        tmp_zwierzeta<-oddaj_do_stada(nr_zwierzecia=i, liczba=1, tmp_zwierzeta)
        for (j in (i-1):1){
          tmp_zwierzeta<-dodaj_do_zagrody(nr_zwierzecia=j, liczba=1, tmp_zwierzeta)
        }
        break
      }
    }
  }
  return(tmp_zwierzeta)
}

#' Oddawanie zwierząt do stada
#'
#' Funkcja ta odejmuje określoną liczbę danego zwierzęcia z aktualnego stanu naszej
#'  zagrody i dodaje tę liczbę do stanu stada.
#'
#' @param nr_zwierzecia numer odpowiadający zwierzęciu, które chcemy oddać do stada
#' @param liczba ile zwierząt ma być oddanych
#' @param tmp_zwierzeta tabela zawierająca liczby posiadanych zwierząt,
#' liczby zwierząt pozostałych w stadzie oraz wartości zwierząt w przeliczeniu
#'  na króliki (doładna postać tabeli w przykładzie).
#'
#' @return Tabela z uaktualnionymi wartościami posiadanych zwierząt.
#'
#' @examples
#' zwierzeta <- data.frame(
#' krolik=c(1, 1, 59),
#' owca=c(0, 6, 24),
#' swinia=c(0, 12, 20),
#' krowa=c(1, 36, 11),
#' duzy_pies=c(1, 36, 1),
#' kon=c(2, 72, 4),
#' maly_pies=c(0,6,4),
#' row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))
#' zwierzeta
#' oddaj_do_stada(1, 1, zwierzeta)
#'
#' @export
#'

oddaj_do_stada <- function(nr_zwierzecia, liczba, tmp_zwierzeta){
  a<-min(liczba, tmp_zwierzeta[1,nr_zwierzecia])
  tmp_zwierzeta[1,nr_zwierzecia]=tmp_zwierzeta[1,nr_zwierzecia]-a
  tmp_zwierzeta[3,nr_zwierzecia]=tmp_zwierzeta[3,nr_zwierzecia]+a

  return(tmp_zwierzeta)
}

#' Dodawanie zwierząt do zagrody
#'
#' Funkcja ta dodaje określoną liczbę danego zwierzęcia do aktualnego stanu naszej
#'  zagrody i odejmuje tę liczbę od stanu stada.
#'
#' @param nr_zwierzecia numer odpowiadający zwierzęciu, które chcemy dodać do zagrody
#' @param liczba ile zwierząt ma być dodanych
#' @param tmp_zwierzeta tabela zawierająca liczby posiadanych zwierząt,
#' liczby zwierząt pozostałych w stadzie oraz wartości zwierząt w przeliczeniu
#'  na króliki (doładna postać tabeli w przykładzie).
#'
#' @return Tabela z uaktualnionymi wartościami posiadanych zwierząt.
#'
#' @examples
#' zwierzeta <- data.frame(
#' krolik=c(1, 1, 59),
#' owca=c(0, 6, 24),
#' swinia=c(0, 12, 20),
#' krowa=c(1, 36, 11),
#' duzy_pies=c(1, 36, 1),
#' kon=c(2, 72, 4),
#' maly_pies=c(0,6,4),
#' row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))
#' zwierzeta
#' dodaj_do_zagrody(2, 1, zwierzeta)
#'
#' @export
#'

dodaj_do_zagrody<-function(nr_zwierzecia, liczba, tmp_zwierzeta){
  a<-min(liczba, tmp_zwierzeta[3,nr_zwierzecia])
  tmp_zwierzeta[1,nr_zwierzecia]=tmp_zwierzeta[1,nr_zwierzecia]+a
  tmp_zwierzeta[3,nr_zwierzecia]=tmp_zwierzeta[3,nr_zwierzecia]-a

  return(tmp_zwierzeta)
}

#' Czy stać nas
#'
#' Funkcja \code{czy_stac_nas} sprawdza, czy jest wystarczająco dużo tańszych
#' zwierząt, by wymienić je na dane zwierzę.
#'
#' @param nr_zwierzecia numer odpowiadający zwierzęciu, dla którego chcemy
#' sprawdzić czy możliwa jest wymiana.
#' @param tmp_zwierzeta tabela zawierająca liczby posiadanych zwierząt,
#' liczby zwierząt pozostałych w stadzie oraz wartości zwierząt w przeliczeniu
#'  na króliki (doładna postać tabeli w przykładzie).
#'
#' @return TRUE jeśli możliwa jest wymiana, FALSE w przeciwnym przypadku.
#'
#' @examples
#' zwierzeta <- data.frame(
#' krolik=c(1, 1, 59),
#' owca=c(0, 6, 24),
#' swinia=c(0, 12, 20),
#' krowa=c(1, 36, 11),
#' duzy_pies=c(1, 36, 1),
#' kon=c(2, 72, 4),
#' maly_pies=c(0,6,4),
#' row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))
#' zwierzeta
#' czy_stac_nas(6, zwierzeta)
#'
#' @export
#'

czy_stac_nas<-function(nr_zwierzecia, tmp_zwierzeta){
  wartosc_tanszych<-sum(tmp_zwierzeta[1,1:(nr_zwierzecia-1)]*tmp_zwierzeta[2,1:(nr_zwierzecia-1)])
  if (tmp_zwierzeta[1,1]>0){
    wartosc_tanszych<-wartosc_tanszych-1  #jednego kr?lika chcemy zachowywa?,
  }
  czy_stac<-wartosc_tanszych>=tmp_zwierzeta[2,nr_zwierzecia]
  return(czy_stac)
}

#' Wymiana na droższe
#'
#' Funkcja ta zamienia posiadane zwierzęta na jedno, możliwie jak największe, zwierzę.
#'
#' @param tmp_zwierzeta tabela zawierająca liczby posiadanych zwierząt,
#' liczby zwierząt pozostałych w stadzie oraz wartości zwierząt w przeliczeniu
#'  na króliki (dokładna postać tabeli w przykładzie).
#'
#' @details Rozpoczynając od najdroższego zwierzęcia sprawdzane jest, czy można
#' je otrzymać wymieniając tańsze zwierzęta. Dokonuje się tylko jedna wymiana. Wymiana nie dokona się, jeśli
#'  powodowałaby wyzerowanie stanu królików.
#'
#' @return Tabela z uaktualnionymi wartościami posiadanych zwierząt.
#'
#' @examples
#' zwierzeta <- data.frame(
#' krolik=c(7, 1, 53),
#' owca=c(1, 6, 23),
#' swinia=c(0, 12, 20),
#' krowa=c(0, 36, 12),
#' duzy_pies=c(0, 36, 2),
#' kon=c(0, 72, 6),
#' maly_pies=c(0,6,4),
#' row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))
#' zwierzeta
#' wymiana_na_drozsze(zwierzeta)
#'
#' @export
#'

wymiana_na_drozsze<-function(tmp_zwierzeta){
  for (i in 6:2){

    if (tmp_zwierzeta[1,i]==0){         #sprawdzamy, czy mamy i-te zwierze
      if(czy_stac_nas(nr_zwierzecia=i,tmp_zwierzeta)){
        tmp_zwierzeta<-dodaj_do_zagrody(nr_zwierzecia=i, liczba=1, tmp_zwierzeta)
        do_splaty<-tmp_zwierzeta[2,i]
        j<-(i-1)
        while(do_splaty>0){
          if (tmp_zwierzeta[1,j]>=1){   #sprzedajemy zwierz?ta kolejno od najdro?szych

            do_splaty<-(do_splaty-tmp_zwierzeta[2,j]) #sp?acamy warto?? (po jednym zwierz?ciu)
            tmp_zwierzeta<-oddaj_do_stada(nr_zwierzecia=j, liczba=1, tmp_zwierzeta)
          } else j<-(j-1)
        }
        break
      }
    }
  }
  tmp_zwierzeta
}
