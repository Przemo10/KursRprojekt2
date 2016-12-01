#' Strategia DKA
#'
#' Funkcja \code{strategy.test} 
#' @param stan_zagrody Wektor zawierający informację dotyczącego posiadanego stada w chwili obecnej.
#' 
#' @return Wektor liczebności stada po wykonaniu strategi.
#'
#' @details Strategia jest strategią jeden  na wiele..
#' \itemize{
#'  \item Konwersji wektora do tabeli gra - możliwość uruchomienia  strategii zewnętrznych w naszym pakiecie.
#'  \item Dopisać co robi
#'  \item Konwersji do wektora - możliwość uruchomienia w innych pakietach.
#' }
#' @author
#' Dominik Ambroziak, dopisz autorów z 1 fazy
#'  
#' @examples
#' strategia.DKA(c(8,1,3,0,0,0,1))
#'
#' @export
#'
## Aby to działało konieczne jest przekazanie tabeli zwierzęta jako zmiennej globalnej może warto ją przekazać jako zbior danych pakietu ?
strategia.DKA <- function(stan_zagrody){
  zwierzeta <<- data.frame(
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

oddaj_do_stada <- function(nr_zwierzecia, liczba, tmp_zwierzeta){
  a<-min(liczba, tmp_zwierzeta[1,nr_zwierzecia])
  tmp_zwierzeta[1,nr_zwierzecia]=tmp_zwierzeta[1,nr_zwierzecia]-a
  tmp_zwierzeta[3,nr_zwierzecia]=tmp_zwierzeta[3,nr_zwierzecia]+a
  
  return(tmp_zwierzeta)
}

dodaj_do_zagrody<-function(nr_zwierzecia, liczba, tmp_zwierzeta){
  a<-min(liczba, tmp_zwierzeta[3,nr_zwierzecia])
  tmp_zwierzeta[1,nr_zwierzecia]=tmp_zwierzeta[1,nr_zwierzecia]+a
  tmp_zwierzeta[3,nr_zwierzecia]=tmp_zwierzeta[3,nr_zwierzecia]-a
  
  return(tmp_zwierzeta)
}

czy_stac_nas<-function(nr_zwierzecia, tmp_zwierzeta){ 
  wartosc_tanszych<-sum(tmp_zwierzeta[1,1:(nr_zwierzecia-1)]*tmp_zwierzeta[2,1:(nr_zwierzecia-1)])
  if (tmp_zwierzeta[1,1]>0){ 
    wartosc_tanszych<-wartosc_tanszych-1  #jednego kr?lika chcemy zachowywa?, 
  }
  czy_stac<-wartosc_tanszych>=tmp_zwierzeta[2,nr_zwierzecia]
  return(czy_stac)
}

wymiana_na_drozsze<-function(tmp_zwierzeta){
  for (i in 6:2){
    
    if (tmp_zwierzeta[1,i]==0){         #sprawdzamy, czy mamy i-te zwierze
      if(czy_stac_nas(nr_zwierzecia=i,tmp_zwierzeta)){
        tmp_zwierzeta<-dodaj_do_zagrody(nr_zwierzecia=i, liczba=1, tmp_zwierzeta)
        do_splaty<-zwierzeta[2,i]
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