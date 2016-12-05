#' Tabela gry SuperFarmer
#'
#' Zbiór danych na którym będą wykonywane strategie gry SuperFarmer.
#' Informacje dotyczące zwierząt są zawarte w wierszach tabeli.
#'
#' Przyjęta kolejność zwierząt w tabeli game:
#' \itemize{
#' \item rabbit
#' \item small_dog
#' \item sheep
#' \item pig
#' \item big_dog
#' \item cow
#' \item horse
#' }
#'
#' Poszczególne kolumny zawierają następujące informacje:
#' \itemize{
#' \item value. Wartość zwierzęcia liczona w królikach.
#' \item max.count. Maksymalnie dopuszczalna liczba zwierząt danego gatunku.
#' \item win.condition. Minimalny stan zwierząt pozwalający ukończyć grę.
#' \item count. Liczba obecnie posiadanych zwierząt.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name game
#' @examples
#' library("SuperFarmerDA")
#' game
#' @format macierz z 7-ma wierszami i 4-ma kolumnami
#'
NULL
