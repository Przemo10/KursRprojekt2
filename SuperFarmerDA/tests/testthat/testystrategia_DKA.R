
## plik strategia_DKA.R

test_that("strategy_DKA zwraca wektor",{
  expect_true(is.vector(strategy_DKA(c(7, 0, 0, 0, 0, 0, 0))))
})

test_that("strategy_DKA dokonuje wymiany",{
  expect_equal(unname(strategy_DKA(c(7, 0, 0, 0, 0, 0, 0)))[2], 1)
  })

test_that("wymiana_na_tansze zwraca ramke danych",{
  zwierzeta <- data.frame(
    krolik=c(1, 1, 59),
    owca=c(0, 6, 24),
    swinia=c(0, 12, 20),
    krowa=c(1, 36, 11),
    duzy_pies=c(1, 36, 1),
    kon=c(2, 72, 4),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))

  expect_true(is.data.frame(wymiana_na_tansze(zwierzeta)))
})

test_that("wymiana_na_tansze dokonuje wymiany",{
  zwierzeta <- data.frame(
    krolik=c(1, 1, 59),
    owca=c(0, 6, 24),
    swinia=c(0, 12, 20),
    krowa=c(1, 36, 11),
    duzy_pies=c(1, 36, 1),
    kon=c(2, 72, 4),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))

  expect_equal(wymiana_na_tansze(zwierzeta)[1,2],1)
})

test_that("oddaj_do_stada zwraca ramke danych",{
  zwierzeta <- data.frame(
    krolik=c(1, 1, 59),
    owca=c(0, 6, 24),
    swinia=c(0, 12, 20),
    krowa=c(1, 36, 11),
    duzy_pies=c(1, 36, 1),
    kon=c(2, 72, 4),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))

  expect_true(is.data.frame(oddaj_do_stada(6, 2, zwierzeta)))
})

test_that("oddaj_do_stada oddaje do stada",{
  zwierzeta <- data.frame(
    krolik=c(1, 1, 59),
    owca=c(0, 6, 24),
    swinia=c(0, 12, 20),
    krowa=c(1, 36, 11),
    duzy_pies=c(1, 36, 1),
    kon=c(2, 72, 4),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))

  expect_equal(oddaj_do_stada(6, 2, zwierzeta)[1,6],0)
})

test_that("dodaj_do_zagrody zwraca ramke danych",{
  zwierzeta <- data.frame(
    krolik=c(1, 1, 59),
    owca=c(0, 6, 24),
    swinia=c(0, 12, 20),
    krowa=c(1, 36, 11),
    duzy_pies=c(1, 36, 1),
    kon=c(2, 72, 4),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))

  expect_true(is.data.frame(dodaj_do_zagrody(6, 2, zwierzeta)))
})

test_that("dodaj_do_zagrody dodaje do zagrody",{
  zwierzeta <- data.frame(
    krolik=c(1, 1, 59),
    owca=c(0, 6, 24),
    swinia=c(0, 12, 20),
    krowa=c(1, 36, 11),
    duzy_pies=c(1, 36, 1),
    kon=c(2, 72, 4),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))

  expect_equal(dodaj_do_zagrody(6, 2, zwierzeta)[1,6], 4)
})

test_that("czy_stac_nas zwraca TRUE",{
  zwierzeta <- data.frame(
    krolik=c(7, 1, 53),
    owca=c(1, 6, 23),
    swinia=c(0, 12, 20),
    krowa=c(0, 36, 12),
    duzy_pies=c(0, 36, 2),
    kon=c(0, 72, 6),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))
  expect_true(czy_stac_nas(3, zwierzeta))
})

test_that("wymiana_na_drozsze zwraca ramke danych",{
  zwierzeta <- data.frame(
    krolik=c(7, 1, 53),
    owca=c(1, 6, 23),
    swinia=c(0, 12, 20),
    krowa=c(0, 36, 12),
    duzy_pies=c(0, 36, 2),
    kon=c(0, 72, 6),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))

  expect_true(is.data.frame(wymiana_na_drozsze(zwierzeta)))
})

test_that("wymiana_na_drozsze dokonuje wymiany",{
  zwierzeta <- data.frame(
    krolik=c(7, 1, 53),
    owca=c(1, 6, 23),
    swinia=c(0, 12, 20),
    krowa=c(0, 36, 12),
    duzy_pies=c(0, 36, 2),
    kon=c(0, 72, 6),
    maly_pies=c(0,6,4),
    row.names=c("w_zagrodzie", "krolikowartosc", "w_stadzie"))

  expect_equal(wymiana_na_drozsze(zwierzeta)[1,3],1)
})
