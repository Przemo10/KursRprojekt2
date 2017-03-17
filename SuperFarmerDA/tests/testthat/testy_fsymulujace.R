
test_that("make.move zwraca macierz", {
  expect_true(is.matrix(make.move(game,strategia_DKA,0,1,dice1,dice2)))
})

test_that("gra zwraca wartosc numeryczna", {
  expect_true(is.numeric(gra(strategia_PDMS,1,1,dice1,dice2)))
})
