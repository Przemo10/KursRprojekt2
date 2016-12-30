
test_that("make.move zwraca macierz", {
  expect_true(is.matrix(make.move(game,strategy_DKA,0,1,dice1,dice2)))
})

test_that("play zwraca wartosc numeryczna", {
  expect_true(is.numeric(play(strategy_PDMS,1,1,dice1,dice2)))
})
