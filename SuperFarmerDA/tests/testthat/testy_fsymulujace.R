
test_that("make.move zwraca macierz", {
  expect_true(is.matrix(make.move(game,strategy_DKA,die1,die2)))
})

test_that("play zwraca wartosc numeryczna", {
  expect_true(is.numeric(play(strategy_PDMS,die1,die2)))
})
