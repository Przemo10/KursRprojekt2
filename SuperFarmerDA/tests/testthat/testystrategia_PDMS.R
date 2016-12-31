
test_that("exchange.animals.for.rabbits zwraca macierz",{
  expect_true(is.matrix(exchange.animals.for.rabbits(game)))
})

test_that("exchange.animals.for.rabbits zwraca macierz", {
  expect_true(is.matrix(exchange.animals.for.rabbits(game)))
})

test_that("exchange.horse zwraca macierz", {
  expect_true(is.matrix(exchange.horse(game)))
})

test_that("get.horse zwraca macierz", {
  expect_true(is.matrix(get.horse(game)))
})

test_that("strategia_PDMS zwraca wektor", {
  expect_true(is.vector(strategia_PDMS(c(8,1,3,0,0,0,1))))
})
