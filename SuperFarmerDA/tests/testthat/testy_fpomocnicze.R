
test_that("change.count zwraca macierz",{
  expect_true(is.matrix(change.count(game, "sheep", 2)))
})

test_that("change.count dodaje zwierze",{
  expect_equal(unname(change.count(game, "sheep", 2))[3,4], 2)
})

test_that("change.count nie zwraca ujemnych wartosci",{
  expect_equal(unname(change.count(game, "sheep", -2))[3,4],0)
})

test_that("exchange.farm zwraca macierz",{
  expect_true(is.matrix(exchange.farm(game, c("rabbit","sheep","pig"), 1)))
})

test_that("get.value zwraca wartosc numeryczna", {
  expect_true(is.numeric(get.value(game, "rabbit")))
})

test_that("get.count zwraca wartosc numeryczna", {
  expect_true(is.numeric(get.count(game, "rabbit")))
})

test_that("clear.count zwraca macierz", {
  expect_true(is.matrix(clear.count(game, "rabbit")))
})

test_that("clear.count zeruje wartosc", {
  game[1,4] <- 4
  expect_equal(clear.count(game, "rabbit")[1,4],0)
})

test_that("clear.all.counts zwraca macierz", {
  expect_true(is.matrix(clear.all.counts(game)))
})

test_that("game.finished zwraca FALSE", {
  expect_false(game.finished(game))
})

test_that("game.finished zwraca TRUE", {
  game[,4] <- c(1,0,1,1,1, 1, 1)
  expect_true(game.finished(game))
})

test_that("exchange.two.animals zwraca macierz",{
  game[4,4]<-1
  expect_true(is.matrix(exchange.two.animals(game,"pig","rabbit",1,12)))
})

test_that("exchange.two.animals dodaje zwierzeta",{
  game[4,4]<-1
  expect_equal(exchange.two.animals(game,"pig","rabbit",1,12)[1,4],12)
})

test_that("convert.farm.vector zwraca macierz", {
  expect_true(is.matrix(convert.farm.vector(c(10,2,1,1,0,0,0))))
})

test_that("convert.game.table zwraca wektor",{
  expect_true(is.vector(convert.game.table(game)))
})
