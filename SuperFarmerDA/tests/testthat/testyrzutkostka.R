
test_that("throw.dice zwraca macierz",{
  expect_true(is.matrix(throw.dice(game,0,0, die1, die2)))
})

test_that("throw.dice nie dodaje psow",{
  expect_equal(unname(throw.dice(game,0,0, die1, die2))[c(2,5),4], c(0,0))
})

test_that("wolf.reaction zwraca macierz",{
  expect_true(is.matrix(wolf.reaction(game, "wolf",1)))
})

test_that("wolf.reaction usuwa kroliki",{
  game[1,4]<-2
  expect_equal(unname(wolf.reaction(game, "wolf",0))[1,4], 0)
})

test_that("fox.reaction zwraca macierz",{
  expect_true(is.matrix(fox.reaction(game, "fox",0)))
})

test_that("fox.reaction usuwa kroliki",{
  game[1,4]<-2
  expect_equal(unname(fox.reaction(game, "fox",0))[1,4], 0)
})

test_that("multiply.animals zwraca macierz",{
  expect_true(is.matrix(multiply.animals(game, "rabbit", "rabbit")))
})

test_that("multiply.animals dodaje krolika",{
  expect_equal(unname(multiply.animals(game, "rabbit", "rabbit"))[1,4],1)
})
