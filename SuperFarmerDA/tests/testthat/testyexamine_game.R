
test_that("examine.game zwraca wartosci numeryczne", {
  expect_true(is.list(examine.game(strategy_PDMS, 20,0,1, dice1, dice2)))
})
