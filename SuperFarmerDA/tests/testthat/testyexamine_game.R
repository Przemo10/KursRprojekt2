
test_that("examine.game zwraca wartosci numeryczne", {
  expect_true(is.list(examine.game(strategy_PDMS, 20, die1, die2)))
})
