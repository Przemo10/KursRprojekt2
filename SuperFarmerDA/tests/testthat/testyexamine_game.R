
test_that("examine.game zwraca wartosci numeryczne", {
  expect_true(is.list(badaj_gre(strategia_PDMS, 20,0,1, dice1, dice2)))
})
