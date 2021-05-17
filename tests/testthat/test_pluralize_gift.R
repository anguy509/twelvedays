context("pluralize_gift")

test_that("pluralize_gift works", {
  output <- "geese"
  expect_equal(pluralize_gift("goose"), output)

  output <- "trinities"
  expect_equal(pluralize_gift("trinity"), output)

  output <- "koalas"
  expect_equal(pluralize_gift("koala"), output)

})

