library("testthat")
context("upticking")

test_that("upticking updates values", {
  test_node <- Node$new()

  expect_equal(test_node$tick, 0)
  expect_equal(test_node$activation, 0)

  test_node$cache <- 1
  test_node$uptick()

  test_node$cache <- 2
  test_node$uptick()

  test_node$cache <- 3
  test_node$uptick()

  expect_equal(test_node$tick, 3)
  expect_equal(test_node$activation, 3)
  expect_equal(test_node$history, 0:2)
})
