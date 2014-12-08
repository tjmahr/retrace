library("testthat")
context("upticking")

test_that("upticking updates values", {
  # Default values
  test_node <- Node$new()
  expect_equal(test_node$tick, 0)
  expect_equal(test_node$activation, 0)

  # First update from zero sets value
  test_node$cache <- .5
  test_node$uptick()
  expect_equal(test_node$activation, .5)

  # History tracking
  test_node$cache <- 1
  test_node$uptick()
  test_node$cache <- 1
  test_node$uptick()

  expect_equal(test_node$tick, 3)
  expect_equal(test_node$activation, 1)
  expect_equal(test_node$history, c(0, .5, 1))
})


context("bias nodes")
test_that("Bias nodes are constant", {

  bias <- BiasNode$new()
  expect_equal(bias$activation, 1)

  # Resting value of 1
  bias$uptick()$uptick()$uptick()
  expect_equal(bias$activation, 1)
  expect_equal(bias$history, c(1, 1, 1))

  # Input does nothing
  bias$cache <- 100
  bias$receive()$uptick()
  expect_equal(bias$activation, 1)

  # Cannot attach edge onto bias
  test_node <- Node$new()
  connect(test_node, bias, 1)
  expect_identical(bias$edges_in, list())


})

# expect_equal(test_node$tick, 3)
# expect_equal(test_node$activation, .875)
# expect_equal(test_node$history, c(0, .5, .75))
