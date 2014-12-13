library("testthat")
context("upticking")

test_that("Upticking on a single Node", {
  # Default values. Ticks start at 0
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

  history <- test_node$remember()
  expect_equal(history$tick, 0:3)
  expect_equal(history$activation, c(0, .5, 1, 1))
})


context("bias nodes")
test_that("Bias nodes are constant", {

  # Default values
  test_timeslice <- 3:5
  bias <- BiasNode$new(timeslice = test_timeslice)
  expect_equal(bias$activation, 0)
  expect_equal(bias$tick, 0)

  # Resting value of 0 until awakens
  replicate(10, bias$uptick()) %>% invisible
  expect_equal(bias$activation, 0)

  # History starts counting at 0.
  test_timeslice_index <- test_timeslice + 1

  # Only values during timeslice are 1
  are_all <- function(xs, y) all(xs == y)

  history <- bias$remember()
  expect_true(are_all(history[test_timeslice_index, "activation"], 1))
  expect_true(are_all(history[-test_timeslice_index, "activation"], 0))

  # Input does nothing
  bias$cache <- 100
  bias$receive()$uptick()
  expect_equal(bias$activation, 0)

  # Cannot attach edge onto bias
  test_node <- Node$new()
  connect(test_node, bias, 1)
  expect_identical(bias$edges_in, list())

})


context("features to phonemes")
test_that("dev functions", {

#   pool <- FeaturePool()
#   phoneme <- PhonemeNode$new("p")
#   connect_pool_to_phoneme(pool, phoneme)


})
# expect_equal(test_node$tick, 3)
# expect_equal(test_node$activation, .875)
# expect_equal(test_node$history, c(0, .5, .75))
