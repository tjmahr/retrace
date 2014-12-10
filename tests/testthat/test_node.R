library("testthat")
context("Nodes")

test_that("Node defaults", {
  # Default values
  test_node_a <- Node$new()
  test_node_b <- Node$new(timeslices = 1)

  expect_true(is.na(test_node_a$timeslices))
  expect_equal(test_node_b$timeslices, 1)

})


test_that("Connect only overlapping nodes", {
  # Nodes overlap when their timeslices overlap
  x <- Node$new(timeslices = 1:2)
  y <- Node$new(timeslices = 2:3)
  z <- Node$new(timeslices = 3:4)
  expect_true(overlap(x, y))
  expect_true(overlap(z, y))
  expect_false(overlap(z, x))

  # Overlapping nodes can connect
  connect(x, y, 1)
  connect(y, z, 1)

  expect_equal(length(x$edges_in), 1)
  expect_equal(length(y$edges_in), 2)

  # Disjoint nodes cannot connect
  connect(x, z, 1)
  expect_equal(length(x$edges_in), 1)
  expect_equal(length(z$edges_in), 1)
})



context("Edges")

test_that("Simple message-passing", {
  x <- Node$new(timeslices = 1:2)
  y <- Node$new(timeslices = 2:3)
  z <- Node$new(timeslices = 1:2)

  y$activation <- y_act <- 10
  z$activation <- z_act <- 20
  weight <- .5

  connect(x, y, weight)
  connect(x, z, weight)

  # Nodes are connected
  expect_equal(x$edges_in[[1]]$s_tag, get_tag(y))
  expect_equal(x$edges_in[[2]]$s_tag, get_tag(z))

  # Reciprocal connection
  expect_equal(y$edges_in[[1]]$s_tag, get_tag(x))
  expect_equal(z$edges_in[[1]]$s_tag, get_tag(x))

  # Visiting an edge is multiplying the activation by the weight
  expect_equal(visit(x$edges_in[[1]], multiply_by), y_act * weight)

  # Receiving inputs
  x$receive()
  expect_equal(x$cache, (z_act + y_act) * weight)

})



