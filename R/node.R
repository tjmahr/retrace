

Node <- R6Class("Node",
  public = list(
    # Fields
    tag = NA,
    last_input = 0,
    last_output = 0,
    edges_in = list(),
    edges_out = list(),

    # Constructor
    initialize = function() {
      # Randomized name to help tell them apart
      self$tag = rnorm(1) %>% digest %>% substr(1, 6)
    },

    add_input = function(n) {
      self$edges_in %<>% set_tail(n) %>% unique
      invisible(self)
    },

    add_output = function(n) {
      self$edges_out %<>% set_tail(n) %>% unique
      invisible(self)
    }
  )
)

#' Append an item onto a list
set_tail <- function(x, y) {
  x[[length(x) + 1]] <- y
  x
}

FeatureNode <- R6Class("FeatureNode",
  inherit = Node,
  public = list(
    type = NA,
    value = NA,
    initialize = function(type, value) {
      # Randomized name to help tell them apart
      self$tag = rnorm(1) %>% digest() %>% substr(1, 6)
      self$type = type
      self$value = value
    }
  )
)




FeatureDetector <- function(type) {
  # Create a pool of feature nodes
  node_set <- Map(FeatureNode$new, type, 1:8) %>% unname

  # All unordered x-y combinations
  xs <- combn(8, 2) %>% extract(1, )
  ys <- combn(8, 2) %>% extract(2, )

  # Connect each x-y pair in the pool
  connect_pair_in_pool <- function(i, j, weight, pool = node_set) {
    connect(pool[[i]], pool[[j]], weight)
  }
  Map(connect_pair_in_pool, xs, ys, 100) %>% invisible

  node_set
}
