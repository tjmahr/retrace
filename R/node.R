require("magrittr")
require("digest")
require("R6")


#' @export
Node <- R6Class("Node",
  public = list(
    # Fields
    tag = NA,
    activation = 0,
    tick = 0,
    history = numeric(0),
    cache = NA,

    edges_in = list(),
    edges_out = list(),

    # Constructor
    initialize = function() {
      # Randomized name to help tell nodes apart
      self$tag = rnorm(1) %>% digest %>% substr(1, 6)
    },

    attach_input = function(n) {
      self$edges_in %<>% set_tail(n) %>% unique
      invisible(self)
    },

    attach_output = function(n) {
      self$edges_out %<>% set_tail(n) %>% unique
      invisible(self)
    },

    receive = function() {
      self$cache <- self$edges_in %>% visit_sender %>% sum
      invisible(self)
    },

    uptick = function() {
      self$tick %<>% add(1)
      self$history %<>% append(self$activation)
      self$activation <- self$cache
      self$cache <- NA
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
