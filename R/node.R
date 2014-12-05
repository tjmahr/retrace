require("magrittr")
require("digest")
require("R6")

act = 1
net = -1
min = -.3
max = 1
decay = 0.1
rest = 0

#' @export
Node <- R6Class("Node",
  public = list(
    # Structural fields
    tag = NA,
    edges_in = list(),
    edges_out = list(),

    # Activation parameters
    act_min = -.3,
    act_max = 1,
    act_rest = 0,
    act_decay = 0,

    # Activation and clock values
    activation = numeric(0),
    history = numeric(0),
    tick = 0,
    cache = 0,


    # Constructor
    initialize = function() {
      # Randomized name to help tell nodes apart
      self$activation <- self$act_rest
      self$tag <- rnorm(1) %>% digest %>% substr(1, 6)
    },

    attach_input = function(n) {
      self$edges_in %<>% set_tail(n) %>% unique
      invisible(self)
    },

    attach_output = function(n) {
      self$edges_out %<>% set_tail(n) %>% unique
      invisible(self)
    },

    send_activation = function() {
      # Activation is sent only if greater than 0.
      signal <- ifelse(self$activation < 0, 0, self$activation)
      signal
    },

    receive = function() {
      self$cache <- self$edges_in %>% visit_sender %>% sum
      invisible(self)
    },

    # Rum and McCl activation function
    compute_activation = function() {
      act <- self$activation
      dist_to_edge <- ifelse(0 <= act, self$act_max - act, act - self$act_min)
      pull_to_edge <- self$cache * dist_to_edge
      pull_to_rest <- self$act_decay * (act - self$act_rest)
      delta <- pull_to_edge - pull_to_rest
      act + delta
    },

    uptick = function() {
      self$tick %<>% add(1)
      self$history %<>% append(self$activation)
      self$activation <- self$compute_activation()
      self$cache <- 0
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
    act_decay = trace_params$decay_feat,

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
