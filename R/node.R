require("magrittr")
require("digest")
require("R6")
require("assertthat")
require("stringr")
require("dplyr", warn.conflicts = FALSE)


# act = 1
# net = -1
# min = -.3
# max = 1
# decay = 0.1
# rest = 0

#' @export
Node <- R6Class("Node",
  public = list(
    # Structural fields
    tag = NA_character_,
    edges_in = list(),
    timeslices = NA_integer_,
    t_start = NA_integer_,
    t_end = NA_integer_,

    # Activation parameters
    act_min = -.3,
    act_max = 1,
    act_rest = 0,
    act_decay = 0,

    # Activation and clock values
    activation = numeric(0),
    history = numeric(0),
    tick = 1,
    cache = 0,

    # Constructor
    initialize = function(timeslices) {
      if (!missing(timeslices)) {
        self$timeslices <- timeslices
        self$t_start <- min(timeslices)
        self$t_end   <- max(timeslices)
      }
      # Randomized name to help tell nodes apart
      self$tag <- rnorm(1) %>% digest %>% substr(1, 6)
      self$activation <- self$act_rest
    },

    attach_input = function(n) {
      self$edges_in %<>% set_tail(n) %>% unique
      invisible(self)
    },

    send_activation = function() {
      # Activation is sent only if greater than 0.
      signal <- if (self$activation < 0) 0 else self$activation
      signal
    },

    describe = function() {
      list(tag = self$tag,
           t_start = min(self$timeslices),
           t_end = max(self$timeslices),
           activation = self$activation,
           edges_in = length(self$edges_in))
    },

    receive = function() {
      self$cache <- self$edges_in %>% visit %>% sum
      invisible(self)
    },

    # Rum and McCl activation function
    compute_activation = function() {
      act <- self$activation
      dist_to_edge <- if (0 <= act) self$act_max - act else act - self$act_min
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

BiasNode <- R6Class("BiasNode",
  inherit = Node,
  public = list(
    # Override fields from the Node class that the bias node cannot receive
    # input and always returns a fixed input

    # Refuse input connections
    attach_input = function(n) invisible(self),
    receive = function() invisible(self),

    # Activate only when turned on
    compute_activation = function() {
      if (self$t_start <= self$tick) self$act_max else self$act_rest
    },

    uptick = function() {
      self$tick %<>% add(1)
      self$history %<>% append(self$activation)
      self$activation <- self$compute_activation()
      invisible(self)
    }
  )
)


get_tag <- function(xs) UseMethod("get_tag")
get_tag.Node <- function(xs) xs$tag
get_tag.list <- function(xs) lapply(xs, get_tag) %>% unlist

FeatureNode <- R6Class("FeatureNode",
  inherit = Node,
  public = list(
    type = NA,
    value = NA,
    act_decay = trace_params$decay_feat,

    initialize = function(timeslices, type, value) {
      super$initialize(timeslices)
      self$type = type
      self$value = value
    },

    describe = function() {
      feature_specific <- list(type = self$type, value = self$value)
      c(super$describe(), feature_specific)
    }
  )
)

PhonemeNode <- R6Class("PhonemeNode",
  inherit = Node,
  public = list(
   type = NA,
   act_decay = trace_params$decay_phon,

   initialize = function(timeslices, type) {
     super$initialize(timeslices)
     self$type <- type
   },

   describe = function() {
     phoneme_specific <- list(type = self$type)
     c(super$describe(), phoneme_specific)
   }
  )
)




FeatureDetector <- function(type, time) {
  # Create a pool of feature nodes
  node_set <- Map(FeatureNode$new, timeslices = rep(time, 8), type = type,
                  value = 1:8) %>% unlist(use.names = FALSE)
  # All unordered x-y combinations
  xs <- combn(8, 2) %>% extract(1, )
  ys <- combn(8, 2) %>% extract(2, )

  weight <- trace_params$inhibit_feat * -1

  # Connect each x-y pair in the pool
  Map(connect, node_set[xs], node_set[ys], weight) %>% invisible

  node_set
}
