
# act = 1
# net = -1
# min = -.3
# max = 1
# decay = 0.1
# rest = 0

#' The Node class
#' @export
Node <- R6Class("Node",
  public = list(
    # Structural fields
    tag = NA_character_,
    timeslices = NA_integer_,
    t_start = NA_integer_,
    t_end = NA_integer_,
    edges_in = list(),

    # Activation parameters
    act_min = -.3,
    act_max = 1,
    act_rest = 0,
    act_decay = 0,

    # Activation and clock values
    activation = numeric(0),
    tick = 0,
    cache = 0,
    # Pre-allocated room for history
    history = numeric(150),

    # Constructor
    initialize = function(timeslices) {
      if (!missing(timeslices)) {
        self$timeslices <- timeslices
        self$t_start <- min(timeslices)
        self$t_end   <- max(timeslices)
      }
      # Randomized name to tell nodes apart
      self$tag <- rnorm(1) %>% digest %>% substr(1, 6)
      self$activation <- self$act_rest
    },

    # Get current state of node [list]
    describe = function() {
      list(tag = self$tag,
           t_start = self$t_start,
           t_end = self$t_end,
           activation = self$activation,
           edges_in = length(self$edges_in))
    },

    # Get activation history [data.frame]
    remember =  function() {
      # Ignore pre-allocated values, but include current state
      values <- self$history %>% extract(seq_len(self$tick)) %>%
        append(self$activation)
      ticks <- seq(from = 0, to = self$tick)
      df <- data_frame(tick = ticks, activation = values, tag = self$tag) %>%
        select(tag, tick, activation)
      df
    },

    # Get activation (when asked by another node) [numeric]
    send_activation = function() {
      # Activation is sent only if greater than 0.
      signal <- if (self$activation < 0) 0 else self$activation
      signal
    },

    # Rum and McCl activation function [numeric]
    compute_activation = function() {
      act <- self$activation
      dist_to_edge <- if (0 <= act) self$act_max - act else act - self$act_min
      pull_to_edge <- self$cache * dist_to_edge
      pull_to_rest <- self$act_decay * (act - self$act_rest)
      delta <- pull_to_edge - pull_to_rest
      act + delta
    },

    # Add an edge
    attach_input = function(n) {
      self$edges_in %<>% set_tail(n) %>% unique
      invisible(self)
    },

    update_history = function() {
      # Expand history vector if running out of space
      slots_left <- length(self$history) - self$tick
      if (slots_left < 5) self$history %<>% append(rep(0, 100))
      self$history[self$tick] <- self$activation
      invisible(self)
    },

    # Collect input from incoming edges
    receive = function() {
      self$cache <- self$edges_in %>% visit %>% sum
      invisible(self)
    },

    uptick = function() {
      self$tick %<>% add(1)
      self$update_history()
      self$activation <- self$compute_activation()
      # "Spend" the collected input
      self$cache <- 0
      invisible(self)
    }
  )
)



#' @export
InputNode <- R6Class("InputNode",
  inherit = Node,
  public = list(
    # Override fields from the Node class so that the input node cannot receive
    # input
    attach_input = function(n) invisible(self),
    receive = function() invisible(self),

    # Activate only when tick falls within timeslices (the input is active)
    compute_activation = function() {
      timely_tick <- is.element(self$tick, self$timeslices)
      if (timely_tick) self$act_max else self$act_rest
    }
  )
)

# S3-based accessor so we can have vectorized access to tags
get_tag <- function(xs) UseMethod("get_tag")
get_tag.Node <- function(xs) xs$tag
get_tag.list <- function(xs) lapply(xs, get_tag) %>% unlist

#' @export
FeatureNode <- R6Class("FeatureNode",
  inherit = Node,
  public = list(
    type = NA_character_,
    value = NA_integer_,
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

#' @export
PhonemeNode <- R6Class("PhonemeNode",
  inherit = Node,
  public = list(
   type = NA_character_,
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

#' @export
WordNode <- R6Class("WordNode",
  inherit = Node,
  public = list(
   type = NA_character_,
   sounds = NA_character_,
   phonemes = NA_character_,
   act_decay = trace_params$decay_word,

   initialize = function(timeslices, type, sounds) {
     super$initialize(timeslices)
     self$type <- type
     self$sounds <- sounds
     self$phonemes <- str_inventory(sounds)
   },

   describe = function() {
     word_specific <- list(type = self$type, sounds = self$sounds)
     c(super$describe(), word_specific)
   }
  )
)





