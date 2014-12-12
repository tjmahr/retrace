#' Create two sets of overlapping phonemes
#' @param n_timeslices number of timeslices to span. Because phonemes are 6
#'   timeslices wide, the number of timeslices must be divisible by 6
#' @return a list of Phoneme
PhonemePool <- function(n_timeslices) {
  assert_that(n_timeslices %% 6 == 0)

  # Plan of how each phoneme should span the timeslices
  alphabet <- phonemes %>% extract2("Phoneme") %>% unique
  phoneme_layers <- alphabet %>%
    spread_phonemes(n_timeslices) %>% as.tbl %>%
    arrange(Phoneme)

  message("Creating ", nrow(phoneme_layers), " phonemes")

  # Create a pool of nodes for the two layers
  timeslices <- phoneme_layers %$% Map(seq, t_start, t_end)
  phoneme_pool <- Map(PhonemeNode$new, timeslices, phoneme_layers$Phoneme) %>%
    unlist(use.names = FALSE)

  # Count and report number of edges
  n_pools <- phoneme_layers %>% select(Layer, Span) %>% unique %>% nrow
  n_phonemes <- length(alphabet)

  message("Creating ", count_phoneme_paths(n_phonemes, n_pools),
          " phoneme-to-phoneme weights")

  ## Brute force solution: Enumerate all unordered phoneme pairs. Connect ones
  ## that overlap.

  # All unordered x-y combinations
  xs <- combn(nrow(phoneme_layers), 2) %>% extract(1, )
  ys <- combn(nrow(phoneme_layers), 2) %>% extract(2, )

  # Phoneme connection is scaled by amount of overlap. No edge if no overlap.
  inhibit_phon <- trace_params$inhibit_phon * -1
  connect_phonemes <- function(x, y) {
    weight <- determine_competition(x, y) * inhibit_phon
    if (weight != 0) connect(x, y, weight)
  }

  # Create edges
  Map(connect_phonemes, phoneme_pool[xs], phoneme_pool[ys]) %>% invisible
  phoneme_pool
}

#' Create plan of how to spread phonemes over the two layers
spread_phonemes <- function(phoneme_alphabet, n_timeslices) {
  phoneme_alphabet <- phoneme_alphabet

  spread_many_units(phoneme_alphabet, n_timeslices) %>%
    rename(Phoneme = Unit)
}



get_phoneme_features <- function(phoneme) {
  phonemes %>% filter(Phoneme == phoneme)
}


count_phoneme_paths <- function(n_phonemes, n_spans) {
  n_gaps <- n_spans - 1
  # n by n combinations across each gap times 2 directions
  n_paths_across <- n_phonemes * n_phonemes * n_gaps * 2
  # minus 1 because no self inhibition
  n_paths_within <- n_phonemes * (n_phonemes - 1) * n_spans
  n_paths_across + n_paths_within
}



#' Create a pool of feature detectors
FeaturePool <- function(time) {
  features <- c("Power", "Vocalic", "Diffuse", "Acute",
                "Consonantal", "Voiced", "Burst")
  detector_pool <- Map(FeatureDetector, features, time) %>%
    unlist(use.names = FALSE)
  detector_pool
}

#' Create a pool of mutually inhibitory nodes for all values in a single
#' feature.
#'
#' TODO the feature value range (0 to 8) is hard-coded. Should be expressed as a
#' parameter.
FeatureDetector <- function(type, time) {
  # Create a pool of feature nodes
  feature_range <- 0:8
  n_features <- length(feature_range)
  slices <- time %>% rep(n_features)

  node_set <- Map(FeatureNode$new, timeslices = slices, type = type,
                  value = feature_range) %>% unlist(use.names = FALSE)

  # All unordered x-y combinations. Note: These values refer to positions in the
  # node_set, not the value of the node. node_set[N] has feature value N-1.
  pairs <- combn(n_features, 2)
  xs <- pairs[1, ]
  ys <- pairs[2, ]

  # Connect each x-y pair in the pool
  weight <- trace_params$inhibit_feat * -1
  Map(connect, node_set[xs], node_set[ys], weight) %>% invisible

  node_set
}




summarize_pool <- function(pool) {
  # Wrapper for Node$describe method so we can vectorize it
  describe <- function(node) {
    # quickdf trick from http://adv-r.had.co.nz/Profiling.html#be-lazy
    l <- node$describe()
    l$NodeClass <- node %>% class %>% head(1)
    class(l) <- "data.frame"
    attr(l, "row.names") <- .set_row_names(length(l[[1]]))
    l
  }

  # Make a data-frame summary of nodes in the pool
  pool %>% lapply(describe) %>% rbind_all
}




connect_tag_onto_tag <- function(x_tag, y_tag, weight, pool) {
  x_node <- find_tag_in_pool(x_tag, pool)
  y_node <- find_tag_in_pool(y_tag, pool)
  connect_onto(x_node, y_node, weight)
}


# Find a node by its name
find_tag_in_pool <- function(tag, pool) {
  node <- Filter(function(node) node$tag == tag, pool)
  assert_that(length(node) == 1)
  lift_node(node, position = 1)
}

# lift a node from a list
lift_node <- function(xs, position = 1) {
  assert_that(is_node(xs[[position]]))
  xs[[position]]
}

is_node <- function(x) inherits(x, "Node")





