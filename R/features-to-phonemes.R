
#' Create a pool of feature detectors
FeaturePool <- function() {
  features <- c("Power", "Vocalic", "Diffuse", "Acute",
                "Consonantal", "Voiced", "Burst")
  detector_pool <- Map(FeatureDetector, features) %>% unlist(use.names = FALSE)
  detector_pool
}

connect_pool_to_phoneme <- function(pool, phoneme) {
  # Single case: Find tag of matching feature-detector and connect
  connect_feature <- function(feature, value) {
    feature <- find_feature(pool, feature, value) %>%
      find_tag_in_pool(pool)
    connect(feature, phoneme, trace_params$excite_feat_phon)
  }

  phoneme_def <- phonemes %>% filter(Phoneme == p_node$type)

  Map(connect_feature, phoneme_def$Feature, phoneme_def$value) %>%
    unlist(use.names = FALSE) %>% invisible
}


#' Find a specific feature node in a pool of nodes
#' @param pool a list of feature nodes
#' @param this_type the name of feature type
#' @param this_value the desired value for the feature
#' @return the name of the tag of a matching feature node, wrapped in a list
find_feature <- function(pool, this_type, this_value) {
  # Wrapper for Node$describe method so we can vectorize it
  describe <- function(node) {
    node$describe() %>% as.data.frame(stringsAsFactors = FALSE)
  }

  # Make a data-frame summary of nodes in the pool
  tag <- pool %>%
    lapply(describe) %>%
    rbind_all %>%
    # Extract the desired node
    filter(type == this_type, value == this_value) %>%
    select(tag) %>%
    as.list

  tag
}


#' Find a node by its name
find_tag_in_pool <- function(tag, pool) {
  node <- Filter(function(node) node$tag == tag, pool)
  assert_that(length(node) == 1)
  lift_node(node, position = 1)
}

# lift a node from a list
lift_node <- function(xs, position = 1) {
  assert_that(is_Node(xs[[position]]))
  xs[[position]]
}

is_Node <- function(x) inherits(x, "Node")

