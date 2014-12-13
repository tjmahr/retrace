
#' Create a TRACE network
#'
#' A Network object is just a list of Nodes. The print function shows the number
#' of nodes of each type, the timespan of the network and the number of updates
#' applied.
#'
#' @param feature_input a matrix of feature values over time
#' @param lexicon a lexicon data-frame
#' @return a list of interconnected feature, phoneme, and word nodes
#' @export
initialize_network <- function(feature_input, lexicon) {
  start_sys_time <- Sys.time()

  # Make sure lexicon includes silence
  if (!is.element("-", lexicon$Word)) {
    lexicon %<>% rbind(data.frame(Word = "-", Sounds = "-"))
  }

  n_timeslices <- ncol(feature_input)
  feature_count <- length(feature_input)
  nonzero_features <- make_feature_dataframe(feature_input) %>%
    filter(Weight != 0)

  message("Creating ", n_timeslices, " input units")
  bias_layer <- Map(BiasNode$new, timeslices = seq_len(n_timeslices))

  message("Creating ", feature_count, " feature units")
  feature_layer <- Map(FeaturePool, time = seq_len(n_timeslices))
  # feature_layer has a sublist for each timeslice. Flatten into a single list
  # to help with some operations later on.
  feature_layer_flat <- feature_layer %>% unlist(use.names = FALSE)

  assert_that(
    length(bias_layer) == n_timeslices,
    length(feature_layer) == n_timeslices,
    length(feature_layer_flat) == feature_count
  )

  bias_layer_tags <- bias_layer %>% summarize_pool %>%
    select(BiasTag = tag, Time = t_start, -t_end)

  feature_layer_tags <- feature_layer_flat  %>% summarize_pool %>%
    select(Time = t_start,
           Feature = type,
           Value = value,
           FeatureTag = tag)

  edges_to_add <- feature_layer_tags %>%
    inner_join(nonzero_features, by = c("Feature", "Value", "Time")) %>%
    left_join(bias_layer_tags, by = "Time")


  message("Creating ", nrow(edges_to_add), " input-to-feature edges")
  bias_feature_pool <- c(bias_layer, feature_layer_flat)
  lambda_connect <- function(x_tag, y_tag, weight, pool = bias_feature_pool) {
    connect_tag_onto_tag(x_tag, y_tag, weight, pool)
  }

  Map(lambda_connect,
      x_tag = edges_to_add$BiasTag,
      y_tag = edges_to_add$FeatureTag,
      weight = edges_to_add$Weight) %>% invisible

  phoneme_layer <- PhonemePool(n_timeslices)

  features_per_phoneme <- phonemes %>% group_by(Phoneme) %>%
    filter(Weight != 0) %>% tally

  phoneme_layer_df <- phoneme_layer %>%
    summarize_pool %>%
    rename(Phoneme = type) %>%
    left_join(features_per_phoneme, by = "Phoneme") %>%
    mutate(n_features = t_end - t_start + 1,
           n_paths = n_features * n)
  feature_to_phoneme <- sum(phoneme_layer_df$n_paths)

  message("Creating ", feature_to_phoneme, " feature-to-phoneme paths")


  connect_feature_pool_to_phoneme <- function(phoneme_node) {
    compatible_features <- get_phoneme_features(phoneme_node$type) %>%
      filter(Weight != 0) %>%
      select(Phoneme, Feature, value = Value, -Weight)
    feature_pools <- feature_layer[phoneme_node$timeslices] %>%
      unlist(use.names = FALSE)

    # Get tags of all compatible feature nodes
    feature_tags <- feature_pools %>% summarize_pool %>%
      rename(Feature = type) %>%
      inner_join(compatible_features, by = c("Feature", "value")) %>%
      extract2("tag")

    # Extract those nodes
    matching_tags <- feature_pools %>% get_tag %>% is.element(feature_tags)
    feature_nodes <- feature_pools[matching_tags] %>% unlist(use.names = FALSE)

    lifted_pnode <- list(phoneme_node)
    Map(connect_onto, feature_nodes, lifted_pnode, trace_params$excite_feat_phon) %>% invisible
  }

  Map(connect_feature_pool_to_phoneme, phoneme_layer) %>% invisible


  word_pool <- WordPool(n_timeslices, lexicon)

  message("Checking ", length(phoneme_layer) * length(word_pool), " phoneme-word paths")
  for (phoneme in phoneme_layer) {
    for (word in word_pool) {
      connect_phoneme_to_word(phoneme, word)
    }
  }


  Sys.time() %>% subtract(start_sys_time) %>%
    as.numeric(units = "secs") %>% round(1) %>%
    paste0("Construction completed in ", ., " seconds") %>%
    message

  structure(c(bias_layer, feature_layer_flat, phoneme_layer, word_pool),
            class = "Network")
}

#' @export
uptick <- function(x, n_ticks = 1) UseMethod("uptick")
uptick.Network <- function(x, n_ticks = 1) {
  for(tick in seq_len(n_ticks)) {
    x %>% lapply(function(n) n$receive()) %>% invisible
    x %>% lapply(function(n) n$uptick()) %>% invisible
  }
  x
}

#' @export
print.Network <- function(x, ...) {
  pool_df <- summarize_pool(x)
  d <- pool_df %>% group_by(NodeClass) %>%
    tally %>%
    mutate(Class = str_replace(NodeClass, "Node", ""))

  pool_facts <- list(
    NodeCounts = structure(as.list(d$n), names = d$Class),
    Timeslices = max(pool_df$t_end),
    Ticks = x[[1]]$tick)

  cat("Network summary: \n")
  str(pool_facts, give.attr = FALSE, give.head = FALSE, no.list = TRUE, ...)
}

