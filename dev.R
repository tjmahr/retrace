# Starting point for this implementation http://bit.ly/1tK0XrE

phonemes <- phonemes86 <-
  read.csv("inst/phonemes1986_l.csv", stringsAsFactors = FALSE) %>% na.omit


# Minimizing the Number of Parameters (p. 21)
#
# At the expense of considerable realism, we have tried to keep TRACE II simple
# by using homogeneous parameters wherever possible. Thus, as already noted, the
# feature specifications of all phonemes were spread out over the same number of
# time slices, effectively giving all phonemes the same duration. The strength
# of the total excitation coming into a particular phoneme unit from the feature
# units was normalized to the same value for all phonemes, thus making each
# phoneme equally excitable by its own canonical pattern. Other simplifying
# assumptions should be noted as well. For example, there were no differences in
# connections or resting levels for words of dfierent frequency. It would have
# been a simple matter to incorporate frequency as McClelland and Rumelhart
# (1981) did, and a complete model would, of course, include some account for
# the ubiquitous effects of word frequency. We left it out here to facilitate an
# examination of the many other factors that appear to influence the process of
# word recognition in speech perception.


# Details of Processing Dynamics (pg. 20)
#
# The interactive activation process in the Trace model follows the dynamic
# assumptions laid out in McClelland and Rumelhart (1981). Each unit has a
# resting activation value arbitrarily set at 0, a maximum activation value
# arbitrarily set at 1.0, and a minimum activation set at -.3. On every time
# cycle of processing, all the weighted excitatory and inhibitory signals
# impinging upon a unit are added together. The signal from one unit to another
# is just the extent to which its activation exceeds 0; if its activation is
# less than 0, the signal is 0.* Global level-specific excitatory, inhibitory,
# and decay parameters scale the relative magnitudes of different types of
# influences on the activation of each unit. [...]
#
# After the net input to each unit has been determined based on the prior
# activations of the units, the activations of the units are all updated for the
# next processing cycle. The new value of the activation of the unit is a
# function of its net input from other units and its previous activation value.
# The exact function used (see McClelland & Rumelhart, 1981) keeps unit
# activations bounded between their maximum and minimum values. Given a constant
# input, the activation of a unit will stabilize at a point between its maximum
# and minimum that depends on the strength and sign (excitatory or inhibitory)
# of the input.
#
# With a net input of 0, the activation of the unit will gradually return to its
# resting level. Each processing time cycle corresponds to a single time slice
# at the feature level. This is actually a parameter of the model-there is no
# intrinsic reason why there should be a single cycle of the interactive
# activation process synchronized with the arrival of each successive slice of
# the input. A higher rate of cycling would speed the percolation of effects of
# new input through the network relative to the rate of presentation.
#
# *At the word level, the inhibitory signal from one word to another is just the
# square of the extent to which the sender’s activation exceeds zero. This tends
# to smooth the effects of many units suddenly becoming slightly activated, and
# of course it also increases the dominance of one active word over many weakly
# activated ones.





lexicon <-
  read.csv("inst/product_lex.csv", stringsAsFactors = FALSE) %>%
  mutate(Phones = nchar(Sounds))
n_timeslices <- lexicon$Phones %>% max %>% compute_word_duration


# feature_list <- rep(0, length(feature_set)) %>%
#   as.list %>%
#   set_names(feature_set) %>%
#   lapply(. %>% rep(times = n_timeslices))

n_timeslices <- compute_word_duration(num_phones = 4)
feat_matrix <- FeatureMatrix(n_timeslices)

t1 <- fill_feature_matrix("t", 1, FeatureMatrix(n_timeslices))
r2 <- fill_feature_matrix("r", 2, FeatureMatrix(n_timeslices))
a3 <- fill_feature_matrix("a", 3, FeatureMatrix(n_timeslices))
t4 <- fill_feature_matrix("t", 4, FeatureMatrix(n_timeslices))

trat <- t1 + r2 + a3 + t4
draw_feature_input(trat)
feature_input <- trat

# library("microbenchmark")
# library("lineprof")
# lineprof(BiasNode$new(1))


initialize_network <- function(feature_input) {

  n_timeslices <- ncol(feature_input)
  nonzero_features <- make_feature_dataframe(feature_input) %>%
    filter(Weight != 0)

  message("Creating ", n_timeslices, " input units")
  bias_layer <- Map(BiasNode$new, timeslices = seq_len(n_timeslices))

  message("Creating ", n_timeslices * 54, " feature units")
  feature_layer <- Map(FeaturePool, time = seq_len(n_timeslices))
  feature_layer_flat <- feature_layer %>% unlist(use.names = FALSE)


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
  features_per_phoneme <- phonemes %>% group_by(Phoneme) %>% tally

  phoneme_layer_df <- phoneme_layer %>%
    summarize_pool %>%
    rename(Phoneme = type) %>%
    left_join(features_per_phoneme, by = "Phoneme") %>%
    mutate(n_features = t_end - t_start + 1,
           n_paths = n_features * n)
  feature_to_phoneme <- sum(phoneme_layer_df$n_paths)

  message("Creating ", feature_to_phoneme, " feature-to-phoneme paths")

  connect_feature_pool_to_phoneme <- function(phoneme_node) {
    compatible_features <- get_phoneme_features(phoneme_node$type)
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

  structure(list(
    bias_layer = bias_layer,
    feature_layer = feature_layer_flat,
    phoneme_layer = phoneme_layer),
    class = "Network")
}



trace <- initialize_network(feature_input)

network <- unlist(trace, use.names = FALSE)


for(x in 1:60) {
  network %>% lapply(function(x) {x$receive(); NULL})
  network %>% lapply(function(x) {x$uptick(); NULL})
}



plot_bias_layer <- function(network) {

#   this_node <- network[get_tag(network) == "27d209"] %>% lift_node
#   this_node$edges_in
  bias_layer <- network %>% summarize_pool %>% filter(NodeClass == "BiasNode")
  network_tick <- network %>% vapply(function(x) x$tick, 1) %>% max

  title = paste0("Num updates: ", network_tick - 1)

  feature_layer <- network %>% summarize_pool %>%
    filter(NodeClass == "FeatureNode", 0 < activation )


  qplot(data = feature_layer, x = t_start, y = activation, geom = "text", label = value) + facet_wrap("type")
  qplot(data = feature_layer, x = t_start, y = value, size = activation) + facet_wrap("type")

  phoneme_layer <- network %>% summarize_pool %>%
    filter(NodeClass == "PhonemeNode", activation != 0) %>%
    mutate(Time = (t_start + t_end) / 2)


  qplot(data = phoneme_layer, x = Time, y = activation, geom = "text", label = type) +
    labs(title = title) + ylim(-.3, 1)

  feature_layer
  network_tick <- network %>% vapply(function(x) x$tick, 1) %>% max

  qplot(data = feature_layer, x = t_start, y = activation, geom = "text", label = factor(value)) + facet_wrap("type") + coord_cartesian(xlim = c(0, 30), ylim = c(-.3, 1.1))

}
