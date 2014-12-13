# Starting point for this implementation http://bit.ly/1tK0XrE

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






# lexicon <- read.csv("inst/product_lex.csv", stringsAsFactors = FALSE)



lexicon <- read.csv("inst/blood_lex.csv", stringsAsFactors = FALSE)
X <- "data-raw/phon_1986_weird.csv" %>%
  read.csv(stringsAsFactors = FALSE) %>%
  filter(Phoneme == "bs") %>%
  mutate(Phoneme = "X")



reduced_set <- lexicon$Sounds %>% str_inventory %>% unique


phonemes <- filter(phonemes, is.element(Phoneme, reduced_set))

trace <- initialize_network(plug, lexicon)




# Create ambiguous phoneme B
# X <- get_phoneme_features("b") %>% mutate(Phoneme = "X")
# X[which(X$Feature == "Voiced"), "value"] <- NA
# X[which(X$Feature == "Burst"), "value"] <- NA

# feat_mat <- tidyr::spread(phoneme_set, Phoneme, value)
#
# feat_mat$X - feat_mat$b
# feat_mat$X - feat_mat$p

# feature_list <- rep(0, length(feature_set)) %>%
#   as.list %>%
#   set_names(feature_set) %>%
#   lapply(. %>% rep(times = n_timeslices))

# n_timeslices <- compute_word_duration(num_phones = 4)
# feat_matrix <- FeatureMatrix(n_timeslices)
#
# phoneme_set <- phonemes
#
# set <- rbind_list(b, p, B)
# qplot(data = set, x = Feature, y = value, label = Phoneme, geom = "text")
#
# blush <- create_input_matrix("bl^S")
# draw_feature_input(blush)

ambig <- create_input_matrix("Xl^g", phoneme_set)
plot_feature_input(ambig)

plug <- create_input_matrix("pl^g", phonemes)
plot_feature_input(plug)

# lexicon
#
#
# trat <- create_input_matrix("trat")


trace <- initialize_network(ambig, lexicon)
trace <- initialize_network(plug, lexicon)


n00 <- summarize_pool(trace) %>% mutate(tick = 0)
n_history <- n00


plot_phonemes_layer <- function(network) {
  n_df <- function
}

for (tick in seq(12, 120, by = 12)) {
  trace <- uptick(trace, 12)
  this_tick <- summarize_pool(trace) %>% mutate(tick = tick)
  n_history <- rbind(n_history, this_tick)
}


uptick(trace, 10)

# n_history <- rbind_list(n00, n06, n12, n18, n24, n36, n48, n60)


history <- get_history(trace)



draw_phones_and_words <- function(history) {
  phones_and_words <- history %>%
    filter(NodeClass %in% c("PhonemeNode", "WordNode")) %>%
    mutate(Time = (t_start + t_end) / 2) %>%
    mutate(Class = factor(NodeClass, levels = c("WordNode", "PhonemeNode")))

  # 5% percent of the y-axis
  offset <- phones_and_words$activation %>% range %>% diff %>% divide_by(20)
  phones_and_words %<>% filter(offset < abs(activation))

  #   ignorable$width <- (ignorable$t_end - ignorable$t_start) + 1


  phones <- filter(phones_and_words, NodeClass == "PhonemeNode")
  # Recursively duplicate a data.frame
  copy <- function(x, n) {
    if (n == 0) x else rbind(x, copy(x, n - 1))
  }
  phones %<>% mutate(reps = t_end - t_start)

  df <- data.frame()
  for (row in seq_len(nrow(phones))) {
    df %<>% rbind(copy(phones[row, ], phones[row, "reps"]))
  }


  df <- df %>% group_by(tag, tick) %>% mutate(Time = t_start:t_end)

  qplot(data = phones, x = t_start, width = t_end - t_start, y = type, geom = "tile", fill = activation) + facet_wrap("tick") +
    scale_fill_gradient2(low="blue", mid = "white", high="red", )

  ggplot(data = phones_and_words) +
    aes(xmin = t_start, xmax = t_end,
        ymin = activation - offset, ymax = activation + offset,
        alpha = abs(activation)) +
#     geom_rect(color = "black", fill = NA) +
    geom_text(aes(x = Time, y = activation, label = type)) +
    facet_grid(Class ~ .)


}

ggplot(data = phones_and_words) +
  aes(x = t_start, xend = t_end, y = activation, yend = activation) +
  geom_segment() + facet_grid(Class ~ .)



write.csv(n_history, file = "ganong.csv", row.names = FALSE)
n_history <- summarize_pool(trace)


#
# ggplot(data = n_history) +
#   aes(x = t_start - .5, xend = t_end, y = activation, yend = activation) +
#   geom_segment() + facet_grid(NodeClass ~ .)



offset <- phones_and_words$activation %>% range %>% diff %>% divide_by(20)
ignorable <- filter(phones_and_words, (offset) < abs(activation))

ignorable$width <- (ignorable$t_end - ignorable$t_start) + 1



ggplot(data = ignorable) +
  aes(xmin = t_start, xmax = t_end, ymin = activation - offset, ymax = activation + offset) +
  geom_rect(color = "black", fill = NA) +
  geom_text(aes(x = Time, y = activation, label = type)) +
  facet_grid(Class ~ .)





#   p + geom_


qplot(data = ignorable, alpha = activation, x = Time, y = activation, angle = 45, geom = "text", label = type) + facet_grid(Class ~ tick)






plot_bias_layer <- function(network) {

#   this_node <- network[get_tag(network) == "27d209"] %>% lift_node
#   this_node$edges_in
  bias_layer <- network %>% summarize_pool %>% filter(NodeClass == "BiasNode")
  network_tick <- network %>% vapply(function(x) x$tick, 1) %>% max

  title = paste0("Num updates: ", network_tick)

  feature_layer <- network %>% summarize_pool %>%
    filter(NodeClass == "FeatureNode", 0 < activation )


  qplot(data = feature_layer, x = factor(t_start), y = activation, geom = "text", label = value) + facet_wrap("type")
  qplot(data = feature_layer, x = factor(t_start), y = value, size = activation) + facet_wrap("type")



p
  feature_layer
  network_tick <- network %>% vapply(function(x) x$tick, 1) %>% max

  qplot(data = feature_layer, x = t_start, y = activation, geom = "text", label = factor(value)) + facet_wrap("type") + coord_cartesian(xlim = c(0, 30), ylim = c(-.3, 1.1))

}
