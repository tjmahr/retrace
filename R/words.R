WordPool <- function(n_timeslices, lexicon) {
  assert_that(n_timeslices %% 6 == 0)

  # Plan of how each word should span the timeslices
  words <- lexicon[["Sounds"]] %>% unique
  word_layers <- words %>% spread_many_units(n_timeslices) %>% as.tbl %>%
    rename(Sounds = Unit) %>%
    left_join(lexicon, by = "Sounds") %>%
    arrange(Sounds)

  message("Creating ", nrow(word_layers), " word units")

  # Create a pool of words
  timeslices <- word_layers %$% Map(seq, t_start, t_end)
  word_pool <- word_layers %$%
    Map(WordNode$new, timeslices, type = Word, sounds = Sounds) %>%
    unlist(use.names = FALSE)

  # Count and report number of edges
  #   n_pools <- word_layers %>% select(Layer, Span) %>% unique %>% nrow
  #   n_words <- length(words)
  #
  #   message("Creating ", count_phoneme_paths(n_phonemes, n_pools),
  #           " phoneme-to-phoneme weights")

  ## Brute force solution: Enumerate all unordered phoneme pairs. Connect ones
  ## that overlap.

  # All unordered x-y combinations
  xs <- combn(nrow(word_layers), 2) %>% extract(1, )
  ys <- combn(nrow(word_layers), 2) %>% extract(2, )

  # Phoneme connection is scaled by amount of overlap. No edge if no overlap.
  inhibit_word <- trace_params$inhibit_word * -1

  connect_words <- function(x, y) {
    weight <- determine_competition(x, y) * inhibit_word
    if (weight != 0) connect(x, y, weight)
  }

  # Create edges
  Map(connect_words, word_pool[xs], word_pool[ys]) %>% invisible
  word_pool

}


# Each higher-level units spans the width of its number of constuent phonemes
compute_unit_span <- function(unit) nchar(unit) * 6


#' Spread several units (smart).
#'
#' This function is smart because it computes the spread of each unit-length
#' only once. Instead of doing 14 spreads for the phoneme alphabet (all 1 unit
#' long) for example, this function does a single spread and copies it 14 times.
spread_many_units <- function(units, n_timeslices) {
  # Represent units as dashes so that each unique unit-length is only spread
  # once. This will save us unnecessary calculations.
  lexicon <- data_frame(LetterUnit = units, Unit = make_dashes(units))
  dashes <- make_dashes(units) %>% unique

  spread_generic_units(dashes, n_timeslices) %>%
    left_join(lexicon, ., by = "Unit") %>%
    select(-Unit) %>%
    rename(Unit = LetterUnit)
}

#' Spread several units (dumb)
spread_generic_units <- function(units, n_timeslices) {
  Map(spread_unit, units, n_timeslices) %>% rbind_all
}

#' Spread a higher-order of unit
spread_unit <- function(unit, n_timeslices) {

  # Here's how the word "to" should spread over 21 timeslices
  # 123456789012345678901
  # [t........o][t.......
  # .o][t........o][t....
  # ....o][t........o][t.
  # .......o][t........o]

  unit_length <- compute_unit_span(unit)

  # Distance between first and last slice
  unit_dist <- unit_length - 1

  # Possible starting locations for layer 1
  l1_starts <- seq(-unit_dist, n_timeslices + unit_length, by = unit_length)

  # The starting locations in the other layers are the same as those in layer 1
  # but offset by some multiple of 3
  offsets <- seq(from = 0, to = unit_dist, by = 3)
  offsets <- offsets[offsets < n_timeslices]
  num_layers <- length(offsets)
  layer_starts <- list(l1_starts) %>% rep(num_layers) %>% Map(add, ., offsets)

  # Derive end locations from start locations
  layer_ends <- Map(add, layer_starts, unit_dist)

  # Truncate the layer starts and ends so they fall within the range
  # 1:n_timeslices
  span <- Map(squish_span, layer_starts, layer_ends, n_timeslices) %>%
    # Number layers and create data-frame
    Map(inject_key_value, ., value = seq_along(offsets), key = "Layer") %>%
    lapply(as.data.frame) %>%
    rbind_all %>%
    mutate(Unit = unit)

  assert_that(
    all(span$t_start <= span$t_end),
    all(1 <= span$t_start),
    all(span$t_end <= n_timeslices))

  span
}





squish_span <- function(starts, ends, n_timeslices) {
  assert_that(all(starts < ends))

  too_early <- ends <= 0
  too_late <- n_timeslices < starts
  timely <- !too_early & !too_late

  starts <- starts[timely] %>% squish(1, n_timeslices)
  ends <- ends[timely]  %>% squish(1, n_timeslices)

  list(t_start = starts, t_end = ends, Span = seq_along(ends))
}

connect_phoneme_to_word <- function(phon, word) {
  if (phoneme_word_overlap(phon, word)) {
    connect_onto(phon, word, weight = trace_params$excite_phon_word)
    connect_onto(word, phon, weight = trace_params$excite_word_phon)
  }
  invisible(NULL)
}

phoneme_word_overlap <- function(x, y) {
  overlap(x, y) & is.element(x$type, y$phonemes)
}


