
#' @export
create_input_matrix <- function(word, phoneme_set = phonemes, silence = TRUE) {
  # The word is wrapped in silence first
  word <- if (silence) str_wrap_silence(word) else word
  sounds <- str_tokenize(word)
  n_timeslices <- length(sounds) %>% compute_word_duration

  # Create an input matrix for each sound. Layer the individual sound matrices
  # on top of each other (i.e., add them together) to create overlapping feature
  # values in the word.
  input_template <- FeatureMatrix(n_timeslices)
  input_matrix <- input_template
  for (sound_i in seq_along(sounds)) {
    sound <- sounds[[sound_i]]
    matrix_i <- fill_feature_matrix(sound, sound_i, input_template, phoneme_set)
    input_matrix <- input_matrix + matrix_i
  }
  input_matrix
}


#' Create an matrix to hold the feature values of an input word
FeatureMatrix <- function(n_timeslices, feature_range = 0:8) {
  features <- c("Acute", "Burst", "Consonantal", "Diffuse",
                "Power", "Vocalic", "Voiced")
  feature_set <- outer(features, feature_range, paste0) %>%
    as.character %>% sort

  matrix(0, nrow = length(feature_set), ncol = n_timeslices, byrow = TRUE) %>%
    set_rownames(feature_set)
}

#' How many timeslices are needed to encode a number of phonemes?
compute_word_duration <- function(num_phones) {
  # Phonemes spread over 11 units. We add one slice to end so that number of
  # slices is divisible six (width of a phoneme unit)
  (num_phones * 6) + 5 + 1
}

# Where does the nth phoneme start in the Trace?
compute_phoneme_start <- function(phoneme_number) {
  1 + (phoneme_number - 1) * 6
}



spread_feature <- function(n_cells, start_cell) {
  zeroes <- rep(0, n_cells)
  zeroes[seq(start_cell, start_cell + 10)] <- feature_gradient
  zeroes
}

make_feature_dataframe <- function(feature_matrix) {
  # Bind row-names as a column of feature labels
  feat_df <- feature_matrix %>% as.data.frame %>%
    mutate(feature = row.names(.)) %>%
    # Convert to long format, treating column numbers as Time values
    tidyr::gather(Time, Weight, -feature) %>%
    mutate(Time = str_replace(Time, "V", "")) %>%
    # Break labels into key-value pairs
    tidyr::extract(feature, c("Feature", "Value"), "([[:alnum:]]+)(\\d+)") %>%
    mutate(Time = as.numeric(Time), Value = as.numeric(Value))
  feat_df
}


#' Plot feature values
#' @param feature_matrix an input feature matrix
#' @return a ggplot object showing how feature values vary and overlap over the
#'   course of the time input
#' @export
plot_feature_input <- function(feature_matrix) {
  # Filter out negative weights so that 0 point values don't clutter plot
  feat_df <- make_feature_dataframe(feature_matrix) %>%
    filter(Weight != 0)

  p <- qplot(data = feat_df, x = Time, size = Weight, y = Value) +
    facet_wrap("Feature")
  p
}

#' TODO set skirt-length as a parameter.
fill_feature_matrix <- function(phoneme, phoneme_number, feat_matrix, phoneme_set) {
  start_time <- compute_phoneme_start(phoneme_number)
  # Rows to select
  phoneme_def <- phoneme_set %>%
    filter(Phoneme == phoneme, !is.na(Value), Weight != 0)%>%
    mutate(Row = paste0(Feature, Value)) %>% arrange(Row)

  # Spread out the value of each feature peak. Stack these vectors on each other
  # to form a matrix.
  feature_spreads <- phoneme_def %$%
    Map(create_feature_gradient, peak = Weight) %>%
    lapply(matrix, nrow = 1, byrow = TRUE) %>% do.call(rbind, .) %>%
    set_rownames(phoneme_def$Row)

  # Rows and columns to select
  times <- seq(from = start_time, length.out = ncol(feature_spreads))
  rows <- rownames(feat_matrix) %>% is.element(phoneme_def$Row)

  # Update subset of feature matrix
  feat_matrix[rows, times] <- feature_spreads
  feat_matrix
}


