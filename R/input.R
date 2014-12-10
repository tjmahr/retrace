FeatureMatrix <- function(n_timeslices) {
  features <- c("Acute", "Burst", "Consonantal", "Diffuse",
                "Power", "Vocalic", "Voiced")
  feature_set <- features %>% lapply(. %>% paste0(0:8)) %>% unlist
  matrix(0, nrow = length(feature_set), ncol = n_timeslices, byrow = TRUE) %>%
    set_rownames(feature_set)
}

#' How many timeslices are needed to encode a number of phonemes?
#' TODO: Encode final bit as silence?
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

draw_feature_input <- function(feature_matrix) {
  feat_df <- feature_matrix %>% make_feature_dataframe %>% filter(Weight != 0)

  require("ggplot2")

  qplot(data = feat_df, x = Time, size = Weight, y = Value) +
    facet_wrap("Feature")
}


fill_feature_matrix <- function(phoneme, phoneme_number, feat_matrix) {
  start_time <- compute_phoneme_start(phoneme_number)
  # Rows to select
  phoneme_def <- phonemes %>%
    filter(Phoneme == phoneme) %>%
    transmute(Row = paste0(Feature, value)) %>%
    extract2("Row")

  # Columns to select
  times <- seq(from = start_time, length.out = length(feature_gradient))

  # All of the values in column-major ordering
  feature_values <- rep(feature_gradient, each = length(phoneme_def))

  # Update subset of feature matrix
  feat_matrix[phoneme_def, times] <- feature_values
  feat_matrix

}


