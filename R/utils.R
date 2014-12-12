
#' Squish values into a range
squish <- function(xs, lower, upper) {
  xs[xs < lower] <- lower
  xs[upper < xs] <- upper
  xs
}

#' Inject a key-value pair into a list
inject_key_value <- function(x, key, value) {
  x[[key]] <- value
  x
}

# Return all by the last item in vector
but_last <- function(xs) head(xs, -1)

#' Append an item onto a list
set_tail <- function(x, y) {
  x[[length(x) + 1]] <- y
  x
}

#' Make an inventory of characters in a string
#' @param word a string
#' @return a sorted vector of unique characters in the string
#' @examples
#' str_inventory("letters")
#' # c("e", "l", "r", "s", "t")
str_inventory <- function(word) {
  str_tokenize(word) %>% unique %>% sort
}

#' Convert a string into a vector of characters
#' @inheritParams str_inventory
#' @return a vector of characters in the string
#' @examples
#' str_tokenize("letters")
#' # c("l", "e", "t", "t", "e", "r", "s")
str_tokenize <- function(word) {
  str_extract_all(word, ".") %>% unlist
}

#' Convert all characters in a string to hyphens
#' @param xs a character vector
#' @return a vector with the same number of strings (and characters) as the
#'   input, but with all characters converted to hyphens
#' @examples
#' str_censor(c("", "a", "ab"))
#' # c("", "-", "--")
str_censor  <- function(xs) {
  str_replace_all(xs, ".", "-")
}

#' Add silence to start and end of word
#' @inheritParams str_censor
#' @return the inputted strings but with silence characters ("-") added to
#'   beginning and end of word.
#' @examples
#' str_wrap_silence(c("", "a", "ab"))
#' # c("--", "-a-", "-ab-")
str_wrap_silence <- function(xs) {
  sprintf("-%s-", xs)
}
