squish <- function(xs, lower, upper) {
  xs[xs < lower] <- lower
  xs[upper < xs] <- upper
  xs
}

inject_key_value <- function(x, key, value) {
  x[[key]] <- value
  x
}

make_dashes <- function(units) str_replace_all(units, ".", "-")

but_last <- function(xs) head(xs, -1)


inventory_sounds <- function(word) {
  tokenize_sounds(word) %>% unique %>% sort
}

tokenize_sounds <- function(word) {
  str_extract_all(word, ".") %>% unlist
}
