#' retrace.
#'
#' @name retrace
#' @docType package
#' @import R6 dplyr magrittr assertthat digest
NULL

#' 1986 Phoneme definitions
#' @docType data
#' @name phonemes86
#' @usage phonemes86
#' @format A \code{data.frame} with 98 observations.
#' @examples
#' phonemes86
NULL

phonemes <-
  structure(list(Phoneme = c("p", "p", "p", "p", "p", "p", "p",
  "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
  "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
  "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
  "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
  "p", "p", "p", "p", "b", "b", "b", "b", "b", "b", "b", "b", "b",
  "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b",
  "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b",
  "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b",
  "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b",
  "b", "b", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
  "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
  "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
  "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
  "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
  "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d",
  "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d",
  "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d",
  "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d",
  "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "k", "k",
  "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k",
  "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k",
  "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k",
  "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k", "k",
  "k", "k", "k", "k", "k", "k", "k", "k", "k", "g", "g", "g", "g",
  "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g",
  "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g",
  "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g",
  "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g", "g",
  "g", "g", "g", "g", "g", "g", "g", "s", "s", "s", "s", "s", "s",
  "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s",
  "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s",
  "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s",
  "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s", "s",
  "s", "s", "s", "s", "s", "S", "S", "S", "S", "S", "S", "S", "S",
  "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S",
  "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S",
  "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S",
  "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S",
  "S", "S", "S", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r",
  "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r",
  "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r",
  "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r",
  "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r",
  "r", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l",
  "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l",
  "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l",
  "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l",
  "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "a",
  "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
  "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
  "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
  "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
  "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "i", "i", "i",
  "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i",
  "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i",
  "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i",
  "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i", "i",
  "i", "i", "i", "i", "i", "i", "i", "i", "u", "u", "u", "u", "u",
  "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u",
  "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u",
  "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u",
  "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u", "u",
  "u", "u", "u", "u", "u", "u", "^", "^", "^", "^", "^", "^", "^",
  "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^",
  "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^",
  "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^",
  "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^", "^",
  "^", "^", "^", "^", "-", "-", "-", "-", "-", "-", "-", "-", "-",
  "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",
  "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",
  "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",
  "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",
  "-", "-"), Feature = c("Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Acute", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
  "Acute", "Acute", "Acute", "Burst", "Burst", "Burst", "Burst",
  "Burst", "Burst", "Burst", "Burst", "Burst", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
  "Consonantal", "Consonantal", "Diffuse", "Diffuse", "Diffuse",
  "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
  "Power", "Power", "Power", "Power", "Power", "Power", "Power",
  "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
  "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Voiced",
  "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
  "Voiced"), Value = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L,
  1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
  8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L,
  6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L,
  4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L,
  2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
  0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L,
  7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L,
  5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L,
  3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L,
  1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
  8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L,
  6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L,
  4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L,
  2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
  0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L,
  7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L,
  5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L,
  3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L,
  1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
  8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L,
  6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L,
  4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L,
  2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
  0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L,
  7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L,
  5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L,
  3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L,
  1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
  8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L,
  6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L,
  4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L,
  2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
  0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L,
  7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L,
  5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L,
  3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L,
  1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
  8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L,
  6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L,
  4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L,
  2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
  0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L,
  7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L,
  5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L,
  3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L,
  1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
  8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L,
  6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L,
  4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L,
  2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
  0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L,
  7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L,
  5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L,
  3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L,
  1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
  8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L,
  6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L,
  4L, 5L, 6L, 7L, 8L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L, 1L,
  2L, 3L, 4L, 5L, 6L, 7L, 8L), Weight = c(0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0.2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0.1,
  0.3, 1, 0.3, 0.1, 0, 0, 0, 0, 0, 0, 0.2, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0.1, 0.3, 1, 0.3, 0.1, 0, 0, 0, 0, 0, 0, 1, 0.2, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.3, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.3, 1, 0.3, 0.1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.75, 0.5, 0.25, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,
  0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0.25, 0.5, 0.75,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0.5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0.3,
  0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0.1, 0.3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0.3, 1, 0.3, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 1, 0.3, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0)), .Names = c("Phoneme", "Feature", "Value", "Weight"
  ), class = "data.frame", row.names = c(NA, -945L))
