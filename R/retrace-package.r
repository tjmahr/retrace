#' retrace.
#'
#' @name retrace
#' @docType package
#' @import R6 magrittr assertthat digest
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
  structure(list(Phoneme = c("p", "b", "t", "d", "k", "g", "s", "S",
    "r", "l", "a", "i", "u", "^", "p", "b", "t", "d", "k", "g", "s",
    "S", "r", "l", "a", "i", "u", "^", "p", "b", "t", "d", "k", "g",
    "s", "S", "r", "l", "a", "i", "u", "^", "p", "b", "t", "d", "k",
    "g", "s", "S", "r", "l", "a", "i", "u", "^", "p", "b", "t", "d",
    "k", "g", "s", "S", "r", "l", "a", "i", "u", "^", "p", "b", "t",
    "d", "k", "g", "s", "S", "r", "l", "a", "i", "u", "^", "p", "b",
    "t", "d", "k", "g"), Feature = c("Power", "Power", "Power", "Power",
    "Power", "Power", "Power", "Power", "Power", "Power", "Power",
    "Power", "Power", "Power", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
    "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Vocalic",
    "Vocalic", "Vocalic", "Vocalic", "Vocalic", "Diffuse", "Diffuse",
    "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
    "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse", "Diffuse",
    "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
    "Acute", "Acute", "Acute", "Acute", "Acute", "Acute", "Acute",
    "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
    "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Consonantal",
    "Consonantal", "Consonantal", "Consonantal", "Consonantal", "Voiced",
    "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced",
    "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Voiced", "Burst",
    "Burst", "Burst", "Burst", "Burst", "Burst"), value = c(4L, 4L,
    4L, 4L, 4L, 4L, 6L, 6L, 7L, 7L, 8L, 8L, 8L, 7L, 1L, 1L, 1L, 1L,
    1L, 1L, 4L, 4L, 7L, 7L, 8L, 8L, 8L, 8L, 7L, 7L, 7L, 7L, 2L, 2L,
    7L, 6L, 1L, 2L, 2L, 8L, 6L, 5L, 2L, 2L, 7L, 7L, 3L, 3L, 8L, 4L,
    2L, 4L, 1L, 8L, 2L, 1L, 8L, 8L, 8L, 8L, 8L, 8L, 5L, 5L, 3L, 3L,
    1L, 1L, 1L, 1L, 1L, 7L, 1L, 7L, 1L, 7L, 1L, 1L, 8L, 8L, 8L, 8L,
    8L, 8L, 8L, 7L, 6L, 5L, 4L, 3L)), .Names = c("Phoneme", "Feature",
    "value"), row.names = c(NA, 90L), class = "data.frame")

