read.csv("inst/phon_1986.csv", stringsAsFactors = FALSE) %>%
  dput(file = "inst/phonemes.R")
