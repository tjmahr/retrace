require("digest")
require("R6")
require("assertthat")
require("stringr")
require("ggplot2")
require("dplyr", warn.conflicts = FALSE)
require("magrittr")

#' @export
trace_params <- list(
  # bottom-up excitation
  excite_feat_phon = .02,
  excite_phon_word = .05,
  # top-down excitation
  excite_word_phon = .03,
  excite_phon_feat = .00,
  # lateral inhibition
  inhibit_feat = .04,
  inhibit_phon = .04,
  inhibit_word = .04,
  # decay
  decay_feat = .01,
  decay_phon = .03,
  decay_word = .05
)

but_first <- . %>% tail(-1)

# 5 up, peak activation, 5 down
create_feature_gradient <- function(peak = 1, skirt = 5) {
  assert_that(peak != 0)
  # Add 1 to skirt to include the peak. Add another 1 because we exclude the
  # first 0.
  ramp <- seq(0, peak, length.out = skirt + 1 + 1) %>%
    but_first %>% but_last %>% round(2)

  c(ramp, peak, rev(ramp))
}
