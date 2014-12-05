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
