# Download lines from cTRACE that define feature names
library("magrittr")
library("stringr")
library("dplyr")
mc <- readLines("https://raw.githubusercontent.com/tjmahr/TRACE/master/gfv.c")

# Find lines where phonemes are defined
phon_comment <- "^/[*]\\S+[*]/"
starts <- mc %>% str_detect(phon_comment) %>% which
ends <- starts + 6

# Convert lines of code in the source file into a data-frame with phoneme
# definitions
extract_phoneme_definition <- function(start, end) {
  # Get phoneme from line 1
  get_phoneme <- . %>% extract(1) %>%
    str_extract(phon_comment) %>%
    str_replace_all("\\s", "") %>%
    str_replace_all("/[*]", "") %>%
    str_replace_all("[*]/", "")

  # Keep only digits, periods and commas
  clean_cells <- . %>% str_replace_all("\\s", "") %>%
    str_replace_all("[^0-9,\\.]", "") %>%
    str_replace_all(",$", "")

  lines <- mc[start:end]
  phoneme <- lines %>% get_phoneme

  feature_names <- c("Power", "Vocalic", "Diffuse", "Acute",
                     "Consonantal", "Voiced", "Burst")

  phoneme_def <- lines %>% clean_cells %>%
    read.csv(text = ., header = FALSE, stringsAsFactors = FALSE) %>%
    # Add informative names
    set_colnames(paste0("value", 8:0)) %>%
    mutate(Feature = feature_names, Phoneme = phoneme) %>%
    # Convert to long format
    tidyr::gather(Value, Weight, -Feature, -Phoneme) %>%
    mutate(Value = tidyr::extract_numeric(Value)) %>%
    arrange(Feature, Value) %>%
    select(Phoneme, Feature, Value, Weight)

  phoneme_def
}

phoneme_inventory <- Map(extract_phoneme_definition, starts, ends) %>%
  rbind_all

weird_ones <- c("vow", "L", "dif", "as", "vs", "bs", "p/t")
oddballs <- phoneme_inventory %>% filter(is.element(Phoneme, weird_ones)) %T>%
  write.csv(file = "data-raw/phon_1986_weird.csv", row.names = FALSE)

phoneme_inventory %<>% filter(!is.element(Phoneme, weird_ones)) %T>%
  write.csv(file = "data-raw/phon_1986.csv", row.names = FALSE)
