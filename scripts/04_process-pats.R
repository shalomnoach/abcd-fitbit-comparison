# LOAD PACKAGES ----------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))

library(dplyr)
library(readr)

dir.create(outputs_dir, showWarnings = FALSE)

## LOAD PATS DATA -------------------------------------------------------------

pats_sleep <- read_csv(pats_path, show_col_types = FALSE) |>
  filter(
    !is.na(studyinfo_age_at_randomization),
    !is.na(avgsleepperiodduration)
  ) |>
  mutate(
    mean_duration = avgsleepperiodduration / 60,
    age = studyinfo_age_at_randomization
  )

## COMPUTE PATS PERCENTILES ---------------------------------------------------

pats_percentiles <- compute_age_percentiles(
  pats_sleep,
  age_col = "age",
  value_col = "mean_duration",
  bin_width = 1,
  min_n = 5
)

## SAVE OUTPUT ----------------------------------------------------------------

write_csv(pats_percentiles, file.path(outputs_dir, "pats-percentiles.csv"))
