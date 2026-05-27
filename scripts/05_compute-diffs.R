# LOAD PACKAGES ----------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))

library(dplyr)
library(readr)

## LOAD INPUTS ----------------------------------------------------------------

abcd_percentiles <- read_csv(
  file.path(outputs_dir, "abcd-percentiles.csv"),
  show_col_types = FALSE
)
nhanes_percentiles <- read_csv(
  file.path(outputs_dir, "nhanes-percentiles.csv"),
  show_col_types = FALSE
)
ffcws_percentiles <- read_csv(
  file.path(outputs_dir, "ffcws-percentiles.csv"),
  show_col_types = FALSE
)
pats_percentiles <- read_csv(
  file.path(outputs_dir, "pats-percentiles.csv"),
  show_col_types = FALSE
)

iglowstein <- read_csv(
  here("data", "reference", "iglowstein.csv"),
  show_col_types = FALSE
)
williams <- read_csv(
  here("data", "reference", "williams.csv"),
  show_col_types = FALSE
)

## COMPUTE DIFFERENCES --------------------------------------------------------

galland_for_diff <- tibble(age = seq(0, 19, by = 0.1)) |>
  mutate(p50 = 9.02 - 1.04 * ((age / 10)^2 - 0.83))

iglowstein_for_diff <- iglowstein |> transmute(age, p50)
williams_for_diff <- williams |> transmute(age, p50)
nhanes_for_diff <- nhanes_percentiles |> transmute(age = age_mid, p50)
ffcws_for_diff <- ffcws_percentiles |> transmute(age = age_mid, p50)
pats_for_diff <- pats_percentiles |> transmute(age = age_mid, p50)

diffs <- list(
  iglowstein = signed_diff_summary(abcd_percentiles, iglowstein_for_diff),
  williams = signed_diff_summary(abcd_percentiles, williams_for_diff),
  nhanes = signed_diff_summary(abcd_percentiles, nhanes_for_diff),
  ffcws = signed_diff_summary(abcd_percentiles, ffcws_for_diff),
  pats = signed_diff_summary(abcd_percentiles, pats_for_diff),
  galland = signed_diff_summary(abcd_percentiles, galland_for_diff)
)

## SAVE OUTPUT ----------------------------------------------------------------

saveRDS(diffs, file.path(outputs_dir, "diffs.rds"))
