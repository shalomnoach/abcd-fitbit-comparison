## PACKAGES -------------------------------------------------------------------

required_packages <- c(
  "arrow", "dplyr", "fpc", "gamlss2", "ggplot2", "haven", "here",
  "metafor", "mgcv", "patchwork", "ragg", "readr", "survey", "systemfonts"
)
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

library(here)

## PATHS ----------------------------------------------------------------------
# Set these before running the pipeline or rendering `manuscript.qmd`.

abcd_fitbit_path <- here(
  "..", "Dissertation Data", "abcd", "concatenated",
  "novel_technologies", "fitbit", "fitbit_ss_sleep_day.parquet"
)
abcd_stc_path <- here(
  "..", "Dissertation Data", "abcd", "rawdata", "phenotype", "ab_g_stc.parquet"
)
nhanes_sleep_path <- here("..", "Dissertation", "nhanes", "datasets", "NHANES Preliminary Day Level Output.csv")
nhanes_demo_paths <- c(
  here("..", "Dissertation", "nhanes", "datasets", "DEMO_G.xpt"),
  here("..", "Dissertation", "nhanes", "datasets", "DEMO_H.xpt")
)
ffcws_sleep_path <- here("..", "Dissertation", "ffcws", "datasets", "ffcws-dataset-0.1.0.csv")
ffcws_demo_path <- here("..", "Dissertation", "ffcws", "datasets", "ffcws-harmonized-dataset-0.1.0.csv")
pats_path <- here("..", "Dissertation", "pats", "datasets", "pats-dataset-0.1.0.csv")

# Prepared participant-level data is written here and is gitignored; only the
# aggregate artifacts in outputs/ are committed.
data_sets_dir <- here("data-sets")
outputs_dir <- here("outputs")
