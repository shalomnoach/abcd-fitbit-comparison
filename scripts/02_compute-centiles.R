# COMPUTE EMPIRICAL CENTILES ---------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))

library(dplyr)
library(readr)
library(survey)

cohorts <- read_csv(
  file.path(data_sets_dir, "cohorts.csv"),
  show_col_types = FALSE
)

empirical_centiles <- bind_rows(
  compute_empirical_summary(filter(cohorts, dataset == "ABCD")),
  compute_survey_summary(filter(cohorts, dataset == "NHANES")),
  compute_empirical_summary(filter(cohorts, dataset == "FFCWS")),
  compute_empirical_summary(filter(cohorts, dataset == "PATS"))
)

print(empirical_centiles, n = 38)
