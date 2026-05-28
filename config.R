## PACKAGES -------------------------------------------------------------------

required_packages <- c(
  "dplyr", "haven", "here", "readr", "survey",
  "ggplot2", "patchwork", "ragg", "systemfonts"
)
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

library(here)

## PATHS ----------------------------------------------------------------------
# Set these before running the pipeline or rendering `manuscript.qmd`.

abcd_rds_path <- here("..", "..", "Dissertation Data", "sleep_data_complete_abcd70.rds")
nhanes_sleep_path <- here("..", "nhanes", "datasets", "NHANES Preliminary Day Level Output.csv")
nhanes_demo_paths <- c(
  here("..", "nhanes", "datasets", "DEMO_G.xpt"),
  here("..", "nhanes", "datasets", "DEMO_H.xpt")
)
ffcws_sleep_path <- here("..", "ffcws", "datasets", "ffcws-dataset-0.1.0.csv")
ffcws_demo_path <- here("..", "ffcws", "datasets", "ffcws-harmonized-dataset-0.1.0.csv")
pats_path <- here("..", "pats", "datasets", "pats-dataset-0.1.0.csv")

outputs_dir <- here("outputs")
