## RUN FULL PIPELINE ----------------------------------------------------------
# Run from the repo root after setting the paths in config.R.

library(here)

source(here("scripts", "01_process-abcd.R"))
source(here("scripts", "02_process-nhanes.R"))
source(here("scripts", "03_process-ffcws.R"))
source(here("scripts", "04_process-pats.R"))
source(here("scripts", "05_compute-diffs.R"))
source(here("scripts", "06_generate-figure.R"))
