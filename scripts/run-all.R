## RUN FULL PIPELINE -----------------------------------------------------------

library(here)

source(here("scripts", "01_prepare-cohorts.R"))
source(here("scripts", "02_compute-centiles.R"))
source(here("scripts", "03_fit-gamlss.R"))
