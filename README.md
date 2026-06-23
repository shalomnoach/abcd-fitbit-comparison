# ABCD Fitbit Sleep Duration Comparison

Reproducible analysis code and manuscript source for:

**Jaffe SN, McGlinchey EL. "Comparing Objectively Estimated Sleep Duration in ABCD With Published Norms"**

This repository reproduces the analytic outputs, Figure 1, Table 1, and the manuscript PDF comparing Fitbit-derived sleep duration in the ABCD Study with published objective pediatric sleep-duration benchmarks (NHANES, FFCWS, PATS, and the Galland et al. meta-analytic curve), using normal location-scale GAMLSS models fit with `gamlss2`.

Restricted raw data are **not** distributed with this repository. Reproduction requires approved access to ABCD, FFCWS, and PATS, plus download of the NHANES files listed below.

## Pipeline

The analysis runs as three sequential scripts. (All 3 can be run in order using `scripts/run-all.R`)

1.  `scripts/01_prepare-cohorts.R`: standardize ABCD, NHANES, FFCWS, and PATS into shared participant-level tables and 1-year age windows.
2.  `scripts/02_compute-centiles.R`: compute age-windowed empirical centiles (survey-weighted for NHANES) used as model-selection targets.
3.  `scripts/03_fit-gamlss.R`: fit candidate normal and log-normal GAMLSS models, select a common specification by mean GAIC, predict percentile curves, build the reference comparisons and Table 1, and render Figure 1.

Shared functions live in `scripts/helpers.R` (formatting, age windows, centiles, comparisons) and `scripts/gamlss-helpers.R` (the `gamlss2` families and model-fitting framework).

## Requirements

- R
- Quarto
- Access to the external datasets listed in [Data access](#data-access)

When `config.R` is sourced, missing analysis packages are installed automatically.

## Data access {#data-access}

| Dataset | Access details |
|------------------------------------|------------------------------------|
| **ABCD Study (release 7.0)** | Apply for a Data Use Certification through the [NIH Brain Development Cohorts (NBDC) Data Hub](https://www.nbdc-datahub.org/). The pipeline reads the release 7.0 Fitbit (`novel_technologies/fitbit/fitbit_ss_sleep_day.parquet`) and demogra (`phenotype/ab_g_stc.parquet`) files directly. |
| **NHANES 2011-2014 sleep data** | Download `NHANES Preliminary Day Level Output.csv` from the NCI/ICPSR [NHANES 2011-2014 Sleep Data](https://www.datalumos.org/datalumos/project/240826/version/V3/view) release, the CDC demographics files [DEMO_G.xpt (2011-2012)](https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/DEMO_G.htm), and [DEMO_H.xpt (2013-2014)](https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DEMO_H.htm). |
| **FFCWS** | Request access through the [National Sleep Research Resource (NSRR)](https://sleepdata.org/datasets/ffcws) and download `ffcws-dataset-0.1.0.csv` and `ffcws-harmonized-dataset-0.1.0.csv`. |
| **PATS** | Request access through the [NSRR](https://sleepdata.org/datasets/pats) and download `pats-dataset-0.1.0.csv`. |

The data from the individual published studies used by Galland et al. in their meta-analysis are included in `data/reference/galland.csv`.

## Reproducing the analysis

### 1. Configure local dataset paths

Open `config.R` and update `abcd_fitbit_path`, `abcd_stc_path`, `nhanes_sleep_path`, `nhanes_demo_paths`, `ffcws_sleep_path`, `ffcws_demo_path`, and `pats_path`. All paths are resolved relative to the repository root with `here::here()`.

### 2. Generate analysis outputs

From the repository root, run:

``` bash
Rscript scripts/run-all.R
```

Standardized participant-level cohorts are written to `data-sets/cohorts.csv` (not included here per the data-use agreements). The aggregate outputs are:

- `outputs/modeled-centiles.csv`: modeled GAMLSS percentile curves
- `outputs/comparison-table.csv`: per-age differences against the reference curves (Table 1)
- `outputs/figure1.png` and `outputs/figure1.tiff`: Figure 1

### 3. Render the manuscript

``` bash
quarto render manuscript.qmd
```

This produces `manuscript.pdf` in the repository root.

## Repository structure

``` text
abcd-fitbit-comparison/
|-- config.R
|-- comparison-letter-references.bib
|-- data/reference/galland.csv
|-- outputs/
|-- scripts/
|   |-- helpers.R
|   |-- gamlss-helpers.R
|   |-- 01_prepare-cohorts.R
|   |-- 02_compute-centiles.R
|   |-- 03_fit-gamlss.R
|   `-- run-all.R
|-- manuscript.qmd
`-- README.md
```