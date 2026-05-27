# ABCD Fitbit Sleep Duration Comparison

Reproducible analysis code and manuscript source for:

**Jaffe SN, McGlinchey EL. "Comparing Objectively Estimated Sleep Duration in ABCD With Published Norms"**

This repository reproduces the analytic outputs, primary figure, and manuscript PDF comparing Fitbit-derived sleep duration in the ABCD Study with published pediatric sleep-duration references.

Restricted raw data are **not** distributed with this repository. Reproduction requires approved access to ABCD, FFCWS, and PATS, plus download of the NHANES files listed below.

## Project overview

The workflow has two stages:

1.  Prepare a local ABCD sleep `.rds` file from ABCD (release 7.0) `.parquet` files.
2.  Run the analysis pipeline to generate percentile curves, comparison summaries, Figure 1, and the manuscript PDF.

Included in the repository:

- `scripts/00_prepare-abcd.R`: one-time preparation of the restricted ABCD source files
- `scripts/01_process-abcd.R` to `scripts/06_generate-figure.R`: analysis pipeline
- `config.R`: local file paths and analysis package setup
- `data/reference/`: included reference curves reconstructed from published sources
- `outputs/`: generated analysis products
- `manuscript.qmd`: manuscript source

## Requirements

- R
- Quarto
- Access to the external datasets listed in [Data access](#data-access)
- For the ABCD preparation step: `arrow`, `dplyr`, and `here`

When `config.R` is sourced, missing analysis packages used by the main pipeline are installed automatically.

## Data access

| Dataset | Access details |
|------------------------------------|------------------------------------|
| **ABCD Study (release 7.0)** | Apply for a Data Use Certification through the [NIH Brain Development Cohorts (NBDC) Data Hub](https://www.nbdc-datahub.org/). |
| **NHANES 2011-2014 sleep data** | Download `NHANES Preliminary Day Level Output.csv` from the NCI/ICPSR [NHANES 2011-2014 Sleep Data](https://www.datalumos.org/datalumos/project/240826/version/V3/view) release, the CDC demographics files [DEMO_G.xpt (2011-2012)](https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/DEMO_G.htm), and [DEMO_H.xpt (2013-2014)](https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DEMO_H.htm). |
| **FFCWS** | Request access through the [National Sleep Research Resource (NSRR)](https://sleepdata.org/datasets/ffcws) and download `ffcws-dataset-0.1.0.csv` and `ffcws-harmonized-dataset-0.1.0.csv`. |
| **PATS** | Request access through the [NSRR](https://sleepdata.org/datasets/pats) and download `pats-dataset-0.1.0.csv`. |

The reconstructed data used for the Iglowstein and Williams comparison curves are already included in `data/reference/`.

## Reproducing the analysis

### 1. Prepare the ABCD sleep file

After obtaining ABCD access, download these release 7.0 files:

- `novel_technologies/fitbit/fitbit_ss_sleep_day.parquet`
- `phenotype/ab_g_stc.parquet`

If your local data live elsewhere, update the paths at the top of `scripts/00_prepare-abcd.R`, then run:

``` bash
Rscript scripts/00_prepare-abcd.R
```

This creates a local `sleep_data_complete_abcd70.rds` file used by the downstream pipeline. You only need to do this once.

### 2. Configure local dataset paths

Open `config.R` and update:

- `abcd_rds_path`
- `nhanes_sleep_path`
- `nhanes_demo_paths`
- `ffcws_sleep_path`
- `ffcws_demo_path`
- `pats_path`

All paths are resolved relative to the repository root with `here::here()`.

### 3. Generate analysis outputs

From the repository root, run:

``` bash
Rscript scripts/run-all.R
```

This writes the analysis products to `outputs/`, including:

- `abcd-percentiles.csv`
- `nhanes-percentiles.csv`
- `ffcws-percentiles.csv`
- `pats-percentiles.csv`
- `abcd-summary.rds`
- `diffs.rds`
- `figure1.png`

### 4. Render the manuscript

``` bash
quarto render manuscript.qmd
```

This produces `manuscript.pdf` in the repository root.

## Repository structure

``` text
abcd-fitbit-comparison/
|-- config.R
|-- data/reference/
|-- outputs/
|-- scripts/
|   |-- 00_prepare-abcd.R
|   |-- 01_process-abcd.R
|   |-- 02_process-nhanes.R
|   |-- 03_process-ffcws.R
|   |-- 04_process-pats.R
|   |-- 05_compute-diffs.R
|   `-- 06_generate-figure.R
|-- manuscript.qmd
`-- README.md
```