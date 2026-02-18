# CLAUDE.md — jk_rscripts

This file describes the codebase structure, conventions, and development context for AI assistants working in this repository.

---

## Overview

This is a personal collection of R scripts authored by **Jan Korevaar** for data preparation, analysis, and reporting in the context of agricultural development programmes (primarily One Acre Fund Kenya). The scripts cover:

- **Farmer roster preparation** — aggregating client-level data to group and site summaries
- **Repayment analysis** — tracking weekly cumulative loan repayment trajectories
- **Product adoption summaries** — summarising uptake of specific agricultural inputs by site and district
- **Survey data preparation and analysis** — cleaning, categorising, and exploring field survey responses
- **Field leader data extraction** — merging Excel check-in sheets into a single dataset

There is no build system, package manager, or test suite. All scripts are standalone and run interactively in R (or RStudio).

---

## Repository Structure

```
jk_rscripts/
├── README.md                          # Minimal project readme
├── CLAUDE.md                          # This file
│
├── prep_roster_5_LR15.R               # Roster prep for Long Rain 2015 season
├── prep_roster_5_LR16.R               # Roster prep for Long Rain 2016 season
├── prep_roster_5_LR17.R               # Roster prep for Long Rain 2017 season
│
├── prep_vertical_repayment.R          # Weekly cumulative repayment trajectory analysis
├── prep_product_summary.R             # Product adoption summary by site and district (LR17)
├── prep_webuye_maize_marketing_survey.R  # Survey data prep for Webuye maize marketing survey
│
├── extract_fl_check-in.R              # Batch extraction of Field Leader check-in Excel files
│
└── exploratory_script.Rmd             # R Markdown exploratory analysis (harvest quality survey)
```

---

## Script Descriptions

### `prep_roster_5_LR*.R` (LR15, LR16, LR17)

**Purpose:** Prepares "Roster 5" client-level data for three consecutive long-rain seasons. Aggregates to group-level and site-level summary tables.

**Input:** CSV exported from the OAF internal system (e.g. `Season Clients Detailed_LR15.csv`)

**Output:**
- `group.clients.5_LR*.csv` — group-level summary
- `site.clients.5_LR*.csv` (or `site.clients.5.csv`) — site-level summary

**Key computed variables:**
- `distinct.site` / `distinct.group` — concatenated unique identifiers
- `new.client` / `ret.client` — binary flags for new vs. returning members
- `client.qualified` — whether a client repaid ≥ KES 500
- `maize.client` / `maize.client.qualified` — maize-specific participation flags (LR16, LR17)
- `planting.group` / `planting.group.r` — scoring of group planting intensity (LR16, LR17)

**Note on differences between seasons:**
- LR15: `NewMember` is a string `"True"`/`"False"`; no maize-specific columns
- LR16/LR17: `NewMember` is boolean; maize column names differ between seasons (check the `mutate()` block carefully when updating for a new season)

---

### `prep_vertical_repayment.R`

**Purpose:** Builds a weekly repayment spread and cumulative repayment percentage table for a vertical repayment pilot.

**Inputs:**
- `Detailed_20161204-051246.csv` — repayment transaction detail
- `Season Clients Detailed_20161204-055412.csv` — client roster

**Key logic:**
1. Classifies each repayment transaction into a week number (weeks 0–61) relative to a hardcoded start date (`st.date <- "2015-10-12"`)
2. Spreads payments into a wide format (one column per week)
3. Calculates cumulative sum per client per week
4. Divides cumulative repaid by `TotalCredit` to get `%` repaid

**Note:** The week classification is done via 61 explicit conditional assignments — a candidate for refactoring with `cut()` or `findInterval()` if updated. The script ends with an incomplete `ggplot` pipeline (`filter()` and `select()` have no arguments).

---

### `prep_product_summary.R`

**Purpose:** Produces site- and district-level product adoption summaries for LR17.

**Input:** `Season Clients Detailed_Nov_LR17.csv`

**Outputs:**
- `d_client_adp.csv` — district-level client adoption rates
- `d_prod_adp.csv` — district-level average quantities per adopter
- `s_client_adp.csv` — site-level client adoption rates
- `s_prod_adp.csv` — site-level average quantities per adopter

**Key logic:**
- Columns 37–80 of the roster are product quantity fields
- `client_adopt()` converts any positive quantity to a binary 1
- Uses `summarize_each()` / `mutate_each()` (deprecated in modern dplyr — update to `across()` if upgrading dplyr)

---

### `prep_webuye_maize_marketing_survey.R`

**Purpose:** Cleans and prepares a post-harvest maize marketing survey for analysis.

**Input:** `forms.csv` (raw survey export)

**Outputs:**
- `data.csv` — cleaned, filtered survey data (consent == "Yes" only)
- `question_labels.csv` — scaffold file for labelling questions and specifying analysis type

**Key helper functions:**
- `ctgrz_oth(data, oth.col, oth.pat, r.col, replace.pat)` — categorises free-text "Other" responses by pattern-matching and appending a standardised label
- `ql_create(data, rep.vars)` — generates a question labels template from multi-response columns
- `looksy(data, x)` — quick inspection helper: prints sorted non-NA values of a column

**Workflow:** Run this script first, fill in `question_labels.csv`, then run `exploratory_script.Rmd`.

---

### `extract_fl_check-in.R`

**Purpose:** Reads all Field Leader check-in Excel files from a folder and merges them into a single CSV.

**Input:** `*.xl*` files in `Field Leader Check-Ins/` directory

**Output:** `test.check-in.csv`

**Key logic:**
- Skips files with `~$` in the name (open/temporary Excel files)
- Reads rows 2–3 for date and district metadata, then from row 6 onward for the main data
- Date is stored as an Excel serial number; converted with `origin = "1899-12-30"`
- Uses `dplyr::bind_rows()` to accumulate across files

---

### `exploratory_script.Rmd`

**Purpose:** R Markdown report for exploratory analysis of a harvest quality survey. Outputs a Word document.

**Input:** `data.csv` and `question_labels_fin.csv` (from the same working directory)

**Key helper functions:**
- `discrete(data, variable, treatment)` — frequency tables + chi-square test + proportional bar chart for categorical variables
- `continues(data, variable, treatment, lrm)` — basic stats table + boxplot + density chart for continuous variables; optionally runs a linear regression
- `bsst_m(data, variable, treatment, lrm)` — multi-variable version of `continues()`
- `bsst_initial(data, vars)` — simple summary stats (no treatment split)
- `ql_replace(data, rep.vars, qst_labels)` — renames columns and recodes values using a question labels lookup table

**Loop logic:** Iterates over `question_labels` rows; dispatches to `discrete()` or `continues()` based on whether `analysis1` column contains `"DISCR"` or `"CONTI"`.

---

## R Libraries Used

| Library | Purpose |
|---|---|
| `dplyr` | Data manipulation (filter, mutate, summarize, group_by) |
| `tidyr` | Reshaping (spread, gather, unnest) |
| `ggplot2` | Visualisation |
| `lubridate` | Date handling |
| `data.table` | Fast cumulative sum operations |
| `broom` | Tidy model output |
| `lsmeans` | Least-squares means (exploratory script) |
| `knitr` / `printr` | Table rendering in R Markdown |
| `stringr` | String manipulation |
| `scales` | Axis formatting in ggplot2 |
| `GGally` | Correlation matrix plots |
| `xlsx` / `openxlsx` | Reading Excel files |

---

## Conventions

### Script structure
Every standalone `.R` script follows this pattern:
1. Clear environment: `rm(list = ls())`
2. Clear console: `cat("\014")`
3. Load libraries
4. Set working directory (`setwd(...)`)
5. Load data
6. Define helper functions (if any)
7. Prep / transform data
8. Write output CSV(s)

### Variable naming
- `snake.case` with dots (e.g. `distinct.site`, `new.trans.size`) — consistent throughout
- Unique identifiers are constructed by string concatenation: `paste(DistrictName, SiteName, sep = "")`
- Season suffixes: `_LR15`, `_LR16`, `_LR17` (Long Rain + year)

### Working directories
All scripts use **absolute, hardcoded `setwd()` paths** (Windows `D:/` or `C:/` paths). When running on a different machine, update `setwd()` and the input file names at the top of each script before execution.

### Output files
All outputs are written with `write.csv(..., row.names = FALSE)`. No output directory is created automatically; outputs land in the current working directory.

---

## Known Issues / Technical Debt

- **Hardcoded paths** — `setwd()` calls reference local Windows paths and must be updated per environment.
- **`summarize_each()` / `mutate_each()`** — these are deprecated in dplyr ≥ 1.0. Use `across()` if upgrading.
- **`tbl_df()`** — deprecated wrapper; use `as_tibble()` instead.
- **Week classification in `prep_vertical_repayment.R`** — 61 explicit conditional blocks; refactorable with `cut()`.
- **Incomplete pipeline** — the final `ggplot` section of `prep_vertical_repayment.R` has empty `filter()` and `select()` calls and a broken pipe (`y <- ggplot(., ...)` without a pipe leading into it); the script does not finish cleanly.
- **`lsmeans`** — package superseded by `emmeans`; update `library(lsmeans)` to `library(emmeans)` if needed.
- **Java heap** — `options(java.parameters = "-Xmx8000m")` must be set before loading `xlsx`; this is correctly done in the scripts that use it.

---

## Domain Context

- **OAF** = One Acre Fund, an agricultural NGO operating in East Africa
- **OAFID** = unique client identifier in OAF systems
- **LR** = Long Rain (planting season, roughly March–August in Kenya)
- **Roster** = the full client list exported from OAF's internal MIS, containing credit, repayment, and product data per client
- **Qualification** = a client who has repaid ≥ KES 500 is considered "qualified" for certain programme benefits (e.g. maize marketing)
- **District / Site / Group** = OAF organisational hierarchy (District → Site → Group → Client)
- **Vertical repayment pilot** = a repayment structure experiment in Tinderet district
- **Webuye** = a district in western Kenya; the maize marketing survey covers this area

---

## Running Scripts

There is no unified entry point or runner. To run a script:

1. Open R or RStudio
2. Update `setwd()` and input file paths at the top of the script
3. Source the entire file or run sections interactively

For the R Markdown report:
```r
rmarkdown::render("exploratory_script.Rmd")
```
This produces a `.docx` Word document.
