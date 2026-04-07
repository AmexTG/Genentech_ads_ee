Pharmaverse Analytics Programming Assessment
Author: Emmanuel Endeshaw
---
## Overview
This repository contains solutions for the Analytical Data Science Programmer
coding assessment using the Pharmaverse ecosystem. The outputs follow clinical
programming conventions and CDISC-aligned structures.
---
## Question 1 — SDTM Creation
  Input:
  * pharmaverseraw::ds_raw
  Output:
  * SDTM.DS dataset
  * ds.xpt
  * ds_listing.html
  Script:
  * 01_create_ds_domain.R
---

Question 2 — ADSL Creation
  Input:
  * pharmaversesdtm domains (DM, VS, EX, DS, AE)
  Output:
  * ADSL dataset
  * adsl.xpt
  * adsl_listing.html
  Script:
  * question_2_adsl/create_adsl.R

## Question 3 — AE Summary Table (TLG)
  Treatment-emergent adverse events summary
  Rows:
  * AETERM or AESOC
  Columns:
  * Treatment (ACTARM)
  Cell values:
  * Count (n) and percentage (%)
  Output:
  * 3_1_table_10_teae_.pdf
  Script:
  * question_3_tlg/01_create_ae_summary_table.R 
  ---
  
## Question 3 — Visualizations
  Plot 1:
  * AE severity distribution by treatment
  Plot 2:
  * Top 10 most frequent adverse events
  * 95% confidence intervals (Clopper-Pearson)
  Outputs:
  * 3_2_1_ae_severity_by_treatment_counts.png
  * 3_2_2_top10_adverse_events.png
  Script:
  * question_3_tlg/02_create_visualizations.R
---

## Question 4 — GenAI Clinical Assistant
  Natural language queries mapped to ADAE dataset variables.
  Examples:
  * "Show severe events" → AESEV
  * "Headache" → AETERM
  * "Cardiac" → AESOC
  Tools:
  * Python
  * Pandas
  * LLM mapping
---

## Prerequisites
 [RStudio](https://posit.cloud) (version R 4.5.3)
 
## R Package Requirements
  dplyr
  tidyr
  ggplot2
  gt
  gtsummary
  haven
  binom
  admiral
  pharmaverseadam
  pharmaversesdtm
---
## Reproducibility
  Each script runs independently. Set the working directory to the script location before running:
  setwd("/cloud/project/question_2_adam").Outputs will be written to the current working directory.
  Example:
  Rscript create_adsl.R
  Rscript ae_summary.R
  Rscript 02_create_visualization.R
  Outputs are written to the local working directory.
---
## Execution and Logs

Each log runs independently. Set the working directory to the script
location before running. All outputs and logs are written to the current
working directory.

Example:
  ADaM_log_run.R
  01_tlf_log_run.R
  02_tlf_log_run.R

Log files capture:

* Program start and end time
* Package loading
* Data derivations
* Controlled terminology warnings
* Output creation
* Row and column counts

Example log output:

  ADSL RUN START: 2026-03-22 10:12:03
  Reading SDTM domains
  Deriving variables
  Writing adsl.xpt
  Rows in ADSL: 254
  Columns in ADSL: 48
  ADSL RUN END: 2026-03-22 10:12:05

log.log file(ex: adsl_domain_log.log) includes: 
  Message/Error Log
  Captures warnings, messages, and errors
  Logs provide:
---
