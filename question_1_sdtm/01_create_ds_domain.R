# ------------------------------------------------------------------------------
# Program:     create_ds.R
# Purpose:     Create SDTM Disposition (DS) domain from pharmaverseraw::ds_raw
#              using the sdtm.oak reusable algorithms.
#
# Author:       Emmanuel Endeshaw
# Date created: Mar21, 2026        
# Input:       pharmaverseraw::ds_raw
#              Controlled Terminology spec CSV (study-specific): sdtm_ct.csv
#
# Output:      ds (data.frame/tibble) with variables:
#              STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT,
#              VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
#
# References:
#  - SDTMIG v3.4 DS coding and DSSTDTC expectations
#  - sdtm.oak assign_no_ct, assign_ct, assign_datetime, derive_seq, derive_study_day
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(sdtm.oak)
  library(haven)
  library(tibble)
  library(labelled)
  library(pharmaverseraw)
  library(pharmaversesdtm)
})

# Define SDTM labels 
ds_labels <- list(
  STUDYID  = "Study Identifier",
  DOMAIN   = "Domain Abbreviation",
  USUBJID  = "Unique Subject Identifier",
  DSSEQ    = "Sequence Number",
  DSTERM   = "Reported Term for the Disposition Event",
  DSDECOD  = "Standardized Disposition Term",
  DSCAT    = "Category for Disposition Event",
  VISITNUM = "Visit Number",
  VISIT    = "Visit Name",
  DSDTC    = "Date/Time of Disposition Event",
  DSSTDTC  = "Start Date/Time of Disposition Event",
  DSSTDY   = "Study Day of Start of Disposition Event"
)

# ---- Input parameters --------------------------------------------

# Path to the study controlled terminology mapping file in sdtm.oak format.
ct_data <- read.csv("sdtm_ct.csv")

# Study day reference date from DM.
ref_day_var <- "RFSTDTC"

# Default category for these records.
default_dscat <- "DISPOSITION EVENT"

# Create TV domain visit mapping (VISIT and VISITNUM)
tv <- tibble(
  VISIT = c("SCREENING", "BASELINE", "VISIT 1", "VISIT 2"),
  VISITNUM = c(1, 2, 3, 4)
)

# ---- Read inputs -------------------------------------------------------------
ds_raw <- pharmaverseraw::ds_raw
# DM reference dataset used for study day derivation
dm <- pharmaversesdtm::dm

# Read controlled terminology specification
study_ct <- sdtm.oak::read_ct_spec("sdtm_ct.csv")

# ---- Create oak_id_vars in raw input for sdtm.oak joins ----------------------

ds_raw <- ds_raw %>%
  sdtm.oak::generate_oak_id_vars(
    raw_dat = .,
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

id_vars <- sdtm.oak::oak_id_vars()

# ID crosswalk to support STUDYID/USUBJID derivations 
id_xwalk <- ds_raw %>%
  dplyr::select(dplyr::all_of(c("oak_id", "raw_source", "patient_number", "STUDY", "PATNUM")))

# ---- Map Topic variable (DSTERM) ---------------------------------------------

ds <- sdtm.oak::assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "IT.DSTERM",
  tgt_var = "DSTERM",
  id_vars = id_vars
)

# ---- Map remaining required variables ----------------------------------------
# Fix CT formatting
ds_raw$IT.DSDECOD <- toupper(ds_raw$IT.DSDECOD)
ds_raw$IT.DSDECOD <- gsub("LOST TO FOLLOW UP", "LOST TO FOLLOW-UP", ds_raw$IT.DSDECOD)

# Remove non-DS term
ds_raw <- ds_raw[ds_raw$IT.DSDECOD != "RANDOMIZED", ]
# Assign CT
ds <- sdtm.oak::assign_ct(
  tgt_dat = ds,
  raw_dat = ds_raw,
  raw_var = "IT.DSDECOD",
  tgt_var = "DSDECOD",
  ct_spec = study_ct,
  ct_clst = "C66727",
  id_vars = id_vars
)

# VISIT and VISITNUM: attempt controlled terminology mapping from INSTANCE;
# if CT does not contain matches, ct_map returns uppercase raw values.
ds <- sdtm.oak::assign_ct(
  tgt_dat = ds,
  raw_dat = ds_raw,
  raw_var = "INSTANCE",
  tgt_var = "VISIT",
  ct_spec = study_ct,
  ct_clst = "VISIT",
  id_vars = id_vars
)

ds <- sdtm.oak::assign_ct(
  tgt_dat = ds,
  raw_dat = ds_raw,
  raw_var = "INSTANCE",
  tgt_var = "VISITNUM",
  ct_spec = study_ct,
  ct_clst = "VISITNUM",
  id_vars = id_vars
)

# Derive VISIT and VISITNUM for DS domain using lookup table (tv).
ds <- ds %>%
  mutate(
    VISIT = coalesce(VISIT, "DISPOSITION")
  ) %>%
  left_join(tv, by = "VISIT") %>%
  mutate(
    VISITNUM = coalesce(VISITNUM, 999)
  )

# DSDTC: collection date + time -> ISO8601
date_fmts <- c("m-d-y", "m/d/y", "d-m-y", "d/m/y", "y-m-d")
time_fmts <- c("H:M", "H:M:S")

ds <- sdtm.oak::assign_datetime(
  tgt_dat = ds,
  tgt_var = "DSDTC",
  raw_dat = ds_raw,
  raw_var = c("DSDTCOL", "DSTMCOL"),
  raw_fmt = list(date_fmts, time_fmts),
  raw_unk = c("UN", "UNK"),
  id_vars = id_vars
)
ds <- ds %>%
  select(-any_of(c("DSSTDTC")))

# DSSTDTC: disposition event date.
# Primary source is IT.DSSTDAT; optionally override with death date where present.
ds <- sdtm.oak::assign_datetime(
  tgt_dat = ds,
  tgt_var = "DSSTDTC",
  raw_dat = ds_raw,
  raw_var = "IT.DSSTDAT",
  raw_fmt = list(date_fmts),
  raw_unk = c("UN", "UNK"),
  id_vars = id_vars
)

# parse DEATHDT and coalesce into DSSTDTC when DEATHDT is present
ds <- sdtm.oak::assign_datetime(
  tgt_dat = ds,
  tgt_var = "DTHDTC_TMP",
  raw_dat = ds_raw,
  raw_var = "DEATHDT",
  raw_fmt = list(date_fmts),
  raw_unk = c("UN", "UNK"),
  id_vars = id_vars
)

# ---- Derive STUDYID, DOMAIN, USUBJID, DSCAT ----------------------------------

ds <- ds %>%
  left_join(id_xwalk, by = c("oak_id", "raw_source", "patient_number")) %>%
  mutate(
    STUDYID = STUDY,
    DOMAIN  = "DS",
    USUBJID = paste0(STUDY, "-", PATNUM),
    DSCAT   = default_dscat
  ) %>%
  select(-STUDY, -PATNUM)

# ---- Derive Study Day (DSSTDY) ------------------------------------------------

ds <- sdtm.oak::derive_study_day(
  sdtm_in = ds,
  dm_domain = dm,
  tgdt = "DSSTDTC",
  refdt = ref_day_var,
  study_day_var = "DSSTDY",
  merge_key = "USUBJID"
)

# ---- Derive Sequence Number (DSSEQ) ------------------------------------------

# Record-level identifiers for sequencing. Adjust ordering variables if needed
# for your sponsor conventions (e.g., include VISITNUM, VISIT).
rec_vars <- c("STUDYID", "USUBJID", "DSSTDTC", "DSTERM", "DSDECOD")

ds <- sdtm.oak::derive_seq(
  tgt_dat = ds,
  tgt_var = "DSSEQ",
  rec_vars = rec_vars
)

# ---- Final variable order & output -------------------------------------------

ds <- ds %>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT,
    VISITNUM, VISIT,
    DSDTC, DSSTDTC, DSSTDY
  ) %>%
  arrange(STUDYID, USUBJID, DSSEQ)


# Apply labels 
ds <- ds %>%
  set_variable_labels(.labels = ds_labels)
#create output 
write_xpt(ds, "ds.xpt")

