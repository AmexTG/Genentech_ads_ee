# ------------------------------------------------------------------------------
# Program:     create_adsl.R
# Purpose:     Create ADaM Subject-Level Analysis Dataset (ADSL)
#              from SDTM domains using {admiral} and tidyverse tools.
#
# Author:      Emmanuel Endeshaw
# Date:        Mar 21, 2026
#
# Input:       
#   pharmaversesdtm::dm   - Demographics (base dataset)
#   pharmaversesdtm::ex   - Exposure
#   pharmaversesdtm::ds   - Disposition
#   pharmaversesdtm::vs   - Vital Signs
#   pharmaversesdtm::ae   - Adverse Events
#
# Output:
#   adsl with key variables:
#   STUDYID, USUBJID, TRT01P, TRT01PN, TRTSDT, TRTEDT,
#   SAFFL, ITTFL, AGE, SEX, RACE, ARM, ARMCD
#
# Derivations:
#   - Base dataset: DM
#   - Treatment variables from EX
#   - Population flags (SAFFL, ITTFL)
#   - Treatment start/end dates
#   - Demographic variables retained from DM
#
# References:
#   - ADaM IG v1.3
#   - Pharmaverse Admiral ADSL vignette
#   - https://pharmaverse.github.io/admiral/articles/adsl.html
# ------------------------------------------------------------------------------

# Libraries
library(dplyr)
library(stringr)
library(haven)
library(gt)
library(admiral)
library(pharmaversesdtm)

# --------------------------------------------
# SDTM data input obtained from vignette("adsl", package = "admiral")
# --------------------------------------------
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# --------------------------------------------
# Helper: valid dose
# A valid dose is:
#   EXDOSE > 0
#   OR EXDOSE = 0 and EXTRT contains "PLACEBO"
# --------------------------------------------
valid_dose <- function(exdose, extrt) {
  (exdose > 0) | (exdose == 0 & str_detect(extrt, "PLACEBO"))
}

# --------------------------------------------
# Base ADSL from DM
# --------------------------------------------
adsl <- dm %>%
  select(-any_of("DOMAIN"))

# --------------------------------------------
# Specification to derivation based on the coding assessment for:
# ITTFL 
# AGEGR9 / AGEGR9N
# TRTSDTM & TRTSTMF
# LSTAVLDT
# --------------------------------------------
# ITTFL
adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  )

# AGEGR9 / AGEGR9N
adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18-50",
      AGE > 50 ~ ">50"
    ),
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50 ~ 3
    )
  )


# --------------------------------------------
# Exposure datetime derivation
# EXSTDTM / EXSTTMF from EXSTDTC
# EXENDTM / EXENTMF from EXENDTC
# --------------------------------------------
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "first",
    highest_imputation = "h",
    ignore_seconds_flag = TRUE
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

# --------------------------------------------
# TRTSDTM / TRTSTMF
# First valid dose start datetime
# --------------------------------------------
ex_ext <- ex %>%
  select(-matches("^TRT")) %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "first",
    highest_imputation = "h",
    ignore_seconds_flag = TRUE
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )
names(ex_ext)
# --------------------------------------------
# TRTEDTM / TRTETMF
# Last valid dose end datetime
# --------------------------------------------
adsl <- derive_vars_merged(
  adsl,
  dataset_add = ex_ext,
  by_vars = exprs(STUDYID, USUBJID),
  filter_add =
    valid_dose(EXDOSE, EXTRT) &
    !is.na(EXENDTM),
  order = exprs(EXENDTM, EXSEQ),
  new_vars = exprs(
    TRTEDTM = EXENDTM,
    TRTETMF = EXENTMF
  ),
  mode = "last"
)

# --------------------------------------------
# SAFFL
# At least one valid dose
# --------------------------------------------
adsl <- derive_var_merged_exist_flag(
  adsl,
  dataset_add = ex,
  by_vars = exprs(STUDYID, USUBJID),
  new_var = SAFFL,
  condition = valid_dose(EXDOSE, EXTRT),
  false_value = "N",
  missing_value = "N"
)

# --------------------------------------------
# ANYVSFL
# At least one VS with non-missing result
# --------------------------------------------
#remove duplicate or overwrite
adsl <- adsl %>%
  select(-any_of("ANYVSFL"))

adsl <- derive_var_merged_exist_flag(
  adsl,
  dataset_add = vs,
  by_vars = exprs(STUDYID, USUBJID),
  new_var = ANYVSFL,
  condition = !(is.na(VSSTRESN) & is.na(VSSTRESC)),
  false_value = "N",
  missing_value = "N"
)

# --------------------------------------------
# Last complete VS date with valid result
# --------------------------------------------
vs_last <- vs %>%
  filter(
    !(is.na(VSSTRESN) & is.na(VSSTRESC)),
    !is.na(convert_dtc_to_dt(VSDTC))
  ) %>%
  mutate(VS_LASTDT = convert_dtc_to_dt(VSDTC)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(
    VS_LASTDT = max(VS_LASTDT),
    .groups = "drop"
  )


# --------------------------------------------
# Last complete AE onset date
# --------------------------------------------
ae_last <- ae %>%
  filter(!is.na(convert_dtc_to_dt(AESTDTC))) %>%
  mutate(AE_LASTDT = convert_dtc_to_dt(AESTDTC)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(
    AE_LASTDT = max(AE_LASTDT),
    .groups = "drop"
  )

# --------------------------------------------
# Last complete disposition date
# --------------------------------------------
ds_last <- ds %>%
  filter(!is.na(convert_dtc_to_dt(DSSTDTC))) %>%
  mutate(DS_LASTDT = convert_dtc_to_dt(DSSTDTC)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(
    DS_LASTDT = max(DS_LASTDT),
    .groups = "drop"
  )

# --------------------------------------------
# Merge last dates into ADSL
# --------------------------------------------
adsl <- adsl %>%
  left_join(vs_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(ae_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(ds_last, by = c("STUDYID", "USUBJID"))

# --------------------------------------------
# LSTAVLDT
# Max of:
#   1) last VS date
#   2) last AE onset date
#   3) last DS date
#   4) datepart of TRTEDTM
# --------------------------------------------
adsl <- adsl %>%
  mutate(
    LSTAVLDT = pmax(
      VS_LASTDT,
      AE_LASTDT,
      DS_LASTDT,
      as.Date(TRTEDTM),
      na.rm = TRUE
    )
  )

# ---- Final variable removal & output ---------
adsl <- adsl %>%
  select(-VS_LASTDT, -AE_LASTDT, -DS_LASTDT)


#create output 
write_xpt(adsl, "adsl.xpt")


#html output
adsl %>%
  head(50) %>%
  gt() %>%
  gtsave("adsl_listing.html")


