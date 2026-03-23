# ------------------------------------------------------------------------------
# Program:     01_create_ae_summary_table.R
# Purpose:     Create FDA Table 10 summary of Treatment-Emergent Adverse Events
#              (TEAEs) by System Organ Class (AESOC) and Preferred Term (AETERM)
#              using ADAE and ADSL datasets.
#
# Author:      Emmanuel Endeshaw
# Date created: Mar22, 2026
#
# Input:       ADSL  - Subject-level analysis dataset
#              ADAE  - Adverse events analysis dataset
#              Source:
#                 1) adsl.xpt / adae.xpt / csv
#                 2) pharmaverseadam::adsl
#                 3) pharmaverseadam::adae
#
# Output:      3_1_table_10_teae_.pdf
#              Hierarchical TEAE table with:
#                 - Overall TEAEs
#                 - System Organ Class (AESOC)
#                 - Preferred Term (AETERM)
#              Columns:
#                 Treatment arms (ARM)
#                 Count (n) and Percentage (%)
#
# Population:  Safety population (SAFFL = "Y")
#
# Derivations:
#              - Treatment Emergent Flag (TRTEMFL)
#              - AE start/end dates (ASTDT, AENDT)
#              - SOC ordering by descending frequency
#              - PT ordering within SOC
#              - Count and percent formatting: n (%)
#
# Display:     FDA Table 10 layout
#              Hierarchical SOC to PT indentation
#              Column headers include N per treatment arm
#
# References:
#  - FDA Table 10 (TEAE Summary)
#  - pharmaverseadam::adae
#  - admiral::derive_var_trtemfl
#  - admiral::derive_vars_dt
#  - CDISC ADaM ADAE specification
#  - pharmaverse cardinal FDA table examples
# ------------------------------------------------------------------------------
library(haven)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(admiral)
library(gtsummary)
library(gt)
library(webshot2)


#--------------------------------------------------
# load ADAE / ADSL
#--------------------------------------------------
find_first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

load_adam <- function(
    adae_candidates = c("adae.xpt", "ADAE.xpt", "adae.csv", "ADAE.csv"),
    adsl_candidates = c("adsl.xpt", "ADSL.xpt", "adsl.csv", "ADSL.csv")
) {
  adae_path <- find_first_existing(adae_candidates)
  adsl_path <- find_first_existing(adsl_candidates)
  
  if (!is.na(adae_path) && grepl("\\.xpt$", adae_path, ignore.case = TRUE)) {
    adae <- read_xpt(adae_path)
  } else if (!is.na(adae_path) && grepl("\\.csv$", adae_path, ignore.case = TRUE)) {
    adae <- read_csv(adae_path, show_col_types = FALSE)
  } else {
    adae <- pharmaverseadam::adae
  }
  
  if (!is.na(adsl_path) && grepl("\\.xpt$", adsl_path, ignore.case = TRUE)) {
    adsl <- read_xpt(adsl_path)
  } else if (!is.na(adsl_path) && grepl("\\.csv$", adsl_path, ignore.case = TRUE)) {
    adsl <- read_csv(adsl_path, show_col_types = FALSE)
  } else {
    adsl <- pharmaverseadam::adsl
  }
  
  list(adsl = adsl, adae = adae)
}

# Load ADaM datasets
dat <- load_adam()

# Extract ADSL (subject-level)
adsl <- dat$adsl

# Extract ADAE (adverse events)
adae <- dat$adae

#--------------------------------------------------
#   Safety population denominator
#    Use ARM directly if present; otherwise fallback
#--------------------------------------------------
adsl_saf <- adsl %>%
  filter(SAFFL == "Y") %>%
  select(USUBJID, ARM) %>%
  distinct()

denom <- adsl_saf %>%
  count(ARM, name = "N")

#--------------------------------------------------
# Derive AE dates only if needed for TRTEMFL derivation
#--------------------------------------------------
adae_dated <- adae %>%
  select(-any_of(c("ASTDT", "ASTDTF", "AENDT", "AENDTF"))) %>%
  derive_vars_dt(
    new_vars_prefix = "AST",
    dtc = AESTDTC,
    highest_imputation = "M",
    date_imputation = "first"
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "AEN",
    dtc = AEENDTC,
    highest_imputation = "M",
    date_imputation = "last"
  )

#--------------------------------------------------
#    Merge ADAE with safety population
#    Keep only safety subjects for AE summaries
#--------------------------------------------------
adae_saf <- adae_dated %>%
  inner_join(adsl_saf, by = "USUBJID") %>%
  mutate(ARM = coalesce(ARM.y, ARM.x)) %>%
  select(-ARM.x, -ARM.y)

#--------------------------------------------------
#    Derive TRTEMFL only if missing
#    Only do this if treatment dates are available
#--------------------------------------------------
if (!"TRTEMFL" %in% names(adae_saf)) {
  trt_dates <- adsl %>%
    filter(SAFFL == "Y") %>%
    transmute(
      USUBJID,
      TRTSDT = coalesce(
        suppressWarnings(as.Date(.data$TRTSDT)),
        suppressWarnings(as.Date(.data$TRTSDTM)),
        suppressWarnings(as.Date(.data$RFXSTDTC))
      ),
      TRTEDT = coalesce(
        suppressWarnings(as.Date(.data$TRTEDT)),
        suppressWarnings(as.Date(.data$TRTEDTM)),
        suppressWarnings(as.Date(.data$RFXENDTC))
      )
    ) %>%
    distinct()
  
  adae_saf <- adae_saf %>%
    left_join(trt_dates, by = "USUBJID") %>%
    derive_var_trtemfl(
      new_var = TRTEMFL,
      start_date = ASTDT,
      end_date = AENDT,
      trt_start_date = TRTSDT,
      trt_end_date = TRTEDT,
      end_window = 30
    )
}

#--------------------------------------------------
#  Keep TEAEs only
#--------------------------------------------------
adae_teae <- adae_saf %>%
  filter(TRTEMFL == "Y")

#--------------------------------------------------
#   Table 10: TEAE by SOC and PT
#    This is the second table you want
#--------------------------------------------------

# Overall TEAE row
overall <- adae_teae %>%
  distinct(USUBJID, ARM) %>%
  count(ARM, name = "n") %>%
  mutate(
    AESOC = "Treatment Emergent AEs",
    AETERM = NA_character_,
    row_type = "overall"
  )

# SOC rows
soc <- adae_teae %>%
  distinct(USUBJID, ARM, AESOC) %>%
  count(ARM, AESOC, name = "n") %>%
  mutate(
    AETERM = NA_character_,
    row_type = "soc"
  )

# PT rows
pt <- adae_teae %>%
  distinct(USUBJID, ARM, AESOC, AETERM) %>%
  count(ARM, AESOC, AETERM, name = "n") %>%
  mutate(row_type = "pt")

# Combine
ae_table <- bind_rows(overall, soc, pt) %>%
  left_join(denom, by = "ARM") %>%
  mutate(
    pct = 100 * n / N,
    value = if_else(
      pct < 10,
      sprintf("%d (%.1f%%)", n, pct),
      sprintf("%d (%.0f%%)", n, pct)
    )
  )

# SOC ordering by total frequency across arms
soc_order <- soc %>%
  group_by(AESOC) %>%
  summarise(total_soc = sum(n), .groups = "drop") %>%
  arrange(desc(total_soc)) %>%
  mutate(soc_ord = row_number())

# PT ordering within SOC by total frequency across arms
pt_order <- pt %>%
  group_by(AESOC, AETERM) %>%
  summarise(total_pt = sum(n), .groups = "drop") %>%
  arrange(AESOC, desc(total_pt), AETERM)

# Build display rows
overall_disp <- ae_table %>%
  filter(row_type == "overall") %>%
  mutate(
    display_term = AESOC,
    soc_ord = 0,
    pt_ord = 0
  )

soc_disp <- ae_table %>%
  filter(row_type == "soc") %>%
  left_join(soc_order, by = "AESOC") %>%
  mutate(
    display_term = AESOC,
    pt_ord = 0
  )

pt_disp <- ae_table %>%
  filter(row_type == "pt") %>%
  left_join(soc_order, by = "AESOC") %>%
  left_join(
    pt_order %>%
      group_by(AESOC) %>%
      mutate(pt_ord = row_number()) %>%
      select(AESOC, AETERM, pt_ord),
    by = c("AESOC", "AETERM")
  ) %>%
  mutate(display_term = paste0("   ", AETERM))

#--------------------------------------------------
# Build final hierarchical table
#--------------------------------------------------
final_long <- bind_rows(overall_disp, soc_disp, pt_disp) %>%
  select(soc_ord, pt_ord, display_term, ARM, value)

final_table <- final_long %>%
  pivot_wider(
    names_from = ARM,
    values_from = value
  ) %>%
  arrange(soc_ord, pt_ord)

final_table <- final_long %>%
  pivot_wider(
    names_from = ARM,
    values_from = value
  ) %>%
  arrange(soc_ord, pt_ord, display_term) %>%
  select(-soc_ord, -pt_ord)


#--------------------------------------------------
#  gt display
#--------------------------------------------------
arm_headers <- denom %>%
  mutate(header = paste0(ARM, "<br>N = ", N)) %>%
  select(ARM, header)

gt_tbl <- final_table %>%
  gt(rowname_col = "display_term") %>%
  tab_stubhead(
    label = "Primary System Organ Class\nReported Term for the Adverse Event"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stubhead()
  )

for (i in seq_len(nrow(arm_headers))) {
  gt_tbl <- gt_tbl %>%
    cols_label(!!arm_headers$ARM[i] := html(arm_headers$header[i]))
}

gtsave(
  gt_tbl,
  "3_1_table_10_teae_.pdf"
)