# -------------------------------------
# Script: 
# Author: Nick Williams
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(purrr)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds")

setDT(cohort)

overdose <- copy(cohort)
overdose <- overdose[, .(BENE_ID, washout_12mos_end_dt, oud_poison_dt)]

overdose[, oud_overdose_24mo := fcase(
  oud_poison_dt %within% interval(
    washout_12mos_end_dt + days(182), 
    washout_12mos_end_dt + days(365)
  ), 1, 
  default = 0
)]

saveRDS(overdose, file.path(drv_root, "washout_12mo_oud_overdose_18mo_to_24mo.rds"))
