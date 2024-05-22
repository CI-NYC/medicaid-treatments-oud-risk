# -------------------------------------
# Script: 03_censoring.R
# Author: Sarah Forrest
# Updated:
# Purpose: Creates an indicator variable for whether or not a beneficiary in the 
#   analysis cohort was uncensored at 18 months and 24 months after their
#   washout start date. 
# Notes:
# -------------------------------------

library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in cohort and dates
cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds")
setDT(cohort)
setkey(cohort, BENE_ID)

cohort <- cohort[, .(BENE_ID, washout_start_dt, censoring_ever_dt)]

# Create censoring variables ---------------------------------------------------

cohort[, uncens_24mo := fcase(is.na(censoring_ever_dt), 1, 
                              censoring_ever_dt >= (washout_start_dt + days(728)), 1, 
                              default = 0)]

censoring_df <- cohort[, .(BENE_ID, uncens_24mo)]

# Save -------------------------------------------------------------------------

saveRDS(censoring_df, file.path(drv_root, "censoring_12mo_washout_24mo.rds"))
