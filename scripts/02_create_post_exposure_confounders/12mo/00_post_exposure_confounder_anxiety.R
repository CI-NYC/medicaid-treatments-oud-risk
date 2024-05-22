# -------------------------------------
# Script: 
# Author: Sarah Forrest
# Updated:
# Purpose: Creates an indicator variable for whether or not an observation in
#   the analysis cohort had an anxiety ICD code in post exposure
#   period, occurring in months 13-18 post-Medicaid enrollment 
# Notes:
# -------------------------------------

library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in anxiety occurrence dates
anxiety <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/anxiety.rds"))
setDT(anxiety)
setkey(anxiety, BENE_ID)

# Read in analysis cohort
cohort <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)

cohort <- cohort[, .(BENE_ID, washout_12mos_end_dt)]

anxiety <- merge(anxiety[, .(BENE_ID, anxiety_dt)], cohort, all.y = TRUE)

anxiety[, anxiety_post_exposure_cal := fcase(
    anxiety_dt %within% interval(washout_12mos_end_dt, washout_12mos_end_dt + days(182)), 1, 
    default = 0
)]

saveRDS(anxiety[, .(BENE_ID, anxiety_post_exposure_cal)], 
        file.path(drv_root, "12mo_post_exposure_anxiety.rds"))
