# -------------------------------------
# Script: 
# Author: Sarah Forrest
# Updated:
# Purpose: Creates an indicator variable for whether or not an observation in
#   the analysis cohort had a depression ICD code in post exposure
#   period, occurring in months 13-18 post-Medicaid enrollment 
# Notes:
# -------------------------------------

library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in depression occurrence dates
depression <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/depression.rds"))
setDT(depression)
setkey(depression, BENE_ID)

# Read in analysis cohort
cohort <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)

cohort <- cohort[, .(BENE_ID, washout_12mos_end_dt)]

depression <- merge(depression[, .(BENE_ID, depression_dt)], cohort, all.y = TRUE)

depression[, depression_post_exposure_cal := fcase(
    depression_dt %within% interval(washout_12mos_end_dt, washout_12mos_end_dt + days(182)), 1, 
    default = 0
)]

saveRDS(depression[, .(BENE_ID, depression_post_exposure_cal)], 
        file.path(drv_root, "12mo_post_exposure_depression.rds"))
