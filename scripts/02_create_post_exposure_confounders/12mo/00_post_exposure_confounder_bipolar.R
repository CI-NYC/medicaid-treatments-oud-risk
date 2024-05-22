# -------------------------------------
# Script: 
# Author: Shodai Inose
# Updated:
# Purpose: Creates an indicator variable for whether or not an observation in
#   the analysis cohort had an bipolar ICD code in post exposure
#   period, occurring in months 13-18 post-Medicaid enrollment 
# Notes:
# -------------------------------------

library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in bipolar occurrence dates
bipolar <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/bipolar.rds"))
setDT(bipolar)
setkey(bipolar, BENE_ID)

# Read in analysis cohort
cohort <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)

cohort <- cohort[, .(BENE_ID, washout_12mos_end_dt)]

bipolar <- merge(bipolar[, .(BENE_ID, bipolar_dt)], cohort, all.y = TRUE)

bipolar[, bipolar_post_exposure_cal := fcase(
    bipolar_dt %within% interval(washout_12mos_end_dt, washout_12mos_end_dt + days(182)), 1, 
    default = 0
)]

saveRDS(bipolar[, .(BENE_ID, bipolar_post_exposure_cal)], 
        file.path(drv_root, "12mo_post_exposure_bipolar.rds"))
