# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-10-27
# Last updated: 2023-11-16 (Nick)
#
# Anxiety post-exposure confounder of the mediator-outcome relationship
#
# Creates an indicator variable for whether or not an observation in
#   the analysis cohort had an anxiety ICD code in post exposure
#   period, occurring in months 7-12 post-Medicaid enrollment 
#
# Note: Modifies code originally written by Kat in:
#   https://github.com/CI-NYC/disability/projects/create_cohort/scripts/04_define_comorbidity_vars/define_anxiety.R
#
# -------------------------------------------------------------------------

library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in anxiety occurrence dates
anxiety <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/anxiety.rds"))
setDT(anxiety)
setkey(anxiety, BENE_ID)

# Read in analysis cohort
cohort <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)

cohort <- cohort[, .(BENE_ID, washout_cal_end_dt)]

# Create indicator for anxiety post-exposure confounder
# 1 if patient received diagnoses and prescriptions related to anxiety treatment occurring 
# in months 7-12 post-Medicaid enrollment (mediator window), 0 otherwise 
anxiety <- merge(anxiety[, .(BENE_ID, anxiety_dt)], cohort, all.y = TRUE)

anxiety[, anxiety_post_exposure_cal := fcase(
    anxiety_dt %within% interval(washout_cal_end_dt, washout_cal_end_dt + days(182)), 1, 
    default = 0
)]

saveRDS(anxiety[, .(BENE_ID, anxiety_post_exposure_cal)], 
        file.path(drv_root, "post_exposure_anxiety.rds"))
