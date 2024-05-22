# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-10-27
# Last updated: 2023-11-16 (Nick)
#
# Depression post-exposure confounder of the mediator-outcome relationship
#
# Creates an indicator variable for whether or not an observation in
#   the analysis cohort had a depression ICD code in post exposure
#   period, occurring in months 7-12 post-Medicaid enrollment 
#  
# Note: Modifies code originally written by Kat in:
#   https://github.com/CI-NYC/disability/projects/create_cohort/scripts/04_define_comorbidity_vars/define_depression.R
#
# -------------------------------------------------------------------------

library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in depression occurrence dates
depression <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/depression.rds"))
setDT(depression)
setkey(depression, BENE_ID)

# Read in analysis cohort
cohort <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)

cohort <- cohort[, .(BENE_ID, washout_cal_end_dt)]

# Create indicator for depression post-exposure confounder
# 1 if patient received diagnoses and prescriptions related to depression treatment occurring in 
# months 7-12 post-Medicaid enrollment (mediator window), 0 otherwise 
depression <- merge(depression[, .(BENE_ID, depression_dt)], cohort, all.y = TRUE)

depression[, depression_post_exposure_cal := fcase(
    depression_dt %within% interval(washout_cal_end_dt, washout_cal_end_dt + days(182)), 1, 
    default = 0
)]

saveRDS(depression[, .(BENE_ID, depression_post_exposure_cal)], 
        file.path(drv_root, "post_exposure_depression.rds"))
