# READ ME -----------------------------------------------------------------
#
# Author: Shodai Inose
# Created: 2024-03-14
#
# bipolar post-exposure confounder of the mediator-outcome relationship
#
# Creates an indicator variable for whether or not an observation in
#   the analysis cohort had an bipolar ICD code in post exposure
#   period, occurring in months 7-12 post-Medicaid enrollment 
#
# Note: Modifies code originally written by Kat in:
#   https://github.com/CI-NYC/disability/projects/create_cohort/scripts/04_define_comorbidity_vars/define_bipolar.R
#
# -------------------------------------------------------------------------

library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in bipolar occurrence dates
bipolar <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/bipolar.rds"))
setDT(bipolar)
setkey(bipolar, BENE_ID)

# Read in analysis cohort
cohort <- readRDS(file.path("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)

cohort <- cohort[, .(BENE_ID, washout_cal_end_dt)]

# Create indicator for bipolar post-exposure confounder
# 1 if patient received diagnoses and prescriptions related to bipolar treatment occurring 
# in months 7-12 post-Medicaid enrollment (mediator window), 0 otherwise 
bipolar <- merge(bipolar[, .(BENE_ID, bipolar_dt)], cohort, all.y = TRUE)

bipolar[, bipolar_post_exposure_cal := fcase(
    bipolar_dt %within% interval(washout_cal_end_dt, washout_cal_end_dt + days(182)), 1, 
    default = 0
)]

saveRDS(bipolar[, .(BENE_ID, bipolar_post_exposure_cal)], 
        file.path(drv_root, "post_exposure_bipolar.rds"))
