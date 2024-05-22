# -------------------------------------
# Script: 
# Author: Nick Williams
# Updated:
# Purpose: Reads in the 24-month censoring indicator variables 
#  and applies cohort exclusion criteria to the joined mediator 
#  data set to create the final mediation analysis cohort.
# Notes:
# -------------------------------------

library(data.table)
library(lubridate)
library(purrr)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds")  # Cohort
mediators <- readRDS(file.path(drv_root, "mediator_12mo_df.rds"))                                     # Mediator vars
censoring_df <- readRDS(file.path(drv_root, "censoring_12mo_washout_24mo.rds"))                       # Censoring vars
oud_df <- readRDS(file.path(drv_root, "washout_12mo_oud_18mo_to_24mo.rds"))                           # OUD outcomes
overdose_df <- readRDS(file.path(drv_root, "washout_12mo_oud_overdose_18mo_to_24mo.rds"))             # Overdose
depression_df <- readRDS(file.path(drv_root, "12mo_post_exposure_depression.rds"))                    # post-exposure confounders
anxiety_df <- readRDS(file.path(drv_root, "12mo_post_exposure_anxiety.rds"))
bipolar_df <- readRDS(file.path(drv_root, "12mo_post_exposure_bipolar.rds"))
counseling <- readRDS(file.path(drv_root, "baseline_12mo_has_counseling.rds"))

# Merge variables to the cohort ------------------------------------------------

setDT(cohort)
setkey(cohort, BENE_ID)

mediation_analysis_df <- 
    reduce(list(cohort, 
                mediators, 
                censoring_df, 
                oud_df, 
                overdose_df, 
                depression_df, 
                anxiety_df,
                bipolar_df, 
                counseling), 
           merge, all.x = TRUE, all.y = TRUE)

rm(cohort, 
   mediators, 
   censoring_df, 
   oud_df, 
   overdose_df, 
   depression_df, 
   anxiety_df,
   bipolar_df, 
   counseling)

# Apply inclusion/exclusion logic ----------------------------------------------

# Filter >= 18 months in the study to deal with missing mediators
# Minimum study duration of 18 months in days

# Calculate study duration
mediation_analysis_df[, study_duration := 
                          fifelse(is.na(censoring_ever_dt), 
                                        547, time_length(censoring_ever_dt - washout_start_dt, "days"))]

# Remove rows with study duration < min_study_duration
mediation_analysis_df <- mediation_analysis_df[study_duration >= 546, ]

mediation_analysis_df[, study_duration := NULL]

# Change outcome for uncensored observations to NA
mediation_analysis_df[, oud_24mo := fifelse(uncens_24mo == 0, NA_real_, oud_24mo)]
mediation_analysis_df[, oud_24mo_icd := fifelse(uncens_24mo == 0, NA_real_, oud_24mo_icd)]
mediation_analysis_df[, oud_overdose_24mo := fifelse(uncens_24mo == 0, NA_real_, oud_overdose_24mo)]

# Filter age >= 35 years to deal with extreme positivity
mediation_analysis_df <- mediation_analysis_df[dem_age >= 35, ]

# Save
saveRDS(mediation_analysis_df, file.path(drv_root, "mediation_12mo_analysis_df.rds"))
