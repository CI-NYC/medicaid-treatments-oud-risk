# -------------------------------------
# Script: 01_create_final_analysis_cohort.R
# Author: Sarah Forrest
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

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds") # Cohort
mediators <- readRDS(file.path(drv_root, "mediator_df.rds"))                              # Mediator vars
censoring_df <- readRDS(file.path(drv_root, "censoring_24mo.rds"))                   # Censoring vars
oud_df <- readRDS(file.path(drv_root, "oud_12mo_to_24mo.rds"))                          # OUD outcomes
depression_df <- readRDS(file.path(drv_root, "post_exposure_depression.rds"))             # post-exposure confounders
anxiety_df <- readRDS(file.path(drv_root, "post_exposure_anxiety.rds"))
bipolar_df <- readRDS(file.path(drv_root, "post_exposure_bipolar.rds"))
counseling <- readRDS(file.path(drv_root, "baseline_has_counseling.rds"))

# Merge variables to the cohort ------------------------------------------------

setDT(cohort)
setkey(cohort, BENE_ID)

mediation_analysis_df <- 
    reduce(list(cohort, 
                mediators, 
                censoring_df, 
                oud_df, 
                depression_df, 
                anxiety_df,
                bipolar_df, 
                counseling), 
           merge, all.x = TRUE, all.y = TRUE)

rm(cohort, 
   mediators, 
   censoring_df, 
   oud_df, 
   depression_df, 
   anxiety_df,
   bipolar_df, 
   counseling)

# Apply inclusion/exclusion logic ----------------------------------------------

# Filter >= 12 months in the study to deal with missing mediators
# Minimum study duration of 12 months in days

# Calculate study duration
mediation_analysis_df[, study_duration := 
                          fifelse(is.na(censoring_ever_dt), 
                                        366, time_length(censoring_ever_dt - washout_start_dt, "days"))]

# Remove rows with study duration < min_study_duration
mediation_analysis_df <- mediation_analysis_df[study_duration >= 365, ]

mediation_analysis_df[, study_duration := NULL]

# Change outcome for uncensored observations to NA
mediation_analysis_df[, oud_24mo := fifelse(uncens_24mo == 0, NA_real_, oud_24mo)]
mediation_analysis_df[, oud_24mo_icd := fifelse(uncens_24mo == 0, NA_real_, oud_24mo_icd)]

# mediation_analysis_df[, age_cat := fifelse(dem_age < 35, "<35", ">34")]
# table(mediation_analysis_df$age_cat, mediation_analysis_df$disability_pain_cal) |> 
#     prop.table(margin = 1)*100
# 
# table(mediation_analysis_df$age_cat, mediation_analysis_df$disability_pain_cal) |> 
#     prop.table(margin = 2)*100

# Filter age >= 35 years to deal with extreme positivity
mediation_analysis_df <- mediation_analysis_df[dem_age >= 35, ]

# Save
saveRDS(mediation_analysis_df, file.path(drv_root, "mediation_analysis_df.rds"))
