# -------------------------------------
# Script: 02_mediator_max_daily_dose_mme.R
# Author: Sarah Forrest
# Updated:
# Purpose: Outputs a mediator dataset with the the maximum daily dose (MME) of 
#  opioids prescribed to a beneficiary within the mediator period, 
#  accounting for multiple prescriptions on the same day, for all 
#  beneficiaries in the analysis cohort
# Notes:
# -------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)
library(tictoc)
library(foreach)
library(doFuture)
library(furrr)

data_dir <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
proj_dir <- "projects/mediation_unsafe_pain_mgmt"

# Read in cohort and dates
dts_cohorts <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")
setDT(dts_cohorts)
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_cal_end_dt)]
setkey(dts_cohorts, BENE_ID)

all_opioids_clean <- readRDS(file.path(data_dir, "all_pain_opioids.rds"))
setDT(all_opioids_clean)
setkey(all_opioids_clean, BENE_ID)
all_opioids_clean <- all_opioids_clean[, .(BENE_ID, NDC, opioid, mme_strength_per_day, rx_start_dt, rx_end_dt)]

# Merge to the analysis cohort
all_opioids_clean_merged <- merge(all_opioids_clean, dts_cohorts, by = "BENE_ID")

# Filter opioid prescriptions to only contain those within mediator period
all_opioids_clean_mediator_period <- all_opioids_clean_merged[
    all_opioids_clean_merged$rx_start_dt %within% interval(
        all_opioids_clean_merged$washout_cal_end_dt, all_opioids_clean_merged$washout_cal_end_dt + days(182)
    ), 
]

# Calculate max daily dose -----------------------------------------------------

# Group by beneficiary and create a list column containing each beneficiairy's data
opioids <- all_opioids_clean_mediator_period[, list(data = list(data.table(.SD))), by = BENE_ID]

# Create function
calculate_max_daily_dose <- function(data) {
    to_modify <- copy(data)
    
    # to_modify[, c("rx_start_dt", "rx_end_dt") := lapply(.SD, as.Date), 
    #           .SDcols = c("rx_start_dt", "rx_end_dt")]
    
    # Calculate the date limit based on washout_cal_end_dt + 182 days
    washout_date_limit <- to_modify$washout_cal_end_dt + lubridate::days(182)
    
    long <- to_modify[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                          NDC, opioid, mme_strength_per_day), by = .(seq_len(nrow(data)))
                      ][date <= washout_date_limit, ]  # Filter rows based on date limit
    
    long[, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE)), by = .(date)
         ][, .(mediator_max_daily_dose_mme = max(total_mme_strength))]
}

plan(multisession, workers = 50)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                   out <- calculate_max_daily_dose(data)
                   out$BENE_ID <- id
                   setcolorder(out, "BENE_ID")
                   out
               }

plan(sequential)

# Right join with cohort
max_daily_dose <- merge(out, dts_cohorts[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
max_daily_dose[, mediator_max_daily_dose_mme := fifelse(is.na(mediator_max_daily_dose_mme), 0, mediator_max_daily_dose_mme)]

# Save final dataset -----------------------------------------------------------

saveRDS(max_daily_dose, file.path(data_dir, "mediator_max_daily_dose_mme.rds"))
