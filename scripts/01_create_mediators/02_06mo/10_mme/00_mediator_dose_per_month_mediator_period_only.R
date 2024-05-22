# -------------------------------------
# Script: 00_mediator_dose_per_month_mediator_period_only.R
# Author: Sarah Forrest
# Updated:
# Purpose: 
#   Outputs: 
#       1. A long format dataset which contains monthly summaries for 
#           beneficiaries with an opioid prescription during the 6 month 
#           mediator period
#       2. A wide format dataset with the average daily dose of opioids 
#           in MME for each month of the mediator period (6 total) for
#           all beneficiaries in the analysis cohort 
#       3. A dataset with the overall average daily dose of opioids 
#           in MME over the entire mediator period, considering all months
#           that a beneficiary was prescribed an opioid, for all 
#           beneficiaries in the analysis cohort
# Notes:
# -------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)
library(tictoc)
library(doFuture)
library(foreach)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in cohort/dates
dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_cal_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Read in opioids
opioids <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/all_pain_opioids.rds")
setDT(opioids)
setkey(opioids, BENE_ID)

opioids <- merge(opioids, dts_cohorts, by = "BENE_ID")

# Filter opioid prescriptions to only contain those within mediator period
opioids <- opioids[rx_start_dt %within% interval(washout_cal_end_dt, 
                                                 washout_cal_end_dt + days(182)), ]

opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]

# Note: modified code from 12_mediator_dose_per_month.R here
summarize_opioids <- function(data) {
    setDT(data)
    
    data[, (c("rx_start_dt", "rx_end_dt")) := lapply(.SD, as.Date), .SDcols = c("rx_start_dt", "rx_end_dt")]
    
    long <- data[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                     mme_strength_per_day, opioid, washout_cal_end_dt), 
                 by = seq_len(nrow(data))
                 ][, seq_len := NULL][]
    
    long <- long[, days_from_mediator_start := difftime(date, washout_cal_end_dt, units = "day")
                 ][, weeks_from_mediator_start := ceiling(time_length(days_from_mediator_start, "weeks"))
                   ][, months_from_mediator_start := ceiling(time_length(days_from_mediator_start, "months"))
                     ][, .(total_opioid_dose = sum(mme_strength_per_day), 
                           avg_30d_opioid_dose = sum(mme_strength_per_day) / 30, 
                           num_unique_opioids = n_distinct(opioid)), by = "months_from_mediator_start"
                       ][, any_opioid := fifelse(total_opioid_dose > 0, 1, 0)]
    
    setorder(long, months_from_mediator_start)
    long[]
}

plan(multisession, workers = 50)

# Monthly summaries for all beneficiaries in the analysis cohort with an 
# opioid prescription during the mediator period (long format)
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                   out <- summarize_opioids(data)
                   out$BENE_ID <- id
                   setcolorder(out, "BENE_ID")
                   out
               }

plan(sequential)

# Filter data for 5 months + 29 days after the mediator period begins
out <- out[months_from_mediator_start <= 5]

saveRDS(out, file.path(drv_root, "mme_month_summary_mediator_period_only.rds"))

# Average daily dose in MME for all beneficiaries in the analysis cohort for each 30-day 
# interval (month) of the mediator period (wide format)
# Convert long format to wide format with average 30-day dose for each month
avg_30d_dose <- pivot_wider(out, 
                            id_cols = "BENE_ID",
                            names_from = "months_from_mediator_start",
                            values_from = "avg_30d_opioid_dose",
                            names_prefix = "mediator_avg_daily_dose_mme_month",
                            values_fill = 0)

# Create columns for each month from the mediator start
avg_30d_dose <- select(
    avg_30d_dose,
    BENE_ID, 
    mediator_avg_daily_dose_mme_month1 = mediator_avg_daily_dose_mme_month0,
    mediator_avg_daily_dose_mme_month2 = mediator_avg_daily_dose_mme_month1,
    mediator_avg_daily_dose_mme_month3 = mediator_avg_daily_dose_mme_month2,
    mediator_avg_daily_dose_mme_month4 = mediator_avg_daily_dose_mme_month3,
    mediator_avg_daily_dose_mme_month5 = mediator_avg_daily_dose_mme_month4,
    mediator_avg_daily_dose_mme_month6 = mediator_avg_daily_dose_mme_month5
)

setDT(avg_30d_dose)
setkey(avg_30d_dose, BENE_ID)

# Right join with cohort
avg_30d_dose <- merge(avg_30d_dose, dts_cohorts[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
avg_30d_dose[is.na(avg_30d_dose)] <- 0

# Round values to 2 decimal places
columns_to_round <- c(
    "mediator_avg_daily_dose_mme_month1",
    "mediator_avg_daily_dose_mme_month2",
    "mediator_avg_daily_dose_mme_month3",
    "mediator_avg_daily_dose_mme_month4",
    "mediator_avg_daily_dose_mme_month5",
    "mediator_avg_daily_dose_mme_month6"
)

avg_30d_dose[, (columns_to_round) := lapply(.SD, round, digits = 2), .SDcols = columns_to_round]

saveRDS(avg_30d_dose, file.path(drv_root, "mediator_average_daily_dose_mme_per_month.rds")) 

# Average daily dose opioid dose in MME for all beneficiaries in the analysis cohort, considering 
# all months during the mediator period that a beneficiary was prescribed an opioid
overall_avg_daily_dose <- 
    out[, .(total_months = .N, 
            total_opioid_dose = sum(total_opioid_dose), 
            avg_30d_opioid_dose = first(avg_30d_opioid_dose)), by = BENE_ID
        ][, mediator_avg_daily_dose_mme_overall := fifelse(total_months > 1, 
                                                           total_opioid_dose / total_months / 30, 
                                                           avg_30d_opioid_dose)
          ][, .(BENE_ID, mediator_avg_daily_dose_mme_overall)]

setkey(overall_avg_daily_dose, BENE_ID)

# Join with analysis cohort
overall_avg_daily_dose <- merge(overall_avg_daily_dose, dts_cohorts[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
overall_avg_daily_dose[is.na(overall_avg_daily_dose)] <- 0

saveRDS(overall_avg_daily_dose, file.path(drv_root, "mediator_average_daily_dose_mme_overall.rds")) 
