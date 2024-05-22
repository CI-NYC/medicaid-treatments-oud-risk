# -------------------------------------
# Script: 00_mediator_dose_per_month.R
# Author: Kat Hoffman
# Updated:
# Purpose: Outputs an object which contains monthly summaries for the first 12 months
# of study enrollment
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

summarize_opioids2 <- function(data) {
    setDT(data)
    
    data[, (c("rx_start_dt", "rx_end_dt")) := lapply(.SD, as.Date), 
         .SDcols = c("rx_start_dt", "rx_end_dt")]
    
    long <- data[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                     mme_strength_per_day, opioid, washout_start_dt), by = seq_len(nrow(data))
    ][, seq_len := NULL][]
    
    long <- long[, days_from_washout := difftime(date, washout_start_dt, units = "day")
    ][, weeks_from_washout := as.numeric(ceiling(days_from_washout / 7))
    ][, months_from_washout := as.numeric(ceiling(days_from_washout / 30))
    ][, .(total_opioid_dose = sum(mme_strength_per_day), 
          avg_30d_opioid_dose = sum(mme_strength_per_day) / 30, 
          num_unique_opioids = n_distinct(opioid)), by = "months_from_washout"
    ][, any_opioid := fifelse(total_opioid_dose > 0, 1, 0)
    ]
    setorder(long, months_from_washout)
    long[]
}

# Read in cohort/dates
dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_cal_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Read in opioids
opioids <- readRDS(file.path(drv_root, "all_pain_opioids.rds"))
setDT(opioids)
setkey(opioids, BENE_ID)

opioids <- merge(opioids, dts_cohorts, by = "BENE_ID")

# Filter opioid prescriptions to only contain those within baseline or mediator period
opioids <- opioids[rx_start_dt %within% interval(washout_start_dt, 
                                                 washout_cal_end_dt + days(182)), ]

opioids[, washout_cal_end_dt := NULL]

opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]

plan(multisession)

tic()
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                   out <- summarize_opioids2(data)
                   out$BENE_ID <- id
                   setcolorder(out, "BENE_ID")
                   out
               }
out <- rbindlist(out)
toc()
# ~ 6.75 minutes

# Filter data for first 11 months + 29 days after washout
out <- out[months_from_washout <= 11]

plan(sequential)

saveRDS(out, file.path(drv_root, "mme_month_summary.rds"))
