# -------------------------------------
# Script: 
# Author: Nick Williams
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in cohort and dates
dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/sens_12mos_analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_12mos_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Read in opioid pain list
otl <- readRDS(file.path(drv_root, "mediator_12mo_otl_opioid_pain_rx.rds"))
rxl <- readRDS(file.path(drv_root, "mediator_12mo_rxl_opioid_pain_rx.rds"))

rx <- rbindlist(list(otl[, setnames(.SD, "LINE_SRVC_BGN_DT", "RX_DT")], 
                     rxl[, setnames(.SD, "RX_FILL_DT", "RX_DT")][, !"DAYS_SUPPLY"])) |> 
    unique()

rx[, `:=`(month1 = RX_DT %within% interval(washout_12mos_end_dt, washout_12mos_end_dt + days(30)), 
          month2 = RX_DT %within% interval(washout_12mos_end_dt + days(31), washout_12mos_end_dt + days(61)), 
          month3 = RX_DT %within% interval(washout_12mos_end_dt + days(62), washout_12mos_end_dt + days(92)), 
          month4 = RX_DT %within% interval(washout_12mos_end_dt + days(93), washout_12mos_end_dt + days(122)), 
          month5 = RX_DT %within% interval(washout_12mos_end_dt + days(123), washout_12mos_end_dt + days(152)), 
          month6 = RX_DT %within% interval(washout_12mos_end_dt + days(153), washout_12mos_end_dt + days(182)))]

opioid_months <- rx[ , lapply(.SD, any), by = BENE_ID, .SDcols = paste0("month", 1:6)
                     ][, .(BENE_ID, mediator_months_opioid_rx = rowSums(.SD)), .SDcols = paste0("month", 1:6)]

opioid_months <- merge(dts_cohorts, opioid_months, by = "BENE_ID", all.x = TRUE)
opioid_months[, mediator_months_opioid_rx := fifelse(is.na(mediator_months_opioid_rx), 
                                                     0, mediator_months_opioid_rx)]

saveRDS(opioid_months, file.path(drv_root, "mediator_12mo_months_opioid_prescription.rds"))
