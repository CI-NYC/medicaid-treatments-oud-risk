# -------------------------------------
# Script: 
# Author: Kat Hoffman
# Updated:
# Purpose: 
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

mme_months <- readRDS(file.path(drv_root, "mediator_12mo_average_daily_dose_mme_per_month.rds"))

cens_dts <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")
setDT(cens_dts)
cens_dts <- cens_dts[, .(BENE_ID, washout_start_dt, censoring_dts_cal_dt)]

tapering <- copy(mme_months)

# Tapering logic using only MME from the mediator window comparing first 2 mediator months to the consecutive 4.
tapering$period1_dose <- rowMeans(tapering[, .(mediator_avg_daily_dose_mme_month1, mediator_avg_daily_dose_mme_month2)])
tapering$period2_dose <- rowMeans(tapering[, .(mediator_avg_daily_dose_mme_month3, mediator_avg_daily_dose_mme_month4)])
tapering$period3_dose <- rowMeans(tapering[, .(mediator_avg_daily_dose_mme_month5, mediator_avg_daily_dose_mme_month6)])

tapering[, tapering := fcase(period1_dose < 50, 0, 
                             period1_dose >= 50 & 
                                 period2_dose <= period1_dose*0.85 & 
                                 period3_dose <= period1_dose*0.85, 1, 
                             default = 0)]

#  figure out which beneficiaries can't have tapering because not in study long enough
cens_dts[, days_from_washout := difftime( censoring_dts_cal_dt, washout_start_dt, units = "day")
         ][, months_to_cens := as.numeric(ceiling(days_from_washout / 30))
           ][, cens_tapering := fifelse(months_to_cens <= 15, 1, 0)]

tapering <- merge(cens_dts, tapering, all.x = TRUE)

tapering <- tapering[, .(BENE_ID, 
                         mediator_has_tapering = fcase(tapering == 1, 1, 
                                                       cens_tapering == 1, NA_real_, 
                                                       default = 0))]

saveRDS(tapering, file.path(drv_root, "mediator_12mo_has_tapering.rds"))
