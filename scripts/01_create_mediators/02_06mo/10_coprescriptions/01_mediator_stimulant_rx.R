# -------------------------------------
# Script: 01_mediator_stimulant.R
# Author: Nick Williams
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in RXL (pharmacy line)
files <- paste0(list.files(src_root, pattern = "TAFRXL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxl <- open_dataset(file.path(src_root, parquet_files))

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# Read in cohort and dates
dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_cal_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Read in stimulant list
stim <- readRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_stimulant_ndc.rds"))

# OTL ---------------------------------------------------------------------

# Filter OTL to stimulant NDC
otl_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY")

otl <- select(otl, all_of(otl_vars)) |> 
    filter(NDC %in% stim$NDC) |>
    collect() |> 
    as.data.table()

otl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                  LINE_SRVC_END_DT, 
                                  LINE_SRVC_BGN_DT)]

# Inner join with cohort 
otl <- unique(merge(otl, dts_cohorts, by = "BENE_ID"))
otl <- merge(otl, stim[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
otl <- otl[LINE_SRVC_BGN_DT %within% interval(washout_cal_end_dt, 
                                              washout_cal_end_dt + days(182)), 
           .(BENE_ID, LINE_SRVC_BGN_DT, washout_cal_end_dt, NDC, NDC_QTY, flag_stim)]

# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
    filter(NDC %in% stim$NDC) |>
    collect() |> 
    as.data.table()

# Inner join with cohort 
rxl <- unique(merge(rxl, dts_cohorts, by = "BENE_ID"))
rxl <- merge(rxl, stim[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
rxl <- rxl[RX_FILL_DT %within% interval(washout_cal_end_dt, 
                                        washout_cal_end_dt + days(182)), 
           .(BENE_ID, RX_FILL_DT, washout_cal_end_dt, NDC, NDC_QTY, DAYS_SUPPLY, flag_stim)]

# Export ------------------------------------------------------------------

saveRDS(otl, file.path(drv_root, "mediator_otl_stimulant_rx.rds"))
saveRDS(rxl, file.path(drv_root, "mediator_rxl_stimulant_rx.rds"))

# Make binary -------------------------------------------------------------

# Combine both datasets and keep only unique rows
stim <- rbind(otl[, .(BENE_ID, NDC, NDC_QTY, flag_stim)], 
              rxl[, .(BENE_ID, NDC, NDC_QTY, flag_stim)])

stim <- unique(stim)

stim <- merge(dts_cohorts, stim, all.x = TRUE)

stim[, mediator_stim_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
stim <- unique(stim[, .(BENE_ID, mediator_stim_rx)])

saveRDS(stim, file.path(drv_root, "mediator_stimulant_rx_bin.rds"))
