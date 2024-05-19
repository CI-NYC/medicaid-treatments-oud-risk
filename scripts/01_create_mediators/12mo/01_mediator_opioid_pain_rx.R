# -------------------------------------
# Script: 01_mediator_opioid_pain_rx.R
# Author: Nick Williams
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

dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_12mos_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Read in opioid pain list
op <- readRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_opioid_pain_ndc.rds"))

# OTL ---------------------------------------------------------------------

# Filter OTL to opioid pain NDC
otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY")

otl <- select(otl, all_of(otl_vars)) |> 
    filter(NDC %in% op$NDC) |>
    collect() |> 
    as.data.table()

otl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                  LINE_SRVC_END_DT, 
                                  LINE_SRVC_BGN_DT)]

# Inner join with cohort 
otl <- unique(merge(otl, dts_cohorts, by = "BENE_ID"))
otl <- merge(otl, op[, c(1, 3, 4)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
otl <- otl[LINE_SRVC_BGN_DT %within% interval(washout_12mos_end_dt, 
                                              washout_12mos_end_dt + days(182)), 
           .(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, washout_12mos_end_dt, NDC, NDC_QTY, 
             flag_opioid_analgesic, flag_opioid_anesthetics)]

# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "CLM_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
    filter(NDC %in% op$NDC) |>
    collect() |> 
    as.data.table()

# Inner join with cohort 
rxl <- unique(merge(rxl, dts_cohorts, by = "BENE_ID"))
rxl <- merge(rxl, op[, c(1, 3, 4)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
rxl <- rxl[RX_FILL_DT %within% interval(washout_12mos_end_dt, 
                                        washout_12mos_end_dt + days(182)), 
           .(BENE_ID, CLM_ID, RX_FILL_DT, washout_12mos_end_dt, NDC, NDC_QTY, DAYS_SUPPLY,
             flag_opioid_analgesic, flag_opioid_anesthetics)]

# Export ------------------------------------------------------------------

saveRDS(otl, file.path(drv_root, "mediator_12mo_otl_opioid_pain_rx.rds"))
saveRDS(rxl, file.path(drv_root, "mediator_12mo_rxl_opioid_pain_rx.rds"))
