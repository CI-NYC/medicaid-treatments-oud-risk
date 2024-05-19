# READ ME -----------------------------------------------------------------
#
#       Author: Sarah Forrest
#      Created: 25 Aug 2023
#  Last edited: 2024-01-29 (Nick)
#
#       Output: A dataset with 6 variables indicating the number of unique 
#               prescribers each beneficiary in the analysis cohort received an 
#               opioid prescription from during each month of the mediator 
#               period, as well as a summary variable indicating the total 
#               number of unique prescribers during the full 6-month mediator
#               period
# 
# -------------------------------------------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

files <- paste0(list.files(src_root, pattern = "TAFRXH", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxh <- open_dataset(file.path(src_root, parquet_files))

# RXL
rxl <- readRDS(file.path(drv_root, "mediator_12mo_rxl_opioid_pain_rx.rds"))

# Read in cohort and dates
dts_cohorts <- as.data.table(readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds"))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_12mos_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Opioids
op <- readRDS(file.path(drv_root, "opioids_mme.rds"))

# Create prescribers dataset ---------------------------------------------------

rxl_claims <- unique(rxl$CLM_ID)

rhl_vars <- c("BENE_ID", "CLM_ID", "RX_FILL_DT", "PRSCRBNG_PRVDR_ID")

prescribers <- select(rxh, all_of(rhl_vars)) |>
    filter(CLM_ID %in% rxl_claims) |>
    collect() |> 
    as.data.table()

# Merge ------------------------------------------------------------------------

rxl <- rxl[, .(BENE_ID, CLM_ID, RX_FILL_DT, NDC)]

prescribers <- 
    merge(prescribers, rxl, by = c("BENE_ID", "CLM_ID", "RX_FILL_DT")) |> 
    unique()

prescribers <- merge(dts_cohorts, prescribers)

# Create prescriber count variables  -------------------------------------------

# Set up month variables
prescribers[, c("month1", "month2", "month3", "month4", "month5", "month6") := 
                .(RX_FILL_DT %within% interval(washout_12mos_end_dt, washout_12mos_end_dt + days(30)),
                  RX_FILL_DT %within% interval(washout_12mos_end_dt + days(31), washout_12mos_end_dt + days(61)),
                  RX_FILL_DT %within% interval(washout_12mos_end_dt + days(62), washout_12mos_end_dt + days(92)),
                  RX_FILL_DT %within% interval(washout_12mos_end_dt + days(93), washout_12mos_end_dt + days(122)),
                  RX_FILL_DT %within% interval(washout_12mos_end_dt + days(123), washout_12mos_end_dt + days(152)),
                  RX_FILL_DT %within% interval(washout_12mos_end_dt + days(153), washout_12mos_end_dt + days(182)))]

# Calculate the sum of unique prescriber IDs for each month and sum variable for the 6-month period
prescribers_per_month <- 
    prescribers[, .(mediator_prescribers_month1 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month1])),
                    mediator_prescribers_month2 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month2])),
                    mediator_prescribers_month3 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month3])),
                    mediator_prescribers_month4 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month4])),
                    mediator_prescribers_month5 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month5])),
                    mediator_prescribers_month6 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month6])),
                    mediator_prescribers_6mo_sum = uniqueN(c(PRSCRBNG_PRVDR_ID[month1],
                                                             PRSCRBNG_PRVDR_ID[month2],
                                                             PRSCRBNG_PRVDR_ID[month3],
                                                             PRSCRBNG_PRVDR_ID[month4],
                                                             PRSCRBNG_PRVDR_ID[month5],
                                                             PRSCRBNG_PRVDR_ID[month6]))),
                by = BENE_ID]

# Merge with analysis cohort  --------------------------------------------------

# Right join with the analysis cohort
mediator_prescribers_per_month <- 
    merge(prescribers_per_month, dts_cohorts[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")

# Replace NA with 0 for each month variable
cols_to_replace <- c("mediator_prescribers_month1", "mediator_prescribers_month2", "mediator_prescribers_month3", "mediator_prescribers_month4", "mediator_prescribers_month5", "mediator_prescribers_month6", "mediator_prescribers_6mo_sum")
mediator_prescribers_per_month[, (cols_to_replace) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = cols_to_replace]

# Save final dataset -----------------------------------------------------------

saveRDS(mediator_prescribers_per_month, file.path(drv_root, "mediator_12mo_prescribers_per_month.rds"))
