# -------------------------------------
# Script: 01_00_mediator_dose_mme.R
# Author: Kat Hoffman
# Updated: Nick Williams
# Purpose: 
# Notes:
# -------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)

td <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Read in RXL (pharmacy line)
files <- paste0(list.files(td, pattern = "TAFRXL_(.+).parquet", recursive = TRUE))
rxl <- open_dataset(file.path(td, files), format = "parquet")

#  Read in OTL (Other services line) 
files <- paste0(list.files(td, pattern = "TAFOTL(.+)_(.+).parquet", recursive = TRUE))
otl <- open_dataset(file.path(td, files), format = "parquet")

mme <- readRDS(file.path(drv_root, "opioids_mme.rds"))

rxl_opioids <-
    rxl |>
    filter(NDC %in% mme$NDC) |>
    select(BENE_ID, CLM_ID, RX_FILL_DT, contains("ndc"), DAYS_SUPPLY) |>
    collect() |>
    left_join(mme)

saveRDS(rxl_opioids, file.path(drv_root, "rxl_opioids_pain_mme.rds"))

otl_opioids <- 
    otl |>
    filter(NDC %in% mme$NDC) |>
    mutate(LINE_SRVC_BGN_DT = ifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT)) |>
    select(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, contains("NDC")) |>
    collect()|>
    left_join(mme)

saveRDS(otl_opioids, file.path(drv_root, "otl_opioids_pain_mme.rds"))
