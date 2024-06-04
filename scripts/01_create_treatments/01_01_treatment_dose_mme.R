# -------------------------------------
# Script: 01_01_mediator_dose_mme.R
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

data_dir <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
proj_dir <- "projects/mediation_unsafe_pain_mgmt"

dts_cohorts <- read_rds("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds") |> 
    select(BENE_ID, washout_start_dt)

rxl_opioids <- read_rds(file.path(data_dir, "rxl_opioids_pain_mme.rds"))
otl_opioids <- read_rds(file.path(data_dir, "otl_opioids_pain_mme.rds"))

# calculate strength per day in Milligram Morphine Equivalent (MME) units
# no caps on number of pills, days supply, and pills per day
rxl_opioids <-
    rxl_opioids |>
    drop_na(BENE_ID) |>
    mutate(number_pills = case_when(!is.na(NDC_QTY) ~ abs(NDC_QTY),
                                    TRUE ~ 1),
           days_supply = case_when(!is.na(DAYS_SUPPLY) ~ DAYS_SUPPLY,
                                   TRUE ~ 1), # best assumption we can make if missing a days supply var
           pills_per_day = number_pills / days_supply,
           strength = parse_number(numeratorValue),
           strength_per_day = strength * pills_per_day,
           mme_strength_per_day = strength_per_day * conversion, 
           mme_strength_per_day = pmin(mme_strength_per_day, quantile(mme_strength_per_day, 0.99)))

# keep only relevant vars for RXL opioids
rxl_opioids <-
    rxl_opioids |>
    select(BENE_ID,
           opioid,
           NDC,
           dose_form,
           days_supply,
           pills_per_day,
           strength,
           strength_per_day,
           mme_strength_per_day,
           days_supply,
           rx_start_dt = RX_FILL_DT) |>
    mutate(rx_end_dt = rx_start_dt + days_supply) |>
    arrange(BENE_ID, rx_start_dt, opioid)

# filter to opioids for pain, calculate strength per day in Milligram Morphine Equivalent (MME) units
otl_opioids <-
    otl_opioids |>
    drop_na(BENE_ID) |>
    mutate(strength = parse_number(numeratorValue),
           # we assume all OTL opioids are one day supply (outpatient)
           mme_strength_per_day = strength * conversion) 

# summary(otl_opioids$mme_strength_per_day)

# keep only relevant vars for OTL opioids
otl_opioids <-
    otl_opioids |>
    select(BENE_ID,
           NDC,
           dose_form,
           opioid,
           strength,
           mme_strength_per_day,
           rx_start_dt = LINE_SRVC_BGN_DT) |>
    mutate(rx_end_dt = rx_start_dt + 1) |> # 1 day supply assumption
    arrange(BENE_ID, rx_start_dt, opioid)

# combine the two datasets
all_opioids_clean <- full_join(otl_opioids, rxl_opioids) |>
     left_join(dts_cohorts)

# save raw data set of all opioids for pain
saveRDS(all_opioids_clean, file.path(data_dir, "all_pain_opioids.rds"))

# nest by BENE_ID to make calculations easier later
all_opioids_clean_nest <-
    all_opioids_clean |>
    group_by(BENE_ID) |>
    nest()

saveRDS(all_opioids_clean_nest, file.path(data_dir, "all_pain_opioids_nest.rds"))
