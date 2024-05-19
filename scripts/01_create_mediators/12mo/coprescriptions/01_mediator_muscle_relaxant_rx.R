# -------------------------------------
# Script: 
# Author: 
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Analysis cohort
cohort <- readRDS(file.path(drv_root, "cohort_ids.rds"))

# Non-opioid pain rx data set
nop_rxl <- readRDS(file.path(drv_root, "mediator_12mo_rxl_nonopioid_pain_rx.rds"))
nop_otl <- readRDS(file.path(drv_root, "mediator_12mo_otl_nonopioid_pain_rx.rds"))

# Non-opioid NDC/ATC codes
nop_ndc <- readRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds"))

M03 <- nop_ndc[grepl("^M03", atc, ignore.case = TRUE)]

# Filter non-opioid RXL data set for muscle relaxants
mr_rxl <- 
    nop_rxl[, mediator_nonopioid_muscle_relaxant_rx := fifelse(NDC %in% M03$NDC, 1, 0)
            ][mediator_nonopioid_muscle_relaxant_rx == 1, ]

# Filter non-opioid OTL data set for muscle relaxants
mr_otl <- 
    nop_otl[, mediator_nonopioid_muscle_relaxant_rx := fifelse(NDC %in% M03$NDC, 1, 0)
            ][mediator_nonopioid_muscle_relaxant_rx == 1, ]

# Save
saveRDS(mr_rxl, file.path(drv_root, "mediator_12mo_rxl_muscle_relaxant_rx.rds"))
saveRDS(mr_otl, file.path(drv_root, "mediator_12mo_otl_muscle_relaxant_rx.rds"))

# Make binary -------------------------------------------------------------

# Combine both datasets and keep only unique rows
mr <- rbind(mr_otl[, .(BENE_ID, NDC, NDC_QTY)], 
            mr_rxl[, .(BENE_ID, NDC, NDC_QTY)])

mr <- unique(mr)

mr <- merge(cohort, mr, all.x = TRUE)

mr[, mediator_nonopioid_muscle_relaxant_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
mr <- unique(mr[, .(BENE_ID, mediator_nonopioid_muscle_relaxant_rx)])

saveRDS(mr, file.path(drv_root, "mediator_12mo_muscle_relaxant_rx_bin.rds"))
