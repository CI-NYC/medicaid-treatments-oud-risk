# -------------------------------------
# Script: 04_mediator_nonopioid_pain_rx_bin.R
# Author: Nick Williams
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(dplyr)
library(lubridate)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")
setDT(cohort, key = "BENE_ID")

# Non-opioid pain rx data set
nop_rxl <- readRDS(file.path(drv_root, "mediator_rxl_nonopioid_pain_rx.rds"))
nop_otl <- readRDS(file.path(drv_root, "mediator_otl_nonopioid_pain_rx.rds"))

# Non-opioid NDC/ATC codes
nop_ndc <- readRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds"))

A03D <- nop_ndc[grepl("^A03D", atc, ignore.case = TRUE)]
A03EA <- nop_ndc[grepl("^A03EA", atc, ignore.case = TRUE)]
M01 <- nop_ndc[grepl("^M01", atc, ignore.case = TRUE)]
M02A <- nop_ndc[grepl("^M02A", atc, ignore.case = TRUE)]
M03 <- nop_ndc[grepl("^M03", atc, ignore.case = TRUE)]
N02B <- nop_ndc[grepl("^N02B", atc, ignore.case = TRUE)]
N06A <- nop_ndc[grepl("^N06A", atc, ignore.case = TRUE)]

nop_rxl[, `:=`(mediator_nonopioid_antiinflam = fifelse(NDC %in% M01$NDC, 1, 0), 
               mediator_nonopioid_topical = fifelse(NDC %in% M02A$NDC, 1, 0), 
               mediator_nonopioid_muscle_relax = fifelse(NDC %in% M03$NDC, 1, 0), 
               mediator_nonopioid_other_analgesic = fifelse(NDC %in% N02B$NDC, 1, 0), 
               mediator_nonopioid_antidep = fifelse(NDC %in% N06A$NDC, 1, 0))]

nop_otl[, `:=`(mediator_nonopioid_antiinflam = fifelse(NDC %in% M01$NDC, 1, 0), 
               mediator_nonopioid_topical = fifelse(NDC %in% M02A$NDC, 1, 0), 
               mediator_nonopioid_muscle_relax = fifelse(NDC %in% M03$NDC, 1, 0), 
               mediator_nonopioid_other_analgesic = fifelse(NDC %in% N02B$NDC, 1, 0), 
               mediator_nonopioid_antidep = fifelse(NDC %in% N06A$NDC, 1, 0))]

# Make binary -------------------------------------------------------------

# Combine both datasets and keep only unique rows
nop <- rbind(nop_otl[, .(BENE_ID, NDC, NDC_QTY, 
                         mediator_nonopioid_antiinflam, 
                         mediator_nonopioid_topical,
                         mediator_nonopioid_muscle_relax,
                         mediator_nonopioid_other_analgesic,
                         mediator_nonopioid_antidep)], 
             nop_rxl[, .(BENE_ID, NDC, NDC_QTY, 
                        mediator_nonopioid_antiinflam, 
                        mediator_nonopioid_topical,
                        mediator_nonopioid_muscle_relax,
                        mediator_nonopioid_other_analgesic,
                        mediator_nonopioid_antidep)])

nop <- unique(nop)

nop <- group_by(nop, BENE_ID) |> 
    summarize(across(starts_with("mediator"), \(x) as.numeric(sum(x) > 0))) |> 
    as.data.table(key = "BENE_ID")

nop <- merge(cohort[, .(BENE_ID)], nop, all.x = TRUE)
nop[is.na(nop)] <- 0

saveRDS(nop, file.path(drv_root, "mediator_nop_binary_refactor.rds"))
