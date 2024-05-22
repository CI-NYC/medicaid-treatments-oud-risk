# -------------------------------------
# Script: 
# Author: 
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(data.table)
library(purrr)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Create a list of mediator datasets
mediator_files <- c(
    # Opioid pain management
    # Opioid pain medication
    "mediator_opioid_pain_rx_bin.rds",
    # Dose, Duration
    "mediator_max_daily_dose_mme.rds",
    # High-risk opioid prescribing practices
    "mediator_prescribers_per_month.rds",             # 1 variable for each month (1-6) and 1 overall variable for throughout the mediator period
    "mediator_average_daily_dose_mme_per_month.rds",  # 1 variable for avg daily dose for each month (1-6)
    "mediator_average_daily_dose_mme_overall.rds",    # 1 overall variable for avg daily dose for months throughout the mediator period with an opioid prescription
    # Co-prescription
    "mediator_opioid_coprescriptions.rds",
    # Tapering
    "mediator_has_tapering.rds",
    # Non-opioid pain medication
    "mediator_nop_binary_refactor.rds",
    # Physical therapy
    "mediator_has_physical_therapy.rds",
    # Other categories for multimodal/multidisciplinary pain treatment
    "mediator_has_multimodal_pain_treatment_restrict.rds",
    # Counseling
    "mediator_has_counseling.rds",
    # Proportion of days during mediator period with opioid prescription
    "mediator_proportion_days_opioid.rds"
)

# Read in all mediator datasets from list above
mediator_list <- lapply(mediator_files, function(file) {
    data <- readRDS(file.path(drv_root, file))
    setDT(data)
    setkey(data, BENE_ID)
    data[]
})

# Read in opioid pain duration dataset and select only necessary variables
mediator_months_opioid_rx <- readRDS(file.path(drv_root, "mediator_months_opioid_prescription.rds"))

setDT(mediator_months_opioid_rx)
setkey(mediator_months_opioid_rx, BENE_ID)

mediator_months_opioid_rx <- mediator_months_opioid_rx[, .(BENE_ID, mediator_months_opioid_rx)]

# Merge mediator datasets ------------------------------------------------------

mediator_df <- reduce(mediator_list, merge, all.x = TRUE, all.y = TRUE)
mediator_df <- merge(mediator_df, mediator_months_opioid_rx, all.x = TRUE)

# replace NA with 0 (tapering and dose variables)
mediator_df[is.na(mediator_df)] <- 0

rm(mediator_list, mediator_months_opioid_rx)

# Create additional mediator variables -----------------------------------------

# Save dataset
saveRDS(mediator_df, file.path(drv_root, "mediator_df.rds"))
