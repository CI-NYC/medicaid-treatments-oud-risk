# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

data_dir <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
proj_dir <- "projects/mediation_unsafe_pain_mgmt"

# Read in cohort and dates
dts_cohorts <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")
setDT(dts_cohorts)
dts_cohorts <- dts_cohorts[, .(BENE_ID, disability_pain_cal)]
setkey(dts_cohorts, BENE_ID)

all_opioids_clean <- readRDS(file.path(data_dir, "all_pain_opioids.rds"))
setDT(all_opioids_clean)
setkey(all_opioids_clean, BENE_ID)
all_opioids_clean <- all_opioids_clean[, .(BENE_ID, NDC, opioid, mme_strength_per_day, rx_start_dt, rx_end_dt)]

# Merge to the analysis cohort
all_opioids_clean_merged <- merge(all_opioids_clean, dts_cohorts, by = "BENE_ID")

# Filter opioid prescriptions to only contain those within mediator period
all_opioids_clean_mediator_period <- all_opioids_clean_merged[
    all_opioids_clean_merged$rx_start_dt %within% interval(
        all_opioids_clean_merged$washout_cal_end_dt, all_opioids_clean_merged$washout_cal_end_dt + days(182)
    ), 
]

opioid_names <- all_opioids_clean_mediator_period[, .(BENE_ID, opioid)]

opioid_names[, opioid := fifelse(opioid %in% c("remifentanil", "alfentanil", "sufentanil"), "fentanyl", opioid)]
opioid_names[, opioid := fifelse(opioid %in% c("butorphanol", 
                                               "meperidine", 
                                               "nalbuphine", 
                                               "oxymorphone", 
                                               "pentazocine", 
                                               "propoxyphene", 
                                               "tapentadol"), "other", opioid)]


opioids <- dcast(opioid_names, BENE_ID ~ opioid, value.var = "opioid", fun.aggregate = \(x) length(x) > 0)

opioids <- merge(dts_cohorts, opioids, all.x = TRUE)
opioids[is.na(opioids)] <- FALSE

saveRDS(opioids, file.path(data_dir, "opioid_names_wide.rds"))
