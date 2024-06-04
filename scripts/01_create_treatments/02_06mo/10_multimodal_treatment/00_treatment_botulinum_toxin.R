# READ ME -----------------------------------------------------------------
#
# Author: Nick Williams
# Created: 2023-07-17
#
# Creates an indicator variable for whether or not an observation in
#   the analysis cohort had a claim for a Botulinum toxin injection
#   during the mediator period.
# 
# Note: File paths edited by SF on 2023-10-10
# -------------------------------------------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

setwd("disability/projects/mediation_unsafe_pain_mgmt")

src_root <- "/mnt/processed-data/disability"
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

mediator <- "Botulinum toxin injections"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# Read in cohort and dates
dts_cohorts <- as.data.table(readRDS(file.path("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")))
dts_cohorts <- dts_cohorts[, .(BENE_ID, washout_cal_end_dt)]
setkey(dts_cohorts, BENE_ID)

# Read in CPT, HCPC, and Modifier codes for mediator claims
codes <- read_yaml("data/mediator_codes.yml")
codes <- c(names(codes[[mediator]]$CPT), 
           names(codes[[mediator]]$HCPC), 
           names(codes[[mediator]]$Modifiers))

# Filter OTL to claims codes
claims_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |> 
    filter(LINE_PRCDR_CD %in% codes) |>
    collect() |> 
    as.data.table()

claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                     LINE_SRVC_END_DT, 
                                     LINE_SRVC_BGN_DT)]

# Inner join with cohort 
claims <- unique(merge(claims, dts_cohorts, by = "BENE_ID"))

# Filter to claims within mediator time-frame
claims <- claims[LINE_SRVC_BGN_DT %within% interval(washout_cal_end_dt, 
                                                    washout_cal_end_dt + days(182)), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, washout_cal_end_dt, LINE_PRCDR_CD)]

# Create indicator variable for whether or not a patient had claim in mediator period
# Right join with cohort
claims <- claims[, .(mediator_has_botulinum_toxin = as.numeric(.N > 0)), by = "BENE_ID"]
claims <- merge(claims, dts_cohorts[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
claims[, mediator_has_botulinum_toxin := fifelse(is.na(mediator_has_botulinum_toxin), 0, mediator_has_botulinum_toxin)]

saveRDS(claims, file.path(drv_root, "mediator_has_botulinum_toxin.rds"))
