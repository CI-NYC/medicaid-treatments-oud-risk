# -------------------------------------
# Script: state_counts.R
# Author: Nick Williams
# Updated:
# Purpose: Percentages of the Medicaid cohort that resides in each of the 25 states considered
# Notes:
# -------------------------------------

library(arrow)
library(data.table)
library(kableExtra)

td <- "/mnt/processed-data/disability/"
dbs_files <- paste0(list.files(td, pattern = "*TAFDEBSE_\\d+\\.parquet", recursive = TRUE))
dbs <- open_dataset(paste0(td, dbs_files), format = "parquet", partition = "year")

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

cohort <- readRDS(file.path(drv_root, "mediation_analysis_df.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)
cohort <- cohort[, .(BENE_ID, disability_pain_cal)]

states <-
  dbs |> 
  select(BENE_ID, STATE_CD)  |>
  collect() |> 
  as.data.table()

cohort <- merge(cohort, states, by = "BENE_ID", all.x = TRUE)
cohort <- unique(cohort)

make_prop_table <- \(x) prop.table(table(x))

state_prop <- cbind(
  make_prop_table(cohort[disability_pain_cal == "disability and chronic pain", STATE_CD]),
  make_prop_table(cohort[disability_pain_cal == "chronic pain only", STATE_CD]),
  make_prop_table(cohort[disability_pain_cal %in% 
                           c("disability and chronic pain", "chronic pain only"), 
                         STATE_CD])
)

colnames(state_prop) <- c("Disability and chronic pain", "Chronic pain only", "Both")
state_prop <- as.data.table(state_prop, keep.rownames = TRUE)

state_prop[, c("Disability and chronic pain", "Chronic pain only", "Both") := 
             lapply(.SD, \(x) sprintf("%.1f%%", x * 100)), 
           .SDcols = c("Disability and chronic pain", "Chronic pain only", "Both")]

kbl(state_prop, format = "latex", booktabs = TRUE)

