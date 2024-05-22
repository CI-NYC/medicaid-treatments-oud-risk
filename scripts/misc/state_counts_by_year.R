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

open_ds <- function(path) {
  dbs_files <- paste0(list.files(path, pattern = "*TAFDEBSE_\\d+\\.parquet", recursive = TRUE))
  open_dataset(paste0(path, dbs_files), format = "parquet")
}

dbs_2016 <- open_ds("/mnt/processed-data/disability/2016/")
dbs_2017 <- open_ds("/mnt/processed-data/disability/2017/")
dbs_2018 <- open_ds("/mnt/processed-data/disability/2018/")
dbs_2019 <- open_ds("/mnt/processed-data/disability/2019/")

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

cohort <- readRDS(file.path(drv_root, "mediation_analysis_df.rds"))
setDT(cohort)
setkey(cohort, BENE_ID)
cohort <- cohort[, .(BENE_ID, disability_pain_cal, washout_start_dt, dem_race)]
cohort[, year := year(washout_start_dt)]

collect_states <- function(x) {
  x |> 
    select(BENE_ID, STATE_CD)  |>
    collect() |> 
    as.data.table()
}

states_2016 <- collect_states(dbs_2016)
states_2017 <- collect_states(dbs_2017)
states_2018 <- collect_states(dbs_2018)
states_2019 <- collect_states(dbs_2019)

states_2016[, year := 2016]
states_2017[, year := 2017]
states_2018[, year := 2018]
states_2019[, year := 2019]

cohort_2016 <- merge(cohort, states_2016, by = c("BENE_ID", "year"))
cohort_2017 <- merge(cohort, states_2017, by = c("BENE_ID", "year"))
cohort_2018 <- merge(cohort, states_2018, by = c("BENE_ID", "year"))
cohort_2019 <- merge(cohort, states_2019, by = c("BENE_ID", "year"))

cohort2 <- rbindlist(
  list(
    cohort_2016, 
    cohort_2017, 
    cohort_2018, 
    cohort_2019
  )
)

cohort2 <- cohort2[disability_pain_cal %in% 
                     c("disability and chronic pain", 
                       "chronic pain only")] |> 
  unique()

# Dealing with the few observations that have multiple states, just selecting the first row...
cohort2 <- cohort2[, .SD[1], by = BENE_ID]

state_years <- with(cohort2, table(STATE_CD, year))

# Percent of observations in state per year
prop.table(state_years)*100

# Percent of observations in state for combined years 2017/2018
cbind(state_years[, c("2016", "2019")], 
      as.matrix(state_years[, "2017"] + state_years[, "2018"])) |> 
  prop.table() |> 
  (\(x) x*100)()

cohort2[STATE_CD != "RI" & 
          !(STATE_CD == "MI" & year %in% 2017:2018) & 
          !(STATE_CD == "MA" & year == 2019)
        ][, sum(is.na(dem_race)) / .N * 100]
