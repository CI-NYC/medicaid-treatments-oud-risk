# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-11-02
# Last updated: 2023-11-16 (Nick)
#
# Creates 3 output datasets with beneificary IDs and the day that they started
#   MOUD (but, met, and nal)
# 
# Uses code originally written by Kat Hoffman in https://github.com/CI-NYC/disability/projects/create_cohort/scripts/06_define_OUD_components folder:
# - define_moud_bup.R
# - define_moud_met.R
# - define_moud_nal.R
#
# -------------------------------------------------------------------------

library(arrow)
library(tidyverse)
library(tidylog)
library(lubridate)
library(data.table)
library(tictoc)
library(here)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

dts_cohorts <- open_dataset("/mnt/general-data/disability/create_cohort/intermediate/tafdedts/dts_cohorts.parquet") |>
  collect()

td <- "/mnt/processed-data/disability"

# read in OTL (other services line)
files <- paste0(list.files(td, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(td, parquet_files))

# Read in RXL (pharmacy line)
files <- paste0(list.files(td, pattern = "TAFRXL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxl <- open_dataset(file.path(td, parquet_files))

# Read in bup NDC list
best_list <- readRDS(here("projects/define_moud/input/best_list.rds"))

# Bup --------------------------------------------------------------------------

best_rxl <-
  rxl |>
  filter(NDC %in% best_list$ndc) |>
  select(BENE_ID, 
         NDC,
         CLM_ID,
         NDC_UOM_CD, 
         NDC_QTY,
         DAYS_SUPPLY,
         RX_FILL_DT) |>
  collect() 

# filter BUP scripts, merge BUP NDC-dose map to compute strength/day
best_rxl_all <-
  best_rxl |>
  left_join(best_list |> rename(NDC = ndc)) |>
  mutate(pills_per_day = NDC_QTY/DAYS_SUPPLY,
         strength_per_day = strength * pills_per_day)

# only keep relevant strengths/day of BUP scripts
best_rxl_clean <-
  best_rxl_all |>
  mutate(keep = case_when(check == 0 ~ 1, # keep  if don't need to check
                          check == 1 & strength_per_day >= 10 & strength_per_day < 50 ~ 1, # less than 10 counts towards probable misuse (and chronic pain)
                          TRUE ~ 0
  )) |>
  filter(keep == 1)  |>
  mutate(moud_end_dt = RX_FILL_DT + days(DAYS_SUPPLY + 21)) |> 
  select(BENE_ID, NDC, moud_med, form, moud_start_dt = RX_FILL_DT, moud_end_dt) 

# Extract all of the scripts (NDC codes) in Other Services
best_otl <-
  otl |>
  filter(NDC %in% best_list$ndc) |>
  mutate(LINE_SRVC_BGN_DT = case_when(is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, TRUE ~ LINE_SRVC_BGN_DT)) |>
  select(BENE_ID,
         CLM_ID,
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_SRVC_END_DT,
         LINE_PRCDR_CD,
         LINE_PRCDR_CD_SYS,
         ACTL_SRVC_QTY,
         ALOWD_SRVC_QTY)  |>
  collect() 

best_otl <- best_otl |> 
    left_join(best_list |> rename(NDC = ndc)) # add best list of buprenorphine back into the full data

# save the bup injections (definitely MOUD) -- 30 days supply
best_injections_otl <-
  best_otl |>
  filter(form == "injection") |>
  select(BENE_ID, NDC, moud_med, form, moud_start_dt = LINE_SRVC_BGN_DT) |>
  mutate(moud_end_dt = moud_start_dt + days(30) + days(21))

# Bup-nal, definitely MOUD -- assuming 1 day supply
best_nal_otl <- 
  best_otl |>
  mutate(form %in% c("tablet","film"), check == 0) |>
  select(BENE_ID, NDC, moud_med, moud_start_dt = LINE_SRVC_BGN_DT, form) |>
  mutate(moud_end_dt =  moud_start_dt + days(21))

# not bup-nal, need to check
best_check_otl <- 
  best_otl |>
  mutate(form %in% c("tablet","film"), check == 1) 

# compute strength / day, ultimately will check if strength / day is > 10mg
best_checked_otl <-
  best_check_otl |>
  mutate(strength_times_quantity = case_when(NDC_UOM_CD == "UN" ~ strength * NDC_QTY, TRUE ~  strength)) |>
  group_by(BENE_ID, LINE_SRVC_BGN_DT) |>
  add_count() |>
  summarize(strength_per_day = sum(strength_times_quantity))

best_otl_over10mg <-
  best_checked_otl |>
  filter(strength_per_day >= 10) |>
  select(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT) |>
  mutate(moud_end_dt =  moud_start_dt + days(21),
         moud_med = "bup")

# pull HCPCS codes --------------------------------------------------------

best_hcpcs <- c(
  "J0570",	# Buprenorphine implant, 74.2 mg
  "J0572",	# Buprenorphine/naloxone, oral, less than or equal to 3 mg buprenorphine
  "J0573",	# Buprenorphine/naloxone, oral, greater than 3 mg, but less than or equal to 6 mg buprenorphine
  "J0574",	# Buprenorphine/naloxone, oral, greater than 6 mg, but less than or equal to 10 mg buprenorphine
  "J0575",	# Buprenorphine/naloxone, oral, greater than 10 mg buprenorphine
  "Q9991",	# Injection, buprenorphine extended-release (sublocade), less than or equal to 100 mg
  "Q9992"	# Injection, buprenorphine extended-release (sublocade), greater than 100 mg
  # None of the "G****" codes are picked up in the data.
  # "G2068",	# Medication assisted treatment, buprenorphine (oral); weekly bundle including dispensing and/or administration, substance use counseling, individual and group therapy, and toxicology testing if performed (provision of the services by a medicare-enrolled opioid treatment program)
  # "G2069",	# Medication assisted treatment, buprenorphine (injectable); weekly bundle including dispensing and/or administration, substance use counseling, individual and group therapy, and toxicology testing if performed (provision of the services by a medicare-enrolled opioid treatment program)
  # "G2070",	# Medication assisted treatment, buprenorphine (implant insertion); weekly bundle including dispensing and/or administration, substance use counseling, individual and group therapy, and toxicology testing if performed (provision of the services by a medicare-enrolled opioid treatment program)
  # "G2071",	# Medication assisted treatment, buprenorphine (implant removal); weekly bundle including dispensing and/or administration, substance use counseling, individual and group therapy, and toxicology testing if performed (provision of the services by a medicare-enrolled opioid treatment program) 
  # "G2072",	# Medication assisted treatment, buprenorphine (implant insertion and removal); weekly bundle including dispensing and/or administration, substance use counseling, individual and group therapy, and toxicology testing if performed (provision of the services by a medicare-enrolled opioid treatment program)
  # "G2079" 	# Take-home supply of buprenorphine (oral); up to 7 additional day supply (provision of the services by a medicare-enrolled opioid treatment program); list separately in addition to code for primary procedure
)

best_hcpcs_otl <-
  otl |>
  filter(LINE_PRCDR_CD %in% best_hcpcs) |>
  mutate(LINE_SRVC_BGN_DT = case_when(is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, TRUE ~ LINE_SRVC_BGN_DT),
  ) |>
  select(BENE_ID,
         STATE_CD, 
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_SRVC_END_DT,
         LINE_PRCDR_CD,
         LINE_PRCDR_CD_SYS,
         ACTL_SRVC_QTY,
         ALOWD_SRVC_QTY) |>
  collect()

best_hcpcs_otl |> count(LINE_PRCDR_CD)

bup_hcpcs_otl <-
  best_hcpcs_otl |>
  mutate(moud_med = "bup",
         form = case_when(LINE_PRCDR_CD == "J0570" ~ "implant",
                          str_detect(LINE_PRCDR_CD, "Q") ~ "injection",
                          str_detect(LINE_PRCDR_CD, "J") ~ "tablet"),
         moud_start_dt = LINE_SRVC_BGN_DT,
         moud_end_dt = case_when(form == "implant" ~ moud_start_dt + 21 + 182,
                                 form == "injection" ~ moud_start_dt + 21 + 30,
                                 form == "tablet" ~ moud_start_dt + 21 
         )) |> # bup implants last 6 months
  select(BENE_ID, moud_med, form, moud_start_dt, moud_end_dt)

# Combine all sources of bup moud
best_all_bup <- full_join(best_injections_otl, best_nal_otl) |>
  full_join(best_otl_over10mg) |>
  full_join(best_rxl_clean) |>
  full_join(bup_hcpcs_otl) |>
  distinct(BENE_ID, moud_start_dt, moud_end_dt) |>
  group_by(BENE_ID) |>
  arrange(BENE_ID, moud_start_dt)

# adjudicate the buprenorphine information depending on when the beneficiary received the next dose
# check when the next/last injection was given, and how long that was since the last injection ended (injection + 28 days)
best_all_bup_adj <-
  best_all_bup |>
  drop_na(BENE_ID) |>
  mutate(lag_moud_end_dt = lag(moud_end_dt), # when did the last bup end?
         days_since_last_moud = as.integer(difftime(moud_start_dt, lag_moud_end_dt, units = "days")),
         lead_moud_start_dt = lead(moud_start_dt), # when is the next dose starting compared to last moud end date
         days_to_next_moud = as.integer(difftime(lead_moud_start_dt, moud_end_dt, units = "days")))  |>
  ungroup() |>
  # indicator of whether to use the moud_start_dt/moud_end_dt or ignore them
  mutate(use_start_dt = case_when(is.na(days_since_last_moud) ~ 1,
                                  days_since_last_moud > 0 ~ 1,
                                  TRUE ~ 0),
         
         use_end_dt = case_when(is.na(days_to_next_moud) ~ 1,
                                days_to_next_moud > 0 ~ 1,
                                TRUE ~ 0))

# keep only the needed rows for start dates
best_all_bup_start_dts <-
  best_all_bup_adj |>
  filter(use_start_dt == 1) |>
  select(BENE_ID, moud_start_dt) |>
  ungroup()

# keep only the needed rows for end dates (same N and order as all_bup_start_dts)
best_all_bup_end_dts <-
  best_all_bup_adj |>
  filter(use_end_dt == 1) |>
  select(BENE_ID, moud_end_dt) |>
  ungroup()

# merge to one final bup start/stop data set
best_all_bup_start_stop <-
  best_all_bup_start_dts |>
  bind_cols(best_all_bup_end_dts |> select(-BENE_ID)) |>
  mutate(moud_med = "bup") |>
  select(BENE_ID, moud_med, everything()) |>
  left_join(dts_cohorts |> select(BENE_ID, washout_start_dt)) |>
  filter(!(moud_end_dt < washout_start_dt)) |> # filter out rows that end before washout period begins
  select(-washout_start_dt)

saveRDS(best_all_bup_start_stop, file.path(drv_root, "bup_start_dt.rds"))

# Met --------------------------------------------------------------------------

met_hcpcs <- c("H0020", "G2067", "G2078", "S0109")

met_hcpcs_otl <-
  otl |>
  filter(LINE_PRCDR_CD %in% met_hcpcs) |>
  mutate(LINE_SRVC_BGN_DT = case_when(is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, TRUE ~ LINE_SRVC_BGN_DT),
  ) |>
  select(BENE_ID,
         STATE_CD, 
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_PRCDR_CD
  ) |>
  collect() |>
  mutate(year = year(LINE_SRVC_BGN_DT)) |>
  filter((LINE_PRCDR_CD == "S0109" & STATE_CD == "IA" & year == 2016) |
           LINE_PRCDR_CD != "S0109") |>
  mutate(moud_med = "met",
         form = "tablet",
         moud_start_dt = LINE_SRVC_BGN_DT,
         moud_end_dt = moud_start_dt + 21) |> # met implants last 1 day
  select(BENE_ID, moud_med, form, moud_start_dt, moud_end_dt)

# adjudicate the methadone information depending on when the beneficiary received the next dose
# check when the next/last injection was given, and how long that was since the last injection ended (injection + 28 days)
met_adj <-
  met_hcpcs_otl |>
  drop_na(BENE_ID) |>
  mutate(lag_moud_end_dt = lag(moud_end_dt), # when did the last bup end?
         days_since_last_moud = as.integer(difftime(moud_start_dt, lag_moud_end_dt, units = "days")),
         lead_moud_start_dt = lead(moud_start_dt), # when is the next dose starting compared to last moud end date
         days_to_next_moud = as.integer(difftime(lead_moud_start_dt, moud_end_dt, units = "days")))  |>
  ungroup() |>
  # indicator of whether to use the moud_start_dt/moud_end_dt or ignore them
  mutate(use_start_dt = case_when(is.na(days_since_last_moud) ~ 1,
                                  days_since_last_moud > 0 ~ 1,
                                  TRUE ~ 0),
         
         use_end_dt = case_when(is.na(days_to_next_moud) ~ 1,
                                days_to_next_moud > 0 ~ 1,
                                TRUE ~ 0))

# keep only the needed rows for start dates
met_start_dts <-
  met_adj |>
  filter(use_start_dt == 1) |>
  select(BENE_ID, moud_start_dt) |>
  ungroup()

# keep only the needed rows for end dates
met_end_dts <-
  met_adj |>
  filter(use_end_dt == 1) |>
  select(BENE_ID, moud_end_dt) |>
  ungroup()

# merge to one final bup start/stop data set
all_met_start_stop <-
  met_start_dts |>
  bind_cols(met_end_dts |> select(-BENE_ID)) |>
  mutate(moud_med = "met") |>
  select(BENE_ID, moud_med, everything()) |>
  left_join(dts_cohorts |> select(BENE_ID, washout_start_dt)) |>
  filter(!(moud_end_dt < washout_start_dt)) |> # filter out rows that end before washout period begins
  select(-washout_start_dt)

saveRDS(all_met_start_stop, file.path(drv_root, "met_start_dt.rds"))

# Nal --------------------------------------------------------------------------

# extract Nal injection scripts
nal_scripts_rxl <- 
  rxl |>
  filter(NDC == "65757030001") |>
  select(BENE_ID,
         RX_FILL_DT) |>
  distinct() |>
  collect() |>
  mutate(moud_med = "nal",
         form = "injection",
         moud_start_dt = RX_FILL_DT,
         moud_end_dt = moud_start_dt + 21 + 30) |> # nal injections last 30 days
  select(BENE_ID, moud_med, form, moud_start_dt, moud_end_dt)

# nal injections
nal_scripts_otl <-
  otl |>
  filter(NDC == "65757030001") |>
  mutate(LINE_SRVC_BGN_DT = case_when(is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, TRUE ~ LINE_SRVC_BGN_DT)) |>
  select(BENE_ID,
         LINE_SRVC_BGN_DT) |>
  distinct() |>
  collect() |>
  mutate(moud_med = "nal",
         form = "injection",
         moud_start_dt = LINE_SRVC_BGN_DT,
         moud_end_dt = moud_start_dt + 21 + 30) |> # nal injections last 30 days
  select(BENE_ID, moud_med, form, moud_start_dt, moud_end_dt)


# pull HCPCS codes

nal_hcpcs <- c(
  "J2315",
  "G2073"
  # "H0033" NOT USING,  too vague
)

nal_hcpcs_otl <-
  otl |>
  filter(LINE_PRCDR_CD %in% nal_hcpcs) |>
  mutate(LINE_SRVC_BGN_DT = case_when(is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, TRUE ~ LINE_SRVC_BGN_DT),
  ) |>
  select(BENE_ID,
         STATE_CD, 
         LINE_SRVC_BGN_DT) |>
  collect() |>
  mutate(moud_med = "nal",
         form = "injection",
         moud_start_dt = LINE_SRVC_BGN_DT,
         moud_end_dt = moud_start_dt + 21 + 30) |> # nal injections last 30 days
  select(BENE_ID, moud_med, form, moud_start_dt, moud_end_dt)

# Combine all sources of nal moud
all_nal <- full_join(nal_scripts_otl, nal_scripts_rxl) |>
  full_join(nal_hcpcs_otl) |>
  distinct(BENE_ID, moud_start_dt, moud_end_dt) |>
  group_by(BENE_ID) |>
  arrange(BENE_ID, moud_start_dt)

# adjudicate the naltrexone information depending on when the beneficiary received the next dose
# check when the next/last injection was given, and how long that was since the last injection ended (injection + 28 days)

# a start date for Nal should be:
#   - injection date if it's their first time receiving Nal, or
#   - injection date if they've received Nal previously and more than 28 + 21 days has passed since the previous injection
# an end date for Nal should be:
#   - injection date + 28 + 21 days if it's their last time ever receiving Nal, or
#   - injection date + 28 +21 days if there's more than 28+21 days between the current injection date and the next injection date

all_nal_adj <-
  all_nal |>
  drop_na(BENE_ID) |>
  mutate(lag_moud_end_dt = lag(moud_end_dt), # when did the last nal end?
         days_since_last_moud = as.integer(difftime(moud_start_dt, lag_moud_end_dt, units = "days")),
         lead_moud_start_dt = lead(moud_start_dt), # when is the next dose starting compared to last moud end date
         days_to_next_moud = as.integer(difftime(lead_moud_start_dt, moud_end_dt, units = "days"))) |>
  ungroup() |>
  # indicator of whether to use the moud_start_dt/moud_end_dt or ignore them
  mutate(use_start_dt = case_when(is.na(days_since_last_moud) ~ 1,
                                  days_since_last_moud > 0 ~ 1,
                                  TRUE ~ 0),
         
         use_end_dt = case_when(is.na(days_to_next_moud) ~ 1,
                                days_to_next_moud > 0 ~ 1,
                                TRUE ~ 0))

# keep only the needed rows for start dates
all_nal_start_dts <-
  all_nal_adj |>
  filter(use_start_dt == 1) |>
  select(BENE_ID, moud_start_dt) |>
  ungroup()

# keep only the needed rows for end dates (same N and order as all_nal_start_dts)
all_nal_end_dts <-
  all_nal_adj |>
  filter(use_end_dt == 1) |>
  select(BENE_ID, moud_end_dt) |>
  ungroup()

# merge to one final nal start/stop data set
all_nal_start_stop <-
  all_nal_start_dts |>
  bind_cols(all_nal_end_dts |> select(-BENE_ID)) |>
  mutate(moud_med = "nal") |>
  select(BENE_ID, moud_med, everything()) |>
  left_join(dts_cohorts |> select(BENE_ID, washout_start_dt)) |>
  filter(!(moud_end_dt < washout_start_dt)) |> # filter out rows that end before washout period begins
  select(-washout_start_dt)

saveRDS(all_nal_start_stop, file.path(drv_root, "nal_start_dt.rds"))
