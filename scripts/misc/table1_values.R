# -------------------------------------
# Script: table1_values.R
# Author: Nick Williams
# Purpose: Misc table 1 values
# Notes:
# -------------------------------------

library(data.table)
library(skimr)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
data <- readRDS(file.path(drv_root, "mediation_analysis_df.rds"))
opioids <- readRDS(file.path(drv_root, "opioid_names_wide.rds"))
nop <- readRDS(file.path(drv_root, "mediator_nop_binary_refactor.rds"))

my_skim <- skim_with(
    numeric = sfl(mean = NULL), 
    factor = sfl(pct = ~ {
        prt <- prop.table(table(.))
        val <- sprintf("%.3f", prt)
        nm1 <- tolower(substr(names(prt), 1, 3))
        stringr::str_c(nm1, val, sep = ": ", collapse = ", ")
    })
)

W <- c(
    "dem_age",
    "dem_sex",
    "dem_race",
    "dem_primary_language_english", # NAs
    "dem_married_or_partnered", # NAs
    "dem_household_size",
    "dem_veteran", # NAs
    "dem_probable_high_income",
    "dem_tanf_benefits", # NAs
    "dem_ssi_benefits", # character
    "bipolar_washout_cal",
    "anxiety_washout_cal",
    "adhd_washout_cal",
    "depression_washout_cal",
    "mental_ill_washout_cal",
    "baseline_has_counseling"
)

# Demographics, Physical disability and chronic pain
data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c(W, "disability_pain_cal"))) |> 
    dplyr::mutate(dplyr::across(dem_sex:baseline_has_counseling, as.factor)) |> 
    # dplyr::group_by(disability_pain_cal) |> 
    dplyr::filter(disability_pain_cal == "disability and chronic pain") |> 
    my_skim()

# Demographics, Chronic pain only
data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c(W, "disability_pain_cal"))) |> 
    dplyr::mutate(dplyr::across(dem_sex:baseline_has_counseling, as.factor)) |> 
    # dplyr::group_by(disability_pain_cal) |> 
    dplyr::filter(disability_pain_cal == "chronic pain only") |> 
    my_skim()

# Psych conditions, months 1-6
data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                  "bipolar_washout_cal",
                                  "anxiety_washout_cal",
                                  "adhd_washout_cal",
                                  "depression_washout_cal",
                                  "mental_ill_washout_cal"))) |> 
    dplyr::mutate(dplyr::across(bipolar_washout_cal:mental_ill_washout_cal, as.factor)) |> 
    dplyr::group_by(disability_pain_cal) |> 
    my_skim()

# Psych conditions, months 7-12
data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                  "anxiety_post_exposure_cal",
                                  "depression_post_exposure_cal"))) |> 
    dplyr::mutate(dplyr::across(anxiety_post_exposure_cal:depression_post_exposure_cal, as.factor)) |> 
    dplyr::group_by(disability_pain_cal) |> 
    my_skim()

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                  "mediator_max_daily_dose_mme",
                                  "mediator_opioid_days_covered",
                                  "mediator_prescribers_6mo_sum",
                                  "mediator_has_tapering",
                                  "mediator_opioid_benzo_copresc",
                                  "mediator_opioid_stimulant_copresc",
                                  "mediator_opioid_mrelax_copresc",
                                  "mediator_opioid_gaba_copresc"))) |> 
    dplyr::mutate(dplyr::across(any_of(c("mediator_opioid_pain_rx", 
                                         "mediator_months_opioid_rx", 
                                         "mediator_opioid_benzo_copresc", 
                                         "mediator_opioid_stimulant_copresc", 
                                         "mediator_prop_opioid_stimulant_copresc", 
                                         "mediator_opioid_gaba_copresc", 
                                         "mediator_opioid_mrelax_copresc", 
                                         "mediator_has_tapering")), as.factor)) |> 
    # dplyr::group_by(disability_pain_cal) |> 
    dplyr::filter(disability_pain_cal == "disability and chronic pain") |> 
    my_skim()

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                  "mediator_opioid_pain_rx",
                                  "mediator_max_daily_dose_mme",
                                  "mediator_opioid_days_covered",
                                  "mediator_prescribers_6mo_sum",
                                  "mediator_has_tapering",
                                  "mediator_opioid_benzo_copresc",
                                  "mediator_opioid_stimulant_copresc",
                                  "mediator_opioid_mrelax_copresc",
                                  "mediator_opioid_gaba_copresc"))) |> 
    dplyr::mutate(dplyr::across(any_of(c("mediator_opioid_pain_rx", 
                                         "mediator_months_opioid_rx", 
                                         "mediator_opioid_benzo_copresc", 
                                         "mediator_opioid_stimulant_copresc", 
                                         "mediator_prop_opioid_stimulant_copresc", 
                                         "mediator_opioid_gaba_copresc", 
                                         "mediator_opioid_mrelax_copresc", 
                                         "mediator_has_tapering")), as.factor)) |> 
    # dplyr::group_by(disability_pain_cal) |> 
    dplyr::filter(disability_pain_cal == "chronic pain only") |> 
    my_skim()

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
  dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                "mediator_opioid_pain_rx",
                                "mediator_max_daily_dose_mme",
                                "mediator_opioid_days_covered",
                                "mediator_prescribers_6mo_sum", 
                                "mediator_opioid_benzo_copresc",
                                "mediator_opioid_stimulant_copresc",
                                "mediator_opioid_mrelax_copresc",
                                "mediator_opioid_gaba_copresc"))) |> 
  dplyr::mutate(dplyr::across(any_of(c("mediator_opioid_benzo_copresc", 
                                       "mediator_opioid_stimulant_copresc", 
                                       "mediator_opioid_gaba_copresc", 
                                       "mediator_opioid_mrelax_copresc")), as.factor)) |> 
  dplyr::filter(mediator_opioid_pain_rx == 1) |> 
  dplyr::filter(disability_pain_cal == "chronic pain only") |> 
  my_skim()

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
  dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                "mediator_opioid_pain_rx",
                                "mediator_max_daily_dose_mme",
                                "mediator_opioid_days_covered",
                                "mediator_prescribers_6mo_sum", 
                                "mediator_opioid_benzo_copresc",
                                "mediator_opioid_stimulant_copresc",
                                "mediator_opioid_mrelax_copresc",
                                "mediator_opioid_gaba_copresc"))) |> 
  dplyr::mutate(dplyr::across(any_of(c("mediator_opioid_benzo_copresc", 
                                       "mediator_opioid_stimulant_copresc", 
                                       "mediator_opioid_gaba_copresc", 
                                       "mediator_opioid_mrelax_copresc")), as.factor)) |> 
  dplyr::filter(mediator_opioid_pain_rx == 1) |> 
  dplyr::filter(disability_pain_cal == "disability and chronic pain") |> 
  my_skim()

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
  dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                "mediator_opioid_pain_rx",
                                "mediator_max_daily_dose_mme",
                                "mediator_opioid_days_covered",
                                "mediator_prescribers_6mo_sum"))) |> 
  dplyr::filter(mediator_opioid_pain_rx == 1) |> 
  dplyr::filter(disability_pain_cal == "disability and chronic pain") |> 
  my_skim()

mediator_df[, mediator_has_multimodal_pain_treatment_restrict := fcase(
    mediator_has_ablative_techniques == 1, 1,
    mediator_has_acupuncture == 1, 1,
    mediator_has_blocks == 1, 1,
    mediator_has_botulinum_toxin == 1, 1,
    mediator_has_electrical_nerve_stimulation == 1, 1,
    mediator_has_epidural_steroid == 1, 1,
    mediator_has_intrathecal_drug_therapy == 1, 1,
    mediator_has_trigger_point_injection == 1, 1,
    mediator_has_massage_therapy == 1, 1,
    mediator_has_chiropractic == 1, 1, 
    default = 0
)]

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                  "mediator_has_multimodal_pain_treatment_restrict",
                                  "mediator_has_physical_therapy",
                                  "mediator_has_massage_therapy",
                                  "mediator_has_chiropractic",
                                  "mediator_has_ablative_techniques",
                                  "mediator_has_acupuncture",
                                  "mediator_has_blocks",
                                  "mediator_has_botulinum_toxin",
                                  "mediator_has_electrical_nerve_stimulation",
                                  "mediator_has_epidural_steroid",
                                  "mediator_has_intrathecal_drug_therapy",
                                  "mediator_has_trigger_point_injection",
                                  "mediator_has_counseling", 
                                  "baseline_has_counseling"))) |> 
    dplyr::mutate(dplyr::across(mediator_has_multimodal_pain_treatment_restrict:baseline_has_counseling, as.factor)) |> 
    # dplyr::group_by(disability_pain_cal) |> 
    dplyr::filter(disability_pain_cal == "disability and chronic pain") |> 
    my_skim()

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c("disability_pain_cal", 
                                  "mediator_has_multimodal_pain_treatment_restrict",
                                  "mediator_has_physical_therapy",
                                  "mediator_has_massage_therapy",
                                  "mediator_has_chiropractic",
                                  "mediator_has_ablative_techniques",
                                  "mediator_has_acupuncture",
                                  "mediator_has_blocks",
                                  "mediator_has_botulinum_toxin",
                                  "mediator_has_electrical_nerve_stimulation",
                                  "mediator_has_epidural_steroid",
                                  "mediator_has_intrathecal_drug_therapy",
                                  "mediator_has_trigger_point_injection",
                                  "mediator_has_counseling", 
                                  "baseline_has_counseling"))) |> 
    dplyr::mutate(dplyr::across(mediator_has_multimodal_pain_treatment_restrict:baseline_has_counseling, as.factor)) |> 
    # dplyr::group_by(disability_pain_cal) |> 
    dplyr::filter(disability_pain_cal == "chronic pain only") |> 
    my_skim()

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only")] |> 
    dplyr::select(dplyr::all_of(c("disability_pain_cal", "oud_24mo", "oud_24mo_icd"))) |> 
    dplyr::mutate(dplyr::across(oud_24mo:oud_24mo_icd, as.factor)) |> 
    dplyr::group_by(disability_pain_cal) |> 
    my_skim()

merge(
    data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only"), .(BENE_ID)], 
    opioids, 
    all.x = TRUE
) |> 
    dplyr::mutate(across(buprenorphine:tramadol, as.factor)) |> 
    dplyr::group_by(disability_pain_cal) |> 
    my_skim()

merge(
    data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only"), .(BENE_ID, disability_pain_cal)], 
    nop, 
    all.x = TRUE
) |> dplyr::mutate(any_nop = as.numeric(fifelse(mediator_nonopioid_antiinflam == 1 |
                                        mediator_nonopioid_topical == 1 |
                                        mediator_nonopioid_muscle_relax == 1 | 
                                        mediator_nonopioid_other_analgesic == 1 | 
                                        mediator_nonopioid_antidep == 1, 1, 0)), 
                  across(mediator_nonopioid_antiinflam:any_nop, as.factor)) |> 
    dplyr::group_by(disability_pain_cal) |> 
    my_skim()

data[disability_pain_cal %in% c("disability and chronic pain", "chronic pain only"), .(.N, mean(dem_age), sd(dem_age))]
