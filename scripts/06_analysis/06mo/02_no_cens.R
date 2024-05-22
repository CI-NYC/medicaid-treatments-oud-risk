# -------------------------------------
# Script: 02_no_cens.R
# Author: Nick Williams
# Purpose: Estimate baseline estimates under 'no-censoring' interventions 
# Notes:
# -------------------------------------

library(data.table)
library(lmtp)
library(mlr3extralearners)
library(glue)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
data <- readRDS(file.path(drv_root, "mediation_analysis_df_clean.rds"))
args <- commandArgs(TRUE)

dcp <- data[disability_pain_cal == "disability and chronic pain"]
setDF(dcp)

cp <- data[disability_pain_cal == "chronic pain only"]
setDF(cp)

W <- c(
    "dem_age",
    "dem_sex_m",
    "dem_race_aian",
    "dem_race_asian",
    "dem_race_black",
    "dem_race_hawaiian",
    "dem_race_hispanic",
    "dem_race_multiracial",
    "dem_primary_language_english", 
    "dem_married_or_partnered",
    "dem_household_size_2",
    "dem_household_size_2plus",
    "dem_veteran", 
    "dem_probable_high_income",
    "dem_tanf_benefits", 
    "dem_ssi_benefits_mandatory_optional",
    "bipolar_washout_cal",
    "anxiety_washout_cal",
    "adhd_washout_cal",
    "depression_washout_cal",
    "mental_ill_washout_cal",
    "baseline_has_counseling",
    "missing_dem_race",
    "missing_dem_primary_language_english",
    "missing_dem_married_or_partnered",
    "missing_dem_household_size",
    "missing_dem_veteran",
    "missing_dem_tanf_benefits",
    "missing_dem_ssi_benefits"
)

Z <- c("anxiety_post_exposure_cal",
       "depression_post_exposure_cal", 
       "mediator_has_counseling")

M <- list(c("mediator_max_daily_dose_mme",
            "mediator_opioid_days_covered",
            "mediator_prescribers_6mo_sum",
            "mediator_has_tapering",
            "mediator_opioid_benzo_copresc",
            "mediator_opioid_stimulant_copresc",
            "mediator_opioid_mrelax_copresc",
            "mediator_opioid_gaba_copresc",
            "mediator_nonopioid_pain_rx",
            "mediator_has_physical_therapy",
            "mediator_has_multimodal_pain_treatment_restrict"))

Y <- "oud_24mo"
mo <- as.numeric(unlist(stringr::str_extract_all(Y, "\\d+")))
cens <- glue::glue("uncens_{mo}mo")

fit_dcp <- lmtp_tmle(
    dcp, M, Y, c(W, Z), 
    cens = cens,
    shift = NULL,
    outcome_type = "binomial", 
    folds = 5,
    learners_outcome = c("glm", "xgboost", "ranger", "nnet", "mean", "earth"), 
    learners_trt = c("glm", "xgboost", "ranger", "nnet", "mean", "earth")
)

saveRDS(fit_dcp, file.path(drv_root, "mtp_fits", glue::glue("fit_dcp_{Y}_outcome_fix_no_cens.rds")))

fit_cp <- lmtp_tmle(
    cp, M, Y, c(W, Z), 
    cens = cens,
    shift = NULL,
    outcome_type = "binomial", 
    folds = 2,
    learners_outcome = c("glm", "xgboost", "ranger", "nnet", "mean", "earth"), 
    learners_trt = c("glm", "xgboost", "ranger", "nnet", "mean", "earth")
)

saveRDS(fit_cp, file.path(drv_root, "mtp_fits", glue::glue("fit_cp_{Y}_outcome_fix_no_cens.rds")))
