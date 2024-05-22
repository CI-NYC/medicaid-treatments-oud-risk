# -------------------------------------
# Script: 00_main.R
# Author: Nick Williams
# Purpose: Estimate joint MTP effects for mediators on OUD at 18 months
# Notes: Run using `callr::rscript_process` with 01_run_main.R
# -------------------------------------

library(data.table)
library(lmtp)
library(mlr3extralearners)
library(glue)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
data <- readRDS(file.path(drv_root, "mediation_12mo_analysis_df_clean.rds"))
args <- commandArgs(TRUE)

# options: ["disability and chronic pain", "disability only", "chronic pain only"]
subset <- args[[1]]
# options: [1:12]
mediator <- as.numeric(args[[2]])
# options: see 01_run_main.R
body <- args[[3]]
eval(parse(text = paste('f <- function(x) ', body, sep='')))
# options: ["oud_24mo"]
Y <- args[[4]]
# options: [2, 5]
folds <- as.numeric(args[[5]])

use <- data[disability_pain_12mos_cal == subset]
setDF(use)

# Shift function function factory 
factory <- function(mediator, func) {
    fs <- lapply(1:11, function(x) function(x) x)
    fs[[mediator]] <- func
    
    function(data, m) {
        out <- list(
            fs[[1]](data[[m[1]]]),  # "mediator_max_daily_dose_mme"
            fs[[2]](data[[m[2]]]),  # "mediator_opioid_days_covered"
            fs[[3]](data[[m[3]]]),  # "mediator_prescribers_6mo_sum"
            fs[[4]](data[[m[4]]]),  # "mediator_has_tapering"
            fs[[5]](data[[m[5]]]),  # "mediator_opioid_benzo_copresc"
            fs[[6]](data[[m[6]]]),  # "mediator_opioid_stimulant_copresc"
            fs[[7]](data[[m[7]]]),  # "mediator_opioid_mrelax_copresc"
            fs[[8]](data[[m[8]]]),  # "mediator_opioid_gaba_copresc"
            fs[[9]](data[[m[9]]]),  # "mediator_nonopioid_pain_rx"
            fs[[10]](data[[m[10]]]),  # "mediator_has_physical_therapy"
            fs[[11]](data[[m[11]]])   # "mediator_has_multimodal_pain_treatment_restrict"
        )
        setNames(out, m)
    }
}

d <- factory(mediator, f)

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

mo <- as.numeric(unlist(stringr::str_extract_all(Y, "\\d+")))
cens <- glue::glue("uncens_{mo}mo")

fit <- lmtp_tmle(use, M, Y, c(W, Z), 
                 cens = cens,
                 shift = d,
                 outcome_type = "binomial", 
                 folds = folds,
                 learners_outcome = c("glm", "xgboost", "ranger", "nnet", "mean", "earth"), 
                 learners_trt = c("glm", "xgboost", "ranger", "nnet", "mean", "earth"),
                 mtp = TRUE)

saveRDS(fit, file.path(drv_root, "mtp_fits", 
                       glue("fit_12mo_{gsub(' ', '_', subset)}_{Y}_mediator_{M[[1]][mediator]}.rds")))
