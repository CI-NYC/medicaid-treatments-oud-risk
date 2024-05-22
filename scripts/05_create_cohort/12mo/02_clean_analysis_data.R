library(data.table)
library(lubridate)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

mediation_analysis_df <- readRDS(file.path(drv_root, "mediation_12mo_analysis_df.rds"))

# List of variables to keep
vars_to_keep <- c(
    # BENE_ID
    "BENE_ID",
    # Outcome
    "oud_24mo",
    "oud_24mo_icd",
    "oud_overdose_24mo",
    # Exposure
    "disability_pain_12mos_cal",
    # Baseline confounders
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
    "baseline_has_counseling",
    # Post exposure confounders
    "anxiety_post_exposure_cal",
    "depression_post_exposure_cal",
    "bipolar_post_exposure_cal",
    "mediator_has_counseling",
    # Mediators
    "mediator_max_daily_dose_mme",
    "mediator_opioid_days_covered",
    "mediator_prescribers_6mo_sum",
    "mediator_has_tapering",
    "mediator_opioid_benzo_copresc",
    "mediator_opioid_stimulant_copresc",
    "mediator_opioid_mrelax_copresc",
    "mediator_opioid_gaba_copresc",
    "mediator_nonopioid_pain_rx",
    "mediator_has_physical_therapy",
    "mediator_has_multimodal_pain_treatment_restrict",
    # Censoring
    "uncens_24mo"
)

clean <- copy(mediation_analysis_df)
clean <- clean[, .SD, .SDcols = vars_to_keep]

# Impute NA values -------------------------------------------------------------

# Check for NAs in all variables before addressing
na_check <- sapply(clean, \(x) any(is.na(x)))

# Create indicator variables for missing dem variables
clean[, `:=`(missing_dem_race = fifelse(is.na(dem_race), 1, 0), 
             missing_dem_primary_language_english = fifelse(is.na(dem_primary_language_english), 1, 0),
             missing_dem_married_or_partnered = fifelse(is.na(dem_married_or_partnered), 1, 0),
             missing_dem_household_size = fifelse(is.na(dem_household_size), 1, 0),
             missing_dem_veteran = fifelse(is.na(dem_veteran), 1, 0),
             missing_dem_tanf_benefits = fifelse(is.na(dem_tanf_benefits), 1, 0),
             missing_dem_ssi_benefits = fifelse(is.na(dem_ssi_benefits), 1, 0))]

Mode <- function(x, na.rm = TRUE) {
    if(na.rm){
        x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
}

# Set NA values to the mode
clean[, `:=`(
    dem_race = fifelse(is.na(dem_race), 
                       Mode(dem_race), dem_race),
    dem_primary_language_english = fifelse(is.na(dem_primary_language_english), 
                                           Mode(dem_primary_language_english), 
                                           dem_primary_language_english),
    dem_married_or_partnered = fifelse(is.na(dem_married_or_partnered), 
                                       Mode(dem_married_or_partnered), 
                                       dem_married_or_partnered),
    dem_household_size = fifelse(is.na(dem_household_size), 
                                 Mode(dem_household_size), 
                                 dem_household_size),
    dem_veteran = fifelse(is.na(dem_veteran), 
                          Mode(dem_veteran), dem_veteran),
    dem_tanf_benefits = fifelse(is.na(dem_tanf_benefits), 
                                Mode(dem_tanf_benefits), dem_tanf_benefits),
    dem_ssi_benefits = fifelse(is.na(dem_ssi_benefits), 
                               Mode(dem_ssi_benefits), dem_ssi_benefits)
)]

# Check for NAs in all variables after addressing
na_check <- sapply(clean, \(x) any(is.na(x)))
any(na_check[setdiff(names(na_check), c("oud_24mo", "oud_24mo_icd", "oud_overdose_24mo"))])

# Dummy coding ------------------------------------------------------------

# Convert all non-numeric variables to numeric using dummy variable coding
non_numeric_cols <- sapply(clean, \(x) !is.numeric(x))
non_numeric_cols <- names(which(non_numeric_cols))

# Convert categorical variables to character type
clean[, (non_numeric_cols) := lapply(.SD, as.character), .SDcols = non_numeric_cols]

clean[, `:=`(dem_sex_m = fifelse(dem_sex == "M", 1, 0), # Sex (reference category set to female)
             # Race (reference category set to white, non-Hispanic)
             dem_race_aian = fifelse(
                 dem_race == "American Indian and Alaska Native (AIAN), non-Hispanic", 
                 1, 0), 
             dem_race_asian = fifelse(dem_race == "Asian, non-Hispanic", 1, 0), 
             dem_race_black = fifelse(dem_race == "Black, non-Hispanic", 1, 0),
             dem_race_hawaiian = fifelse(dem_race == "Hawaiian/Pacific Islander", 1, 0),
             dem_race_hispanic = fifelse(dem_race == "Hispanic, all races", 1, 0),
             dem_race_multiracial = fifelse(dem_race == "Multiracial, non-Hispanic", 1, 0),
             # Household size (reference category set to 1)
             dem_household_size_2 = fifelse(dem_household_size == "2", 1, 0),
             dem_household_size_2plus = fifelse(dem_household_size == "2+", 1, 0),
             # SSI benefits (reference category set to not applicable)
             dem_ssi_benefits_mandatory_optional = fifelse(
                 dem_ssi_benefits == "Mandatory or optional", 1, 0))]


# Save in the mediation folder
saveRDS(clean, file.path(drv_root, "mediation_12mo_analysis_df_clean.rds"))

# Subset and create dummy exposure variables -----------------------------------

# Disability and chronic pain
disability_pain_ref <- 
    clean[disability_pain_12mos_cal == "disability and chronic pain" | 
            disability_pain_12mos_cal == "neither", 
          ][, exposure_disability_pain := fifelse(
            disability_pain_12mos_cal == "disability and chronic pain", 1, 0)
            ][]

saveRDS(disability_pain_ref, file.path(drv_root, "subset_12mo_disability_pain_ref_df.rds"))

# Disability only
disability_only_ref <- clean[disability_pain_12mos_cal == "disability only" | 
                                 disability_pain_12mos_cal == "neither", 
                             ][, exposure_disability_only := fifelse(
                                 disability_pain_12mos_cal == "disability only", 1, 0)
                               ][]

saveRDS(disability_only_ref, file.path(drv_root, "subset_12mo_disability_only_ref_df.rds"))

# Chronic pain only
pain_only_ref <- clean[disability_pain_12mos_cal == "chronic pain only" | 
                           disability_pain_12mos_cal == "neither", 
                       ][, exposure_pain_only := fifelse(
                           disability_pain_12mos_cal == "chronic pain only", 1, 0)
                         ][]

saveRDS(pain_only_ref, file.path(drv_root, "subset_12mo_pain_only_ref_df.rds"))
