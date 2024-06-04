# -------------------------------------
# Script: 00_opioid_mme.R
# Author: Kat Hoffman
# Updated: Nick Williams
# Purpose: Calculate opioid MME
# Notes:
# -------------------------------------

library(tidyverse)
library(fuzzyjoin)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

opioids <- readRDS(file.path(drv_root, "opioids_with_strength.rds"))
mme_conversion <- read_csv("data/mme.csv")

ci_str_detect <- function(x, y) str_detect(x, regex(y, ignore_case = TRUE))

opioids_mme <- 
    filter(opioids, flag_opioid_analgesic == TRUE | flag_opioid_anesthetics == TRUE) |> 
    unnest(cols = "strength") |> 
    fuzzy_inner_join(mme_conversion, 
                     by = c("activeIngredientName" = "opioid"), 
                     match_fun = ci_str_detect)

# Listed as codeine and dihydrocodeine, but really just dihydrocodeine
remove_codeine <- c("42195084010", "57664041988", "66992084010", "55887045690")

opioids_mme <- filter(opioids_mme, !(NDC %in% remove_codeine & opioid == "codeine"))

# Should only be drugs used for MOUD
no_mme <- anti_join(opioids, opioids_mme, by = "NDC")
unnest(no_mme, cols = "strength") |> 
    select(activeIngredientName) |> 
    unique()

opioids_mme <- opioids_mme |> 
    mutate(
        conversion = case_when(
            str_detect(activeIngredientName, "buprenorphine") &
                str_detect(dose_form, "Film|Tablet") ~ 30,
            str_detect(activeIngredientName, "buprenorphine") &
                str_detect(dose_form, "Trans") ~ 12.6,
            str_detect(activeIngredientName, "buprenorphine") &
                str_detect(dose_form, "Implant|Cartridge|Syringe|Inject") ~ 75,
            str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
                str_detect(dose_form, "Lozenge|Tablet") ~ 130,
            str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
                str_detect(dose_form, "Spray") ~ 160,
            str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
                str_detect(dose_form, "Transdermal") ~ 720,
            str_detect(activeIngredientName, "fentanyl|remifentanil|alfentanil|sufentanil") & 
                str_detect(dose_form, "Syringe|Injection|Cartridge|Pack") ~ 100,
            TRUE ~ conversion
        )
    )

saveRDS(opioids_mme, file.path(drv_root, "opioids_mme.rds"))
