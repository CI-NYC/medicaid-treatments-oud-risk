# -------------------------------------
# Script: 01_run_main.R
# Author: Nick Williams
# Purpose: Run 00_main.R using `callr`
# Notes:
# -------------------------------------

library(callr)
library(tibble)

script <- "scripts/06_analysis/12mo/00_main.R"

dcp_param <- tribble(~subset, ~mediator, ~func, 
                     "disability and chronic pain", 1,  "x*1.2",     # "mediator_max_daily_dose_mme"
                     "disability and chronic pain", 2,  "((x*1.2) > 1)*x + ((x*1.2) <= 1)*(x*1.2)",     # "mediator_opioid_days_covered"
                     "disability and chronic pain", 3,  "x*1.2",     # "mediator_prescribers_6mo_sum"
                     "disability and chronic pain", 4,  "(x*0) + 1", # "mediator_has_tapering"
                     "disability and chronic pain", 5,  "(x*0) + 1", # "mediator_opioid_benzo_copresc"
                     "disability and chronic pain", 6,  "(x*0) + 1", # "mediator_opioid_stimulant_copresc"
                     "disability and chronic pain", 7,  "(x*0) + 1", # "mediator_opioid_mrelax_copresc"
                     "disability and chronic pain", 8,  "(x*0) + 1", # "mediator_opioid_gaba_copresc"
                     "disability and chronic pain", 9,  "(x*0) + 1", # "mediator_nonopioid_pain_rx"
                     "disability and chronic pain", 10, "(x*0) + 1", # "mediator_has_physical_therapy"
                     "disability and chronic pain", 11, "(x*0) + 1") # "mediator_has_multimodal_pain_treatment_restrict"

cp_param <- dcp_param
cp_param$subset <- "chronic pain only"

y <- "oud_24mo_icd"

# Execute for chronic pain and disability ---------------------------------

# # Crossfit with 2-folds
# for (i in 1:nrow(dcp_param)) {
#   if (i == 4 | i == 6) next
#   
#   Rprocess <- rscript_process$new(
#     rscript_process_options(
#       script = script, 
#       cmdargs = c(dcp_param$subset[i], dcp_param$mediator[i], dcp_param$func[i], y, 2)
#     )
#   )
#   Rprocess$wait()
# }

# Execute for chronic pain only -------------------------------------------

# Crossfit with 2-folds
for (i in 1:nrow(dcp_param)) {
  if (i == 4 | i == 6) next
  
  Rprocess <- rscript_process$new(
    rscript_process_options(
      script = script, 
      cmdargs = c(cp_param$subset[i], cp_param$mediator[i], cp_param$func[i], y, 2)
    )
  )
  Rprocess$wait()
}
