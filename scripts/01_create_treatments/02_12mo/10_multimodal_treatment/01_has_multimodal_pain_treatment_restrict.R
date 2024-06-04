# -------------------------------------
# Script:
# Author:
# Purpose: Create an indicator variable for multimodal/multidisciplinary pain treatment 
#   1 or more of: 
#     1) ablative techniques
#     2) acupuncture
#     3) blocks  
#     4) botulinum toxin injections
#     5) electrical nerve stimulation
#     6) epidural steroids
#     7) intrathecal drug therapies
#     8) trigger point injection
#     9) massage therapy 
#     10) chiropractic
# Notes:
# -------------------------------------

library(data.table)
library(purrr)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

mediator_files <- c(
  # Categories for multimodal/multidisciplinary pain treatment
  "mediator_12mo_has_massage_therapy.rds",
  "mediator_12mo_has_chiropractic.rds",
  "mediator_12mo_has_ablative_techniques.rds",
  "mediator_12mo_has_acupuncture.rds",
  "mediator_12mo_has_blocks.rds",
  "mediator_12mo_has_botulinum_toxin.rds",
  "mediator_12mo_has_electrical_nerve_stimulation.rds",
  "mediator_12mo_has_epidural_steroid.rds",
  "mediator_12mo_has_intrathecal_drug_therapy.rds",
  "mediator_12mo_has_minimally_invasive_spinal_procedure.rds",
  "mediator_12mo_has_trigger_point_injection.rds"
)

mediator_list <- lapply(mediator_files, function(file) {
  data <- readRDS(file.path(drv_root, file))
  setDT(data)
  setkey(data, BENE_ID)
  data[]
})

mediator_df <- reduce(mediator_list, merge, all.x = TRUE, all.y = TRUE)

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

saveRDS(mediator_df[, .(BENE_ID, mediator_has_multimodal_pain_treatment_restrict)], 
        file.path(drv_root, "mediator_12mo_has_multimodal_pain_treatment_restrict.rds"))
