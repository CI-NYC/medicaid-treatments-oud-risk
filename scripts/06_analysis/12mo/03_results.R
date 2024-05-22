# -------------------------------------
# Script: 02_results.R
# Author: Nick Williams
# Purpose: Load and combine results from runs of 01_run_main.R
# Notes:
# -------------------------------------

library(lmtp)
library(glue)
library(purrr)
library(dplyr)
library(ggplot2)
library(data.table)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
data <- readRDS(file.path(drv_root, "mediation_12mo_analysis_df_clean.rds"))

Y <- "oud_24mo"

M <- c("mediator_max_daily_dose_mme",
       "mediator_opioid_days_covered",
       "mediator_prescribers_6mo_sum",
       # "mediator_has_tapering",
       "mediator_opioid_benzo_copresc",
       # "mediator_opioid_stimulant_copresc",
       "mediator_opioid_mrelax_copresc",
       "mediator_opioid_gaba_copresc",
       "mediator_nonopioid_pain_rx",
       # "mediator_has_counseling",
       "mediator_has_physical_therapy",
       "mediator_has_multimodal_pain_treatment_restrict")

read_res <- function(Y, subset) {
    map_dfr(M, function(mediator) {
        file.path(drv_root, 
                  "mtp_fits", 
                  glue("fit_12mo_{gsub(' ', '_', subset)}_{Y}_mediator_{mediator}.rds")) |> 
            readRDS() |> 
            tidy() |> 
            mutate(mediator = mediator, .before = "estimator")
    })
}

read_diff <- function(Y, subset1, subset2) {
    map_dfr(M, function(mediator) {
        diff <- file.path(drv_root, 
                          "mtp_fits", 
                          glue("fit_12mo_{gsub(' ', '_', subset1)}_{Y}_mediator_{mediator}.rds")) |> 
            readRDS() |> 
            lmtp_contrast(ref = readRDS(file.path(drv_root, "mtp_fits", glue("fit_12mo_{subset2}_{Y}_no_cens.rds"))))
        mutate(diff$vals, mediator = mediator, .before = "theta") |> 
            mutate(estimate = shift - ref)
    })
}

read_relr <- function(Y, subset1, subset2) {
    map_dfr(M, function(mediator) {
        diff <- file.path(drv_root, 
                          "mtp_fits", 
                          glue("fit_12mo_{gsub(' ', '_', subset1)}_{Y}_mediator_{mediator}.rds")) |> 
            readRDS() |> 
            lmtp_contrast(ref = readRDS(file.path(drv_root, "mtp_fits", glue("fit_12mo_{subset2}_{Y}_no_cens.rds"))), 
                          type = "rr")
        mutate(diff$vals, mediator = mediator, .before = "theta") |> 
            mutate(theta = theta - 1, 
                   conf.low = conf.low - 1, 
                   conf.high = conf.high - 1)
    })
}

# Results for disability and chronic pain
res_dcp <- file.path(drv_root, "mtp_fits", glue("fit_12mo_dcp_{Y}_no_cens.rds")) |> 
    readRDS() |> 
    tidy() |> 
    mutate(mediator = "No censoring", .before = "estimator") |> 
    bind_rows(read_res(Y, "disability and chronic pain"))

# Results for chronic pain only
res_cp <- file.path(drv_root, "mtp_fits", glue("fit_12mo_cp_{Y}_no_cens.rds")) |> 
    readRDS() |> 
    tidy() |> 
    mutate(mediator = "No censoring", .before = "estimator") |> 
    bind_rows(read_res(Y, "chronic pain only"))

# Plot results ------------------------------------------------------------

theme_set(theme_minimal(base_family = "Spline Sans", 
                        base_size = 3,
                        base_line_size = 0.2,
                        base_rect_size = 0.2))
theme_update(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = .15),
    axis.ticks.x = element_line(color = "black", linewidth = .15),
    axis.title.y = element_blank(),
    plot.margin = margin(10, 15, 10, 15),
    text = element_text(color = "black", 
                        size = 3)
)

label_counts <- function(data, subset, m) {
    subsetted <- data[disability_pain_12mos_cal == subset]
    has_mediator <- subsetted[subsetted[[m]] == 1]
    vals <- table(has_mediator[[Y]])
    glue("Outcome count: {vals['1']}")
}

cl_dcp <-setNames(map_chr(M[3:length(M)], \(m) label_counts(data, "disability and chronic pain", m)),
                  M[3:length(M)])
cl_cp <- setNames(map_chr(M[3:length(M)], \(m) label_counts(data, "chronic pain only", m)), 
                  M[3:length(M)])

relabel <- function(data) {
    mutate(data, 
           mediator = case_when(
               mediator == "mediator_opioid_stimulant_copresc" ~ glue("Opioid, stimulant copresc."), 
               mediator == "mediator_has_physical_therapy" ~ glue("Physical therapy"), 
               mediator == "mediator_opioid_mrelax_copresc" ~ glue("Opioid, muscle-relax. copresc."), 
               mediator == "mediator_prescribers_6mo_sum" ~ "Six-month total prescribers", 
               mediator == "mediator_nonopioid_pain_rx" ~ glue("Non-opioid pain Rx"), 
               mediator == "mediator_has_multimodal_pain_treatment_restrict" ~ glue("Other pain treatment modality"), 
               mediator == "mediator_max_daily_dose_mme" ~ "Max daily MME", 
               mediator == "mediator_has_counseling" ~ glue("Counseling"), 
               mediator == "mediator_opioid_gaba_copresc" ~ glue("Opioid, gabapentin copresc."), 
               mediator == "mediator_opioid_benzo_copresc" ~ glue("Opioid, benzodiazepine copresc."), 
               mediator == "mediator_opioid_days_covered" ~ "Proportion of opioid days", 
               TRUE ~ mediator
           ))
}

plot_diff <- function(data) {
    ggplot(data, aes(theta, mediator)) +
        geom_col(aes(fill = theta > 0), 
                 width = 0.3) +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                       height = 0.2, 
                       position = position_nudge(y = 0.4), 
                       size = 0.15) + 
        geom_vline(xintercept = 0, 
                   linetype = "dashed", 
                   size = 0.15, 
                   color = "grey50") + 
        scale_y_discrete() +
        scale_x_continuous(name = "Risk difference") + 
        geom_text(
            aes(label = paste0("  ", sprintf("%.4f", theta), "  "), 
                hjust = ifelse(theta < 0, 1, 0)),
            size = 0.75, family = "Spline Sans"
        ) +
        scale_color_manual(values = c("black"), guide = "none") + 
        scale_fill_manual(values = c("#1D785A", "red3"), guide = "none") +
        theme(axis.text.y = element_text(
            hjust = 0, margin = margin(1, 0, 1, 0), 
            size = rel(1.1), 
            color = "black"
        ))
}

plot_relr <- function(data) {
    ggplot(data, aes(theta, mediator)) +
        geom_col(aes(fill = theta > 0),
                 width = 0.3) +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                       height = 0.2,
                       position = position_nudge(y = 0.4), 
                       size = 0.1) +
        geom_vline(xintercept = 0, 
                   linetype = "dashed", 
                   size = 0.15, 
                   color = "grey50") + 
        scale_y_discrete() +
        scale_x_continuous(name = "Relative risk", 
                           labels = scales::label_percent()) +
        geom_text(
            aes(label = paste0("  ", sprintf("%2.1f", theta * 100), "%  "), 
                hjust = ifelse(theta < 0, 1, 0)),
            size = 0.75, 
            family = "Spline Sans"
        ) +
        scale_color_manual(values = c("black"), guide = "none") + 
        scale_fill_manual(values = c("#1D785A", "red3"), guide = "none") +
        theme(axis.text.y = element_text(
            hjust = 0, margin = margin(1, 0, 1, 0),
            size = rel(1.1),
            color = "black"
        ))
}

plot_res <- function(data, limits) {
    ggplot(data, aes(x = reorder(mediator, estimate), y = estimate)) + 
        geom_point() + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                      width = 0.2) + 
        scale_y_continuous(name = "Incidence") + 
        coord_flip(ylim = limits) + 
        theme(axis.text.y = element_text(
            hjust = 0, margin = margin(1, 0, 1, 0), 
            size = rel(1.1), 
            # face = "bold", 
            color = "black"
        ))
}

extract_count <- function(x) {
    map_dbl(x, function(i) {
        if (!stringr::str_detect(i, "\\d+")) return(11)
        as.numeric(unlist(stringr::str_extract_all(i, "\\d+")))
    })
}

ragg::agg_png(
    glue("figures/mtp_12mo_{Y}_disability_chronic_pain_riskdiff.png"), 
    width = 7, height = 3, units = "cm", res = 600
)

read_diff(Y, "disability and chronic pain", "dcp") |> 
    relabel() |> 
    filter(extract_count(c(rep("foo", 2), cl_dcp)) > 10) |> 
    mutate(mediator = forcats::fct_reorder(mediator, theta, .desc = FALSE)) |> 
    plot_diff()

dev.off()

ragg::agg_png(
    glue("figures/mtp_12mo_{Y}_disability_chronic_pain_relrisk.png"), 
    width = 7, height = 3, units = "cm", res = 600
)

read_relr(Y, "disability and chronic pain", "dcp") |> 
    relabel() |> 
    filter(extract_count(c(rep("foo", 2), cl_dcp)) > 10) |> 
    mutate(mediator = forcats::fct_reorder(mediator, theta, .desc = FALSE)) |> 
    plot_relr()

dev.off()

ragg::agg_png(
    glue("figures/mtp_12mo_{Y}_chronic_pain_only_riskdiff.png"), 
    width = 7, height = 3, units = "cm", res = 600
)

read_diff(Y, "chronic pain only", "cp") |> 
    relabel() |> 
    filter(extract_count(c(rep("foo", 2), cl_cp)) > 10) |> 
    mutate(mediator = forcats::fct_reorder(mediator, theta, .desc = FALSE)) |> 
    plot_diff()

dev.off()

ragg::agg_png(
    glue("figures/mtp_12mo_{Y}_chronic_pain_only_relrisk.png"), 
    width = 7, height = 3, units = "cm", res = 600
)

read_relr(Y, "chronic pain only", "cp") |> 
    relabel() |> 
    filter(extract_count(c(rep("foo", 2), cl_cp)) > 10) |> 
    mutate(mediator = forcats::fct_reorder(mediator, theta, .desc = FALSE)) |> 
    plot_relr()

dev.off()
