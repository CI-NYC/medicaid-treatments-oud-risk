# -------------------------------------
# Script: 
# Author: 
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(data.table)
library(purrr)
library(dplyr)
library(tidyr)
library(doFuture)
library(lubridate)

drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"

# Analysis cohort
cohort <- readRDS(file.path(drv_root, "cohort_ids.rds"))[, .(BENE_ID)]

# RXL datasets
opioid <- readRDS(file.path(drv_root, "mediator_12mo_rxl_opioid_pain_rx.rds"))
benzo <- readRDS(file.path(drv_root, "mediator_12mo_rxl_benzo_rx.rds"))
stimulant <- readRDS(file.path(drv_root, "mediator_12mo_rxl_stimulant_rx.rds"))
gaba <- readRDS(file.path(drv_root, "mediator_12mo_rxl_gabapentinoid_rx.rds"))
mrelax <- readRDS(file.path(drv_root, "mediator_12mo_rxl_muscle_relaxant_rx.rds"))

prop_days_covered <- function(data) {
    dur <- 0
    current_int <- data$rx_int[1]
    for (i in 1:nrow(data)) {
        check <- intersect(current_int, data$rx_int[i + 1])
        if (is.na(check)) {
            # if they don't intersect, add the duration of the first interval
            dur <- dur + as.duration(current_int)
            current_int <- data$rx_int[i + 1]
        } else {
            # if they do intersect, then update current interval as the union
            current_int <- union(current_int, data$rx_int[i + 1])
        }
    }
    
    min(max(time_length(dur, "days") / 182, 1 / 182), 1)
}

int_overlaps_numeric <- function (int1, int2) {
    stopifnot(c(is.interval(int1), is.interval(int2)))
    
    x <- intersect(int1, int2)@.Data
    x[is.na(x)] <- 0
    time_length(as.duration(x), "days")
}

which_min <- function(...) {
    l <- list(...)
    which.min(Reduce(cbind, l))
}

# Create datasets---------------------------------------------------------------

id <- "BENE_ID"
vars <- c("RX_FILL_DT", "DAYS_SUPPLY")
keep <- c(id, vars)
throwaway <- setdiff(names(opioid), keep)

opioid[, opioids_end_date := RX_FILL_DT + DAYS_SUPPLY
       ][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
         ][, opioids_end_date := 
               fifelse(is.na(opioids_end_date), RX_FILL_DT + days(1), opioids_end_date)]
setnames(opioid, vars, c("opioids_fill_date", "opioids_days_supply"))

throwaway <- setdiff(names(benzo), keep)

benzo[, benzo_end_date := RX_FILL_DT + DAYS_SUPPLY
      ][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
        ][, benzo_end_date := 
            fifelse(is.na(benzo_end_date), RX_FILL_DT + days(1), benzo_end_date)]
setnames(benzo, vars, c("benzo_fill_date", "benzo_days_supply"))

throwaway <- setdiff(names(stimulant), keep)

stimulant[, stimulant_end_date := RX_FILL_DT + DAYS_SUPPLY
          ][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
            ][, stimulant_end_date := 
                  fifelse(is.na(stimulant_end_date), RX_FILL_DT + days(1), stimulant_end_date)]
setnames(stimulant, vars, c("stimulant_fill_date", "stimulant_days_supply"))

throwaway <- setdiff(names(gaba), keep)

gaba[, gaba_end_date := RX_FILL_DT + DAYS_SUPPLY
     ][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
       ][, gaba_end_date := 
             fifelse(is.na(gaba_end_date), RX_FILL_DT + days(1), gaba_end_date)]
setnames(gaba, vars, c("gaba_fill_date", "gaba_days_supply"))

throwaway <- setdiff(names(mrelax), keep)

mrelax[, mrelax_end_date := RX_FILL_DT + DAYS_SUPPLY
       ][, (throwaway) := lapply(.SD, \(x) NULL), .SDcols = throwaway
         ][, mrelax_end_date := 
               fifelse(is.na(mrelax_end_date), RX_FILL_DT + days(1), mrelax_end_date)]
setnames(mrelax, vars, c("mrelax_fill_date", "mrelax_days_supply"))

# Create co-prescription variables ----------------------------------------

plan(multisession, workers = 50)

## Opioids and benzos
opioids_benzos <- 
    merge(opioid, benzo, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(benzos_interval = interval(benzo_fill_date, benzo_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(benzos_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "benzo")[which_min(opioids_fill_date, benzo_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "benzo" & 
                benzo_days_supply > 5 & 
                overlap / benzo_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(benzos_interval, opioids_interval)) |> 
    group_by(BENE_ID) |> 
    nest()

mediator_prop_opioid_benzo_copresc <- 
    foreach(data = opioids_benzos$data, 
            .combine = "c",
            .options.future = list(chunk.size = 500)) %dofuture% {
                arrange(data, int_start(rx_int)) |> 
                    prop_days_covered()
            }

opioids_benzos <- 
    ungroup(opioids_benzos) |> 
    mutate(mediator_opioid_benzo_copresc = 1, 
           mediator_prop_opioid_benzo_copresc = mediator_prop_opioid_benzo_copresc) |> 
    select(-data)

## Opioids and stimulants
opioids_stimulants <- 
    merge(opioid, stimulant, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(stim_interval = interval(stimulant_fill_date, stimulant_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(stim_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "stim")[which_min(opioids_fill_date, stimulant_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "stim" & 
                stimulant_days_supply > 5 & 
                overlap / stimulant_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(stim_interval, opioids_interval)) |> 
    group_by(BENE_ID) |> 
    nest()

mediator_prop_opioid_stimulant_copresc <- 
    foreach(data = opioids_stimulants$data, 
            .combine = "c",
            .options.future = list(chunk.size = 500)) %dofuture% {
                arrange(data, int_start(rx_int)) |> 
                    prop_days_covered()
            }

opioids_stimulants <- 
    ungroup(opioids_stimulants) |> 
    mutate(mediator_opioid_stimulant_copresc = 1, 
           mediator_prop_opioid_stimulant_copresc = mediator_prop_opioid_stimulant_copresc) |> 
    select(-data)

## Opioids and gabapentinoids
opioids_gabas <- 
    merge(opioid, gaba, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(gaba_interval = interval(gaba_fill_date, gaba_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(gaba_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "gaba")[which_min(opioids_fill_date, gaba_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "gaba" & 
                gaba_days_supply > 5 & 
                overlap / gaba_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(gaba_interval, opioids_interval)) |> 
    group_by(BENE_ID) |> 
    nest()

mediator_prop_opioid_gaba_copresc <- 
    foreach(data = opioids_gabas$data, 
            .combine = "c",
            .options.future = list(chunk.size = 500)) %dofuture% {
                arrange(data, int_start(rx_int)) |> 
                    prop_days_covered()
            }

opioids_gabas <- 
    ungroup(opioids_gabas) |> 
    mutate(mediator_opioid_gaba_copresc = 1, 
           mediator_prop_opioid_gaba_copresc = mediator_prop_opioid_gaba_copresc) |> 
    select(-data)

##  Opioids and muscle relaxants
opioids_mrelax <- 
    merge(opioid, mrelax, by = "BENE_ID") |> 
    as_tibble() |> 
    mutate(mrelax_interval = interval(mrelax_fill_date, mrelax_end_date), 
           opioids_interval = interval(opioids_fill_date, opioids_end_date), 
           overlap = int_overlaps_numeric(mrelax_interval, opioids_interval)) |> 
    rowwise() |> 
    mutate(first_drug = c("opioids", "mrelax")[which_min(opioids_fill_date, mrelax_fill_date)]) |> 
    ungroup() |> 
    filter((first_drug == "mrelax" & 
                mrelax_days_supply > 5 & 
                overlap / mrelax_days_supply >= 0.25) |
               (first_drug == "opioids" & 
                    opioids_days_supply > 5 & 
                    overlap / opioids_days_supply >= 0.25)) |> 
    mutate(rx_int = intersect(mrelax_interval, opioids_interval)) |> 
    group_by(BENE_ID) |> 
    nest()

mediator_prop_opioid_mrelax_copresc <- 
    foreach(data = opioids_mrelax$data, 
            .combine = "c",
            .options.future = list(chunk.size = 500)) %dofuture% {
                arrange(data, int_start(rx_int)) |> 
                    prop_days_covered()
            }

opioids_mrelax <- 
    ungroup(opioids_mrelax) |> 
    mutate(mediator_opioid_mrelax_copresc = 1, 
           mediator_prop_opioid_mrelax_copresc = mediator_prop_opioid_mrelax_copresc) |> 
    select(-data)

plan(sequential)

# Merge datasets ---------------------------------------------------------------

walk(list(opioids_benzos, opioids_stimulants, opioids_gabas, opioids_mrelax), 
     setDT)

walk(list(opioids_benzos, opioids_stimulants, opioids_gabas, opioids_mrelax), 
     \(x) setkey(x, BENE_ID))

coprescriptions <- 
    reduce(list(cohort, 
                opioids_benzos, 
                opioids_stimulants, 
                opioids_gabas, 
                opioids_mrelax), merge, all.x = TRUE)

coprescriptions[is.na(coprescriptions)] <- 0

saveRDS(coprescriptions, file.path(drv_root, "mediator_12mo_opioid_coprescriptions.rds"))
