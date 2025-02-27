#Load libraries
library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)

slip_folder_ <- "/mnt/isilon/bgdlab_processing/releases_clinical/slip/2024-09_release/slip_2024_05/"
slip_participants__filename <- paste0(slip_folder, "BIDS/participants.tsv")
slip_participants_ <- read.table(slip_participants_filename, sep = "\t", header = TRUE)
sum(!is.na(slip_participants_$gestational_age))
sum(!is.na(slip_participants_$birth_weight_kg))
sum(!is.na(slip_participants_$birth_length_cm))
table(slip_participants_$gestational_age)
sum(slip_participants$gestational_age < 37, na.rm = TRUE)
sum(!is.na(slip_participants$gestational_age) & slip_participants$age_at_scan/365 > 8.5 & slip_participants$age_at_scan/365 < 13.5, na.rm = TRUE)
sum(slip_participants$gestational_age < 37 & slip_participants$age_at_scan/365 > 8.5 & slip_participants$age_at_scan/365 < 13.5, na.rm = TRUE)
