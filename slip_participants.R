#Load libraries
library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
source("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/ABCD_premie_code/data_functions.R")

slip_release_folder <- "/mnt/isilon/bgdlab_processing/releases_clinical/slip/2024-09_release/"
slip_deliveries <- c("slip_2022", "slip_2023_02", "slip_2023_03", "slip_2023_09", "slip_2023_11", "slip_2024_02", "slip_2024_03", "slip_2024_04", "slip_2024_05")
slip_participants_filenames <- paste0(slip_release_folder, slip_deliveries, "/BIDS/participants.tsv")
slip_participants <- load_data(names = slip_deliveries,slip_participants_filenames) %>% do.call("rbind", .)
length(unique(slip_participants$subject_id))#4269
length(unique(slip_participants$session_id))#4375

sum(!is.na(slip_participants$gestational_age))
sum(!is.na(slip_participants$birth_weight_kg))
sum(!is.na(slip_participants$birth_length_cm))

table(slip_participants$gestational_age)
sum(slip_participants$gestational_age < 37, na.rm = TRUE)
sum(!is.na(slip_participants$gestational_age) & slip_participants$age_at_scan/365 > 8.5 & slip_participants$age_at_scan/365 < 13.5, na.rm = TRUE)
sum(slip_participants$gestational_age < 37 & slip_participants$age_at_scan/365 > 8.5 & slip_participants$age_at_scan/365 < 13.5, na.rm = TRUE)


slip_qc_filename <- "/mnt/isilon/bgdlab_processing/braincharts/SLIP/recon-all-clinical/data/SLIP_QC.csv"
slip_qc <- read.csv(slip_qc_filename) %>% 
  mutate(subject_id = sub("_.*", "", scan_id)) %>%
  mutate(session_id = sub("^[^_]*_([^_]*)_.*$", "\\1", scan_id))

length(unique(slip_qc$subject_id))

sum(slip_qc$subject_id %in% slip_participants$subject_id)

slip_all_qc <- merge(slip_qc, slip_participants, by = c("subject_id", "session_id"), all.x = TRUE)%>%
  filter(euler_mean > -40 & general.white.matter > 0.65 & general.grey.matter > 0.65 & general.csf > 0.65 & 
           general.csf > 0.65 & cerebellum > 0.65 & brainstem > 0.65 & thalamus > 0.65 & 
           putamen.pallidum > 0.65 & hippocampus.amygdala > 0.65)

slip_all_qc$MPR <- grepl("MPR", slip_all_qc$scan_id)

slip_qc_MPR <- slip_all_qc %>% filter(MPR == TRUE)#2727

length(unique(slip_qc_MPR$subject_id))#1153

slip_qc_MPR_adolescent <- slip_qc_MPR %>% filter(age_at_scan/365 > 8.5 & age_at_scan/365 < 13.5)#436
length(unique(slip_qc_MPR_adolescent$subject_id))#173

slip_qc_MPR_adolescent %>%
group_by(subject_id) %>%
  slice_sample(n = 1) %>%
  summary()





