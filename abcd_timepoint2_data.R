#Load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/")
source("ABCD_premie_code/data_functions.R")
out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/"


abcd_processed <- read.csv("CSV/process_tables_5.1/abcd5.1_long_full_2024-10-04.csv") %>% 
  filter(eventname %in% c("baseline_year_1_arm_1", "2_year_follow_up_y_arm_1"))

#IDs for split half
splitOne_train_ids <- read.csv("CSV/process_tables_5.1/abcd_splitOne_train_ids.csv") %>% select(-1)
splitOne_test_ids <- read.csv("CSV/process_tables_5.1/abcd_splitOne_test_ids.csv") %>% select(-1)
splitTwo_train_ids <- read.csv("CSV/process_tables_5.1/abcd_splitTwo_train_ids.csv") %>% select(-1)
splitTwo_test_ids <- read.csv("CSV/process_tables_5.1/abcd_splitTwo_test_ids.csv") %>% select(-1)

vars_to_save <- readRDS("ABCD_premie_code/vars_to_save_ABCD.rds")

abcd_t2 <- abcd_processed %>% filter(eventname %in% c("2_year_follow_up_y_arm_1")) %>% 
  filter(imgincl_t1w_include == 1) %>% 
  select(all_of(vars_to_save))#7896 subject ID
abcd_t1<- abcd_processed %>% filter(eventname %in% c("baseline_year_1_arm_1")) %>% 
  filter(src_subject_id %in% abcd_t2$src_subject_id) %>%
  select(all_of(vars_to_save))#7563 subject id

#for some reason this merged df has 0 obs .... but all of these seem to be identical between t1 and t2 ...
merged <- merge(abcd_t1, abcd_t2, by = c("src_subject_id", "sex_baseline", "gestAge", "PTB", "preterm", 
                                         "twin_statusFAM", "twin_statusP", "triplet_statusFAM",
                                          "genetic_zygosity_status_1", "nonSingleton", "related_status"),
                suffixes = c(".t1",".t2"))#7563!!

#find which cols have all NAs
na_colnames <- colnames(merged)[colSums(is.na(merged)) == nrow(merged)]

merged <- merged %>% select(-na_colnames) %>% 
  rename(devhx_5_p = devhx_5_p.t1, rel_birth_id = rel_birth_id.t1, rel_family_id = rel_family_id.t1)

#site changes
site_changed <- merged[which(merged$site_id_l.t1 != merged$site_id_l.t2),] %>% 
  select(c("site_id_l.t1", "site_id_l.t2"))
dim(site_changed)[1]

#remove those subjects whose site IDs have changed between t1 and t2 (N = 74)
merged <- merged[-which(merged$site_id_l.t1 != merged$site_id_l.t2),]


merged$PCW_between_t1t2 <- merged$PCW_at_scan.t2 - merged$PCW_at_scan.t1

#rename columns for column fit
#site_id_l.t2 as site, PCW_at_scan.t2 as age, sex_baseline as sex
rename_vec <- c(age = "PCW_at_scan.t2", 
                sex = "sex_baseline", 
                site = "site_id_l.t2", nonSingleton = "nonSingleton")
merged <- merged %>% rename(., all_of(rename_vec))




#Save full
filename_full <- paste0(out_folder, "abcd5.1_t1t2_wide_full.csv")
write.csv(merged, filename_full)


#split by split-half IDs

merged_sOne_train <- merged %>% filter(src_subject_id %in% splitOne_train_ids$x)

merged_sOne_test <- merged %>% filter(src_subject_id %in% splitOne_test_ids$x)

merged_sTwo_train <- merged %>% filter(src_subject_id %in% splitTwo_train_ids$x)

merged_sTwo_test <- merged %>% filter(src_subject_id %in% splitTwo_test_ids$x)

#save split-halves
write.csv(merged_sOne_train, paste0(out_folder, "abcd5.1_t1t2_wide_sOne_train.csv"))
write.csv(merged_sOne_test, paste0(out_folder, "abcd5.1_t1t2_wide_sOne_test.csv"))
write.csv(merged_sTwo_train, paste0(out_folder, "abcd5.1_t1t2_wide_sTwo_train.csv"))
write.csv(merged_sTwo_test, paste0(out_folder, "abcd5.1_t1t2_wide_sTwo_test.csv"))
