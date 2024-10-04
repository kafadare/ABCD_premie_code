#Load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/")
source("ABCD_premie_code/data_functions.R")
out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/"


abcd_processed <- read.csv("CSV/process_tables_5.1/abcd5.1_long_full_2024-10-04.csv") %>%
  filter(eventname == "baseline_year_1_arm_1")

#select useful variables for future analysis
vars_to_save <- c("src_subject_id", "eventname", "sex_baseline", "gestAge", "PTB", "preterm", "devhx_5_p",
                  "PCW_at_scan", "interview_age", "year", "month", "site_id_l", "twin_statusFAM", 
                  "twin_statusP","triplet_statusFAM","rel_family_id", "rel_birth_id", 
                  "genetic_zygosity_status_1", "nonSingleton", "related_status",
                  "imgincl_t2w_include", "imgincl_t1w_include","mri_info_deviceserialnumber", "mri_info_softwareversion",
                  names(abcd_processed)[grep("smri", names(abcd_processed))], "totalWM_cb", "totalWM_crb", "totalGM_crb")
saveRDS(vars_to_save, file = "ABCD_premie_code/vars_to_save_ABCD.rds")

abcd_split <- read.table('CSV/participants.tsv', sep = "\t", header = T) %>% 
  mutate(participant_id = gsub("sub-", "", .$participant_id)) %>% 
  mutate(participant_id = gsub("NDAR", "NDAR_", .$participant_id)) %>%
  rename(src_subject_id = participant_id, site_id_l = site)
site_IDS <- readRDS("CSV/DEAP-siteID.rds") %>%
  filter(event_name == "baseline_year_1_arm_1" & src_subject_id %in% abcd_processed$src_subject_id)
#Check if the siteID.rds file Kevin shared matches the site IDs in my original table from 5.1 data release.
identical(order(paste0(abcd_processed$src_subject_id, abcd_processed$site_id_l)), 
          order(paste0(site_IDS$src_subject_id, site_IDS$abcd_site))) 
#^TRUE. Means that the site IDs from my processed table are the ones being used, so go ahead with those ones.
#length(unique(abcd_processed$rel_family_id))#9396 unique family IDs - sanity check for keeping 1 person per family.

#Load file with birth weight and GA PRS in ABCD.
abcd_prs <- read.csv('CSV/abcd_clean.csv')


out <- abcd_split[which(duplicated(abcd_split$src_subject_id) | duplicated(abcd_split$src_subject_id, fromLast = TRUE)),] %>%
  group_by(src_subject_id) %>%
  mutate(col_diff = find_diff_cols(cur_data())) %>%
  ungroup()
table(out$col_diff)

#merge processed data and ARMS table by participant ID and site (duplicate participant IDs in the participant.tsv table with different site IDs)
#only keep those rows that exist in abcd_processed
abcd_all <- merge(abcd_processed, abcd_split[,names(abcd_split) %in% c("src_subject_id", "siblings_twins",
                                            "matched_group", "handedness", 
                                            "parental_education", "income", 
                                            "anesthesia_exposure", "scanner_model",
                                            "scanner_software", "participant_education", "site_id_l")], 
                                             by = c("src_subject_id", "site_id_l"), 
                                             all.x = T, all.y = F)

#Check if twin encoding matches the encoding I have
# 0 -single, 1 - siblings, 2 - twins, 3 - triplets
# SURPRISE - it doesn't match. I will go ahead with the nonSingleton encoding I have decided on until further notice.
table(abcd_all[which(abcd_all$twin_statusFAM == TRUE), "siblings_twins"])#11 singles and 39 siblings?
table(abcd_all[which(abcd_all$triplet_statusFAM == TRUE), "siblings_twins"])#all encoded as triplets
table(abcd_all[which(abcd_all$twin_statusP == TRUE), "siblings_twins"])#75 singles and 44 siblings
table(abcd_all[which(abcd_all$siblings_twins > 1), "genetic_zygosity_status_1"])#12 siblings 

abcd_all_test <- abcd_all[which(abcd_all$matched_group == 1),]
abcd_all_train <- abcd_all[which(abcd_all$matched_group == 2),]

#check how many repeated family IDs in each split group
paste0("Test, # of unique family IDS:",length(unique(abcd_all_test$rel_family_id)),
       "  unique # of participants:",length(unique(abcd_all_test$src_subject_id)),
       "  # of rows in the dataframe:",dim(abcd_all_test)[1])
paste0("Train, # of unique family IDS:",length(unique(abcd_all_train$rel_family_id)),
       "  unique # of participants:",length(unique(abcd_all_train$src_subject_id)),
       "  # of rows in the dataframe:",dim(abcd_all_train)[1])
#Repeated family ids in each group. Keep one per family in each group, then cross-check the groups for repeated family IDs in between.
#Keep one subject per family in the TRAIN set, and also ensure no shared family IDs between TRAIN and TEST
#Remove matching ones from TEST. (check numbers between TRAIN and TEST sets to ensure that the N is similar.)
set.seed(42)
abcd_all_train_famFilt <- abcd_all_train %>%
  group_by(rel_family_id) %>%
  slice_sample(n = 1) #got rid of 901 people in the train set
#look for people who share family IDs between train and test groups.
abcd_all_test_famFilt <- abcd_all_test %>%
  filter(!(rel_family_id %in% abcd_all_train_famFilt$rel_family_id))#got rid of 73 people in the test set
sum(duplicated(abcd_all_test_famFilt$rel_family_id))#886 duplicates in the test set
#sanity check
sum(abcd_all_test_famFilt$rel_family_id %in% abcd_all_train_famFilt$rel_family_id) #0

#Check GA/PM distribution between the splits.
#distribution seems similar
hist(abcd_all_train_famFilt$gestAge)
hist(abcd_all_test_famFilt$gestAge)
#look more closely to GA < 40, also seems similarly distributed
hist(abcd_all_train_famFilt[abcd_all_train_famFilt$gestAge < 40,]$gestAge)
hist(abcd_all_test_famFilt[abcd_all_test_famFilt$gestAge < 40,]$gestAge)
#numbers are similar for PTB vs Not
table(abcd_all_train_famFilt$PTB)
table(abcd_all_test_famFilt$PTB)

abcd_matched_train <- abcd_all_train_famFilt %>% select(all_of(vars_to_save)) #242 variables
abcd_matched_test <- abcd_all_test_famFilt %>% select(all_of(vars_to_save)) #242 variables

write.csv(abcd_matched_train, file = paste0(out_folder,
                                            "abcd_baseline_matchedTrain_selectVars_famfilter", 
                                            Sys.Date(),".csv"))
write.csv(abcd_matched_test, file = paste0(out_folder,
                                            "abcd_baseline_matchedTest_selectVars_famfilter", 
                                            Sys.Date(),".csv"))
