#Load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/")
source("ABCD_premie_code/data_functions.R")
abcd_processed <- read.csv("CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-09-12.csv") %>%
  filter(eventname == "baseline_year_1_arm_1")
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
                                             all.x = F, all.y = F)

#Check if twin encoding matches the encoding I have
# SURPRISE - it doesn't exactly ... now need to weed this out
table(abcd_all[which(abcd_all$twin_statusFAM == TRUE), "siblings_twins"])
table(abcd_all[which(abcd_all$twin_statusP == TRUE), "siblings_twins"])

#abcd_all_split1 <- abcd_all[which(abcd_all$matched_group == 1),]
#abcd_all_split2 <- abcd_all[which(abcd_all$matched_group == 2),]

#Check GA/PM distribution between the splits.
#distribution seems similar
hist(abcd_all[which(abcd_all$matched_group == 1),"gestAge"])
hist(abcd_all[which(abcd_all$matched_group == 2),"gestAge"])
#look more closely to GA < 40, also seems similarly distributed
hist(abcd_all[which(abcd_all$matched_group == 1 & abcd_all$gestAge < 40),"gestAge"])
hist(abcd_all[which(abcd_all$matched_group == 2 & abcd_all$gestAge < 40),"gestAge"])

#numbers are similar for PTB vs Not
table(abcd_all[which(abcd_all$matched_group == 1),"PTB"])
table(abcd_all[which(abcd_all$matched_group == 2),"PTB"])

abcd_all_twoSplit <- abcd_all[which(abcd_all$matched_group %in% c("1", "2")),]

t.test(gestAge ~ matched_group, abcd_all_twoSplit)#not different
