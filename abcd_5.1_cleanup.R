#Load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(rlang)
library(gridExtra)
library(purrr)
library(cowplot)
library(gamlss) #to fit model
library(mgcv) # helps with the gam models
library(tidygam) # helps with the gam models
library(tidyr)

source_folder <- "/mnt/isilon/bgdlab_processing/Eren/GestationalAge/"
out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/"
raw_tables_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/raw_tables_5.1/"

#load functions library
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/ABCD_premie_code/")
source(paste0(source_folder,"R/scripts/data_functions.R"))
#source(paste0(source_folder,"R/scripts/lib_mpr_analysis_EK.r"))
#source(paste0(source_folder,"R/scripts/growth_chart_fcts_EK.r"))
#source(paste0(source_folder,"R/scripts/figures.r"))
#source(paste0(source_folder,"R/scripts/GAmodeling_draft.r"))
#source(paste0(source_folder,"R/scripts/centile_functions_ABCD.r"))

#specify the locations for ABCD 5.1 data files
#imaging files
abcd_vol_aseg_file <- paste0(raw_tables_folder,"mri_y_smr_vol_aseg.csv")
abcd_vol_dsk_file <- paste0(raw_tables_folder,"mri_y_smr_vol_dsk.csv")
abcd_mri_adm_file <- paste0(raw_tables_folder,"mri_y_adm_info.csv")
#imaging QC file
abcd_mri_qc_file <- paste0(raw_tables_folder,"mri_y_qc_incl.csv")
#demographics and health hx files
abcd_demo_file <- paste0(raw_tables_folder,"abcd_p_demo.csv")
abcd_devhx_file <- paste0(raw_tables_folder,"ph_p_dhx.csv")
abcd_medhx_file <- paste0(raw_tables_folder,"ph_p_mhx.csv")
abcd_ltrack_file <- paste0(raw_tables_folder,"abcd_y_lt.csv")
abcd_screen_file <- paste0(raw_tables_folder,"abcd_p_screen.csv")
abcd_gen_file <- paste0(raw_tables_folder,"gen_y_pihat.csv")

#Load the data files specified above 
abcd_data <- load_data(names = c("abcd_vol_aseg", "abcd_vol_dsk", "abcd_mri_adm", "abcd_mri_qc", "abcd_demo", "abcd_devhx", "abcd_medhx", "abcd_ltrack"), 
                       c(abcd_vol_aseg_file, abcd_vol_dsk_file, abcd_mri_adm_file, abcd_mri_qc_file, abcd_demo_file, abcd_devhx_file, abcd_medhx_file,abcd_ltrack_file))

## Jul 4th 2024 This field is populated with err, when compared against the gen_y_pihat data, it should be "52671" for birth ID. Also makes sense, because there is only one row with this family ID.
# Cross-checked all other rows for family ID and birth ID against gen_y_pihat data, they are all the same with the abcd_ltrack data. So I will use the abcd_ltrack data.
# Changing this field here manually to avoid issues down the line in case I decide to use birth ID for any exclusions/analyses.
abcd_data$abcd_ltrack[which(abcd_data$abcd_ltrack$rel_family_id == 5267),"rel_birth_id"] <- 52671

abcd_long <- Reduce(function(x, y) merge(x, y, by = c("src_subject_id", "eventname"), all=TRUE), abcd_data, accumulate=FALSE)

#drop "eventname" column because the variables in the screener are one-time values (not longitudinally changing)
abcd_screener <- read.csv(abcd_screen_file) %>% select(-eventname)

#merge abcd_long data with screener variables
abcd_long <- merge(abcd_long, abcd_screener, by = "src_subject_id", all = TRUE)

#calculate total mri phenotypes adding RH and LH
# !!! Need to figure out if there is total cortical GM available somewhere in the aseg segmentation data
abcd_long$totalWM_cb <- abcd_long$smri_vol_scs_cbwmatterlh + abcd_long$smri_vol_scs_cbwmatterrh
#abcd_long$totalGM_cb <- smri_vol_scs_suprateial
abcd_long$totalWM_crb <- abcd_long$smri_vol_scs_crbwmatterlh + abcd_long$smri_vol_scs_crbwmatterrh
abcd_long$totalGM_crb <- abcd_long$smri_vol_scs_crbcortexlh + abcd_long$smri_vol_scs_crbcortexrh

#sum all regions bilaterally

rh_ind <- grep("rh$", names(abcd_long))
lh_ind <- grep("lh$", names(abcd_long))

names <- names(abcd_long)[grep("rh$", names(abcd_long))] %>% gsub("rh$", "", .)

blt_vol <- apply(cbind(rh_ind,lh_ind), 1, function(i) {
  abcd_long[,i[1]] + abcd_long[,i[2]]
})

colnames(blt_vol) <- paste0(names, "_total")

abcd_long <- cbind(abcd_long, blt_vol)

#convert all events to "months" baseline = 0
abcd_long$year <- NA
abcd_long$year <- gsub("_year_follow_up_y_arm_1$", "", abcd_long$eventname)
abcd_long[abcd_long$year %in% c("baseline_year_1_arm_1"),"year"] <- 0
abcd_long$month <- NA
abcd_long$month <- as.numeric(abcd_long$year) * 12
abcd_long[is.na(abcd_long$month),]$month <-  as.numeric(abcd_long[is.na(abcd_long$month),]$eventname %>% gsub("_month_follow_up_arm_1$", "", .))

#Get GA from the dhx survey, compare baseline & 4yr f/up reporting (if duplicated). Decided to use baseline reporting, very few people filled the fouryear survey.
baseline_dev <- abcd_data$abcd_devhx[abcd_data$abcd_devhx$eventname == "baseline_year_1_arm_1",]
fouryear_dev <- abcd_data$abcd_devhx[abcd_data$abcd_devhx$eventname == "4_year_follow_up_y_arm_1",]
table(baseline_dev$devhx_12a_p)
table(baseline_dev$devhx_12_p)
table(fouryear_dev$devhx_12a_p) #only 30 responses, 3 "yes"
table(fouryear_dev$devhx_12_p)
base_ids_na <- baseline_dev[is.na(baseline_dev$devhx_12a_p), c("src_subject_id","devhx_12a_p", "devhx_12_p")] 
ids <- fouryear_dev[!is.na(fouryear_dev$devhx_12a_p), c("src_subject_id","devhx_12a_p", "devhx_12_p")] 
sum(base_ids_na$src_subject_id %in% ids$src_subject_id) #no NAs in baseline data have data in four year f/up
ids_base <-baseline_dev[which(baseline_dev$src_subject_id %in% ids$src_subject_id),  c("src_subject_id","devhx_12a_p", "devhx_12_p")]
## one subject reported "premature" in 4year f/up, but not at baseline (NDAR_INVL7KKBD44). one subject reported "premature" in baseline but not at 4year f/up (NDAR_INVEJHM574R). Both cases reported prematurity by 1 week (would correspond to 39 weeks GA). I will use the baseline reported value.
## In new column gestAge, reporting "don't know" to "premature or no" is encoded as NA (140) + 2 subjects had NA for this question. Total NA in gestAge is 142 
## Reporting "don't know" to "how premature" (999) at baseline N = 27. This will also be encoded as NA for gestAge column. For premature vs not analysis, these wil be considered "premature".
baseline_dev$gestAge <- ifelse(baseline_dev$devhx_12a_p == "0", 40, ifelse(baseline_dev$devhx_12a_p == "999", NA, ifelse(baseline_dev$devhx_12_p == "999", NA, 40 - baseline_dev$devhx_12_p)))
table(baseline_dev$gestAge)
baseline_dev$PTB <- baseline_dev$devhx_12a_p 
baseline_dev <- baseline_dev %>% mutate(PTB = na_if(PTB, 999))
##Three subject IDs that do not have any data associated with them other than mri data(at baseline) and screener data. No demographics etc. Keep/Remove?

#Merge gestAge and PTB info to ABCD long
abcd_long <- merge(abcd_long, baseline_dev[,c("src_subject_id", "gestAge", "PTB")], by = c("src_subject_id"), all = TRUE) 

#Now deal with sex assigned at birth, use demographic data from baseline for all
unique(abcd_data$abcd_demo$eventname) #every year
table(abcd_data$abcd_demo$eventname)
baseline_demo <-  abcd_data$abcd_demo[abcd_data$abcd_demo$eventname == "baseline_year_1_arm_1",]
sum(is.na(baseline_demo$demo_sex_v2)) #no NAs in baseline data
baseline_demo$sex_baseline <- baseline_demo$demo_sex_v2
#Merge sex info
abcd_long <- merge(abcd_long, baseline_demo[,c("src_subject_id", "sex_baseline")], by = c("src_subject_id"), all = TRUE) 
#total 145 IDs where PTB is NA 
#3 of them as mentioned above do not have medhx data at all
#2 had NA for the PTB question, 140 responded "don't know"
#total of 172 IDs where gestAge is NA. 
#3 of them as mentioned above no medhx data. 2 had NA for the PTB question, 140 responded "don't know" to PTB question, 
#27 responded "don't know" to how many weeks premature question. 2

#Sanity Check to make sure to subject-event pairs are repeated
dup_indices <- duplicated(abcd_long[, c("src_subject_id", "eventname")]) | duplicated(abcd_long[, c("src_subject_id", "eventname")], fromLast = TRUE)
sum(dup_indices) #0 duplicates - what we want.
rm(dup_indices)
rm(baseline_dev, fouryear_dev, base_ids_na, ids, ids_base, baseline_demo)

#Calculate post-conceptual-weeks at time of scan. Using 4.3 weeks/month (average) + gestAge (when available)
abcd_long$PCW_at_scan <- as.numeric(abcd_long$interview_age)*4.34 + abcd_long$gestAge
sum(is.na(abcd_long$PCW_at_scan)) #1243 NAs, 1235 gestAge NAs + 8 interview_age NAs
#define preterm categories
abcd_long$preterm <- as.factor(cut(abcd_long$gestAge, breaks = c(-Inf, 31.9, 36.9, Inf), labels = c("VPM", "LPM", "Term"), include.lowest = TRUE))

#Deal with sex, convert to 1 for M and 2 for F, and make class factor
sum(is.na(abcd_long$sex_baseline)) # 0, using baseline "sex" for all
abcd_long$sex_baseline <- as.factor(ifelse(abcd_long$sex_baseline %in% "1", "M", 
                                           ifelse(abcd_long$sex_baseline %in% "2", "F", 
                                                  ifelse(abcd_long$sex_baseline %in% "3", "I", NA))))

#Remove based on QC image inclusion recommendation for T1W images
#use the include/don't include label for t1w
sum(is.na(abcd_long$smri_vol_scs_allventricles))
#remove rows without imaging data -- using all ventricle volume to check
abcd_long <- abcd_long %>% filter(!is.na(smri_vol_scs_allventricles))
table(abcd_long$eventname)
#remove rows that aren't recommended for image inclusion
sum(is.na(abcd_long$imgincl_t1w_include))
abcd_long <- abcd_long %>% filter(imgincl_t1w_include == 1) #22166 obs left. (longitudinal)

#Subset baseline data to remove siblings
baseline_data <- abcd_long %>% filter(eventname == "baseline_year_1_arm_1") #11270
#sanity checks
length(unique(baseline_data$src_subject_id))#11270
sum(baseline_data$imgincl_t1w_include == 1)#11270

### Diagnosis exclusion code went here originally. ####
##IN THE FUTURE !!!! FOR LONGITUDINAL ANALYSIS !!!!! 
#check for diagnoses not reported at baseline (might be relevant for longitudinal analysis)
# ?? should remove "diagnosis since last time" from the longitudinal analyses?? 
#in that case we should also remove looking at the in-between questionnaires since "last saw you" implies that (assuming the questionnaries are given at non-imaging visits as well....)

#For twin/non-singleton status, using rel_birth_ID because it is available for all subjects.
#rel_family_id and rel_birth_id columns in the abcd_ltrack table are only populated for baseline data. Jul 4th 24 (checked)
baseline_data$twin_statusFAM <- duplicated(baseline_data$rel_birth_id) | duplicated(baseline_data$rel_birth_id, fromLast=TRUE) #1982 TRUE
anyDuplicated(baseline_data$rel_birth_id)
sum(na.omit(baseline_data$devhx_5_p == 1))#2115 ... discrepancy :( .... Maybe twin is not included in the study in this case? So should I use this one instead?
baseline_data$twin_statusP <- baseline_data$devhx_5_p == 1
#merge Twin Status info
abcd_long <- merge(abcd_long, baseline_data[,c("src_subject_id", "twin_statusFAM", "twin_statusP")], by = c("src_subject_id"), all = TRUE) 
#Sanity Check to make sure to subject-event pairs are not repeated
dup_indices <- duplicated(abcd_long[, c("src_subject_id", "eventname")]) | duplicated(abcd_long[, c("src_subject_id", "eventname")], fromLast = TRUE)
sum(dup_indices) #0 duplicates - what we want.

#Keep one subject per family.
length(unique(baseline_data$rel_family_id)) #9396
set.seed(42)
baseline_famFilt <- baseline_data %>%
  group_by(rel_family_id) %>%
  slice_sample(n = 1)

#Only keep one data per family ID
abcd_long_toSave <- abcd_long %>% filter(src_subject_id %in% baseline_famFilt$src_subject_id)

#select useful variables for future analysis
vars_to_save <- c("src_subject_id", "eventname", "sex_baseline", "gestAge", "PTB", "preterm", "devhx_5_p",
                  "PCW_at_scan", "interview_age", "year", "month", "site_id_l", "twin_statusFAM", "twin_statusP",
                  "rel_family_id", "rel_birth_id", 
                  "imgincl_t2w_include", "imgincl_t1w_include","mri_info_deviceserialnumber", "mri_info_softwareversion",
                  names(abcd_long)[grep("smri", names(abcd_long))], "totalWM_cb", "totalWM_crb", "totalGM_crb")

abcd_long_toSave <- abcd_long_toSave %>% select(all_of(vars_to_save)) #140 variables

write.csv(abcd_long_toSave, file = paste0(out_folder,"abcd5.1_long_selectVars_dxfilter_", Sys.Date(),".csv"))
write.csv(abcd_long_toSave, file = paste0(out_folder,"abcd5.1_long_selectVars_NOdxfilter_famfilter", Sys.Date(),".csv"))
write.csv(abcd_long, file = paste0(out_folder,"abcd5.1_long_full_", Sys.Date(),".csv"))

#convert to wide
scrn_names <- names(abcd_screener[,2:81]) #screener questions aren't by event, so separate them out
values_cols <- setdiff(names(abcd_long), 
                       c("eventname", "src_subject_id", "gestAge", "PTB", "sex_baseline", "twin_statusFAM", "twinstatusP", scrn_names)) #define the names that should not be converted to wide
abcd_wide <- abcd_long %>% pivot_wider(., names_from = "eventname", values_from = all_of(values_cols))

abcd_wide_toSave <- abcd_long_toSave %>% pivot_wider(., names_from = "eventname", values_from = any_of(values_cols))

write.csv(abcd_wide_toSave, file = paste0(out_folder,"abcd5.1_wide_selectVars_dxfilter_famfilter", Sys.Date(),".csv"))
write.csv(abcd_wide_toSave, file = paste0(out_folder,"abcd5.1_wide_selectVars_NOdxfilter_famfilter", Sys.Date(),".csv"))
write.csv(abcd_wide, file = paste0(out_folder,"abcd5.1_wide_full_", Sys.Date(),".csv"))


#rm(abcd_data, abcd_screener, scrn_names, values_cols, vars_to_save, abcd_long, abcd_wide, abcd_long_toSave, abcd_wide_toSave)

#test <- read.csv(paste0(out_folder,"abcd5.1_long_selectVars_NOdxfilter_famfilter2024-07-04.csv"))



### Diagnosis Exclusion STUFF

#Medical Diagnoses/Exclusions List
#deal with diagnoses/exclusions
#complications at birth
#devhx_14a3_p - blue at birth
#devhx_14b3_p - slow heartbeat at birth
#devhx_14c3_p - did not eat at first
#devhx_14d3_p - convulsions
#devhx_14e3_p - jaundice
#devhx_14f3_p - required oxygen
#devhx_14g3_p - required blood transfusion
#devhx_14h3_p - Rh incompatability
#devhx_15 - incubator how many days (includes 0 for none)
#devhx_15_dk - incubator - don't know
#devhx_16_p - fever >104 for first 12 months of life, how many days?
#devhx_17_p - first 12 months, how many days with serious illness/infection?
#medhx_2c - brain injury
#medhx_2e - cancer/leukemia
#medhx_2f - cerebral palsy
#medhx_2g - diabetes
#medhx_2h - epilepsy
#medhx_2i - hearing problem
#medhx_2j - kidney disease
#medhx_2k - lead poisoning
#medhx_2l - muscular dystrophy
#medhx_2m - multiple sclerosis
#medhx_2n - vision problems
#medhx_2o - heart problems
#medhx_2p - sickle cell anemia
#medhx_2q - very bad headaches
#medhx_2r - surgery ("an operation")
#medhx_2s - any other illness
#medhx_6i - head injury,  medhx_6i_times
#medhx_6o - overdose, medhx_6o_notes
#medhx_6p - seizures, medhx_6p_notes
#medhx_8a - hospital overnight or longer
## with _l at the end, the question changes to "since we last saw you"


# #look at diagnoses in baseline data
# ##Aaron Meeting Jul 2nd, Kevin confirmation, NOT removing any data based on these discussions. Jul 4th 2024.
# #medhx_2c - brain injury **
# #medhx_2e - cancer/leukemia **
# #medhx_2f - cerebral palsy **
# #medhx_2g - diabetes
# #medhx_2h - epilepsy **
# #medhx_2i - hearing problem
# #medhx_2j - kidney disease
# #medhx_2k - lead poisoning **
# #medhx_2l - muscular dystrophy **
# #medhx_2m - multiple sclerosis **
# #medhx_2n - vision problems
# #medhx_2o - heart problems
# #medhx_2p - sickle cell anemia **
# table(baseline_data$medhx_2c)#193 yes
# table(baseline_data$medhx_2e)#23 yes
# table(baseline_data$medhx_2f)#17 yes
# table(baseline_data$medhx_2h)#211 yes
# table(baseline_data$medhx_2j)#211 yes
# table(baseline_data$medhx_2k)#55 yes
# table(baseline_data$medhx_2l)#15 yes
# table(baseline_data$medhx_2m)#13 yes
# table(baseline_data$medhx_2o)#362 yes
# table(baseline_data$medhx_2p)#38 yes
# 
# table(baseline_data$gestAge)
# baseline_data_dxfilter <- baseline_data %>% filter(medhx_2c == 0 & medhx_2e == 0 & medhx_2f == 0 & medhx_2h == 0 & medhx_2k == 0 & medhx_2l == 0 & medhx_2m == 0 & medhx_2p == 0)
# #length baseline after dx filter above - 10808
# table(baseline_data_dxfilter$gestAge) #still has a good spread of gestational age
# ##If I remove all of the above then
# baseline_data_dxfilter_strict <- baseline_data %>% filter(medhx_2c == 0 & medhx_2e == 0 & medhx_2f == 0 & medhx_2h == 0 & medhx_2k == 0 & medhx_2l == 0 & medhx_2m == 0 & medhx_2p == 0 & medhx_2g == 0 & medhx_2i == 0 & medhx_2j == 0 & medhx_2n == 0 & medhx_2o == 0)
# #length baseline after strict dx filter above - 7615
# table(baseline_data_dxfilter_strict$gestAge) #still has a good spread of gestational age
# 
# #filtering out birth complications AND the original dx filter
# #complications at birth
# #devhx_14a3_p - blue at birth **
# #devhx_14b3_p - slow heartbeat at birth
# #devhx_14c3_p - did not eat at first
# #devhx_14d3_p - convulsions **
# #devhx_14e3_p - jaundice
# #devhx_14f3_p - required oxygen **
# #devhx_14g3_p - required blood transfusion **
# #devhx_14h3_p - Rh incompatability
# #devhx_5_p - does your child have a twin?
# baseline_data_dx_birth_filter <- baseline_data %>% filter(medhx_2c == 0 & medhx_2e == 0 & medhx_2f == 0 & medhx_2h == 0 & medhx_2k == 0 & medhx_2l == 0 & medhx_2m == 0 & medhx_2p == 0 & devhx_14a3_p == 0 & devhx_14d3_p == 0 & devhx_14f3_p == 0 & devhx_14g3_p == 0)
# #length baseline after dx and birth filter above - 9300
# table(baseline_data_dx_birth_filter$gestAge) #lose a lot of 30-37 week people here.
# 
# rm(baseline_data_dxfilter_strict, baseline_data_dx_birth_filter)

## Jun 29 2024 I will go ahead with only using the first DX filter for now. I should discuss again. I'll save both data including twins and data without twins. EK 6/29/2024.
# Also note - using self-report dev questionnaire for twin filtering here, not using the group ID variable from the genetics tables.
