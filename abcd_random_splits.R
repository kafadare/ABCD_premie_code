#Script to obtain random splits of the ABCD data
#Second block of code is testing how to use these partitions when applying model fitting to multiple splits.

#Load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/")
source("ABCD_premie_code/data_functions.R")
out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/random_splits/"

#select useful variables for future analysis
vars_to_save <- readRDS("ABCD_premie_code/vars_to_save_ABCD.rds")

abcd_full <- bind_rows(read.csv("CSV/process_tables_5.1/abcd_baseline_matchedTrain_selectVars_famfilter2024-09-20.csv"), 
                       read.csv("CSV/process_tables_5.1/abcd_baseline_matchedTest_selectVars_famfilter2024-09-20.csv"))

## 50-50 split
smp_size <- floor(0.5 * nrow(abcd_full))#4590
set.seed(42)
random_seeds <- runif(100, min = 1, max = 1000)

for(i in 1:length(random_seeds)){
## set the seed to make your partition reproducible
set.seed(random_seeds[i])
train_ind <- sample(seq_len(nrow(abcd_full)), size = smp_size)
filename = paste0(out_folder, "split",i, ".rds")
saveRDS(train_ind, file = filename)
}



####Code for loading the split and removing related individuals within each group (train and test)
## Should I make this a function?

#select useful variables for future analysis
split_no = 1 #When using this block of code, get this as input from the function (batch job id for example)
#Folder defined below, but in use can provide this folder as input too.
split_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/random_splits/"
#Define filename for split to use in this case
filename = paste0(split_folder, "split",split_no, ".rds")

#Define output folder to save the data tables for train/test if save = TRUE.
out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/random_splits/data/"
save = TRUE #defined as true to test the saving, when using in the future might make this an input.

#Load variables you want to save, that are likely relevant to the future analysis/work.
vars_to_save <- readRDS("ABCD_premie_code/vars_to_save_ABCD.rds")

#Merge the two original (from matched ARMS samples) train/test datasets to make sure each random split is on the same data.
#Also the random split indices were derived from the row numbers of this combination.
abcd_full <- bind_rows(read.csv("CSV/process_tables_5.1/abcd_baseline_matchedTrain_selectVars_famfilter2024-09-20.csv"), 
                       read.csv("CSV/process_tables_5.1/abcd_baseline_matchedTest_selectVars_famfilter2024-09-20.csv")) %>%
                      select(all_of(vars_to_save)) #242 variables

train_ind <- readRDS(filename)

train <- abcd_full[train_ind,]
test <- abcd_full[-train_ind,]

##Check for repeated family IDs within train and test, and remove if they exist.

#check how many repeated family IDs in each split group
paste0("Train, # of unique family IDS:",length(unique(train$rel_family_id)),
       "  unique # of participants:",length(unique(train$src_subject_id)),
       "  # of rows in the dataframe:",dim(train)[1])
paste0("Test, # of unique family IDS:",length(unique(test$rel_family_id)),
       "  unique # of participants:",length(unique(test$src_subject_id)),
       "  # of rows in the dataframe:",dim(test)[1])

#Repeated family ids in each group. Keep one per family in each group, 
#then cross-check the groups for repeated family IDs in between.
#Keep one subject per family WITHIN each split.
set.seed(42)
train <- train %>%
  group_by(rel_family_id) %>%
  slice_sample(n = 1)
test <- test %>%
  group_by(rel_family_id) %>%
  slice_sample(n = 1)

#get numbers on the new N for train and test
paste0("Train N: ",dim(train)[1],"  Test N: ", dim(test)[1])

#look for people who share family IDs between train and test groups.
paste0("# of family IDs shared between train and test groups: ",
       sum(train$rel_family_id %in% test$rel_family_id))

#Check GA/PM distribution between the splits.
#distribution seems similar
hist(train$gestAge)
hist(test$gestAge)

#Save the family filtered results, or use them in further analysis
if (save){
  write.csv(train, file = paste0(out_folder,"Train_split", split_no,"_",
                                              Sys.Date(),".csv"))
  write.csv(test, file = paste0(out_folder,"Test_split", split_no,"_",
                                Sys.Date(),".csv"))
}

