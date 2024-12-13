#remove outliers from phenotype set variables, to re-run models.
#Outlier defined as > 3 MADs
#Load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/")
source("ABCD_premie_code/data_functions.R")
out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/"


abcd_processed <- read.csv("CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-09-20.csv") %>%
  filter(eventname == "baseline_year_1_arm_1")

phenotype_set <- readRDS("ABCD_premie_code/phenotype_set.rds")
vars_toSave <- readRDS("ABCD_premie_code/vars_to_save_ABCD.rds")

mad_factor <- function(data){
  mad <- mad(data)
  mad_factor <- abs(mean(data) - data)/mad
  return(mad_factor)
}

df <- abcd_processed %>%
  mutate(across(all_of(phenotype_set), mad_factor, .names = "{.col}_mad"))

dev_counts <- df %>% select(ends_with("_mad")) %>%
                summarise(across(everything(), ~ sum(. > 3))) %>%
                pivot_longer(everything(), names_to = "column", values_to = "count") %>%
                arrange(desc(count))

df <- df %>%
  mutate(across(all_of(phenotype_set), ~ ifelse(df[[paste0(cur_column(), "_mad")]] > 3, NA, .)))
df_save <- df %>% select(all_of(vars_toSave))


write.csv(df_save, file = paste0(out_folder,"abcd5.1_long_selectVars_NOdxfilter_famfilter_outliersRM", Sys.Date(),".csv"))
                
                
                
                