#Script to find models that have failed to fit, and try to refit them.

#Load libraries
#rm(list=ls())
library(dplyr)
library(magrittr)
library(gamlss) #to fit model
library(mgcv) # helps with the gam models
library(tidygam) # helps with the gam models
library(tidyr)

options(warn = 1)

#read in the arguments passed on from the shell script
args <- commandArgs(trailingOnly = TRUE)

# Is argument length > 1?
if (length(args) > 1) {
  n <- as.numeric(args[1])
  models_folder <- args[2]
  data_filename <- args[3]
  out_folder <- args[4]
  output_file <- args[5]
}else{
  n = 1
  models_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_t1t2"
  data_filename <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_t1t2_wide_sTwo_train.csv"
  out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_long_global_TWO/"
  output_file <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_long_global_TWO/gamlss_fits_stats"
}

#Find if output file does not have matching saved .rds file
output_folder <- paste0(out_folder, "out_messages/")
output_list <- list.files(output_folder, pattern = "\\.txt$", full.names = FALSE) %>% 
  sub(".*?(\\d+)_output\\.txt$", "\\1", .)
fitted_list <- list.files(out_folder, pattern = "\\.rds$", full.names = FALSE) %>% 
  sub("gamlssFIT_", "", .) %>% 
  sub(".rds", "", .)

non_fitted <- setdiff(output_list, fitted_list)

##Need to find a solution to find and load the right model from the "non-fitted" ones ..!

#get list of models and choose the one specified
models_list <-  list.files(models_folder, pattern = "\\.rds$", full.names = TRUE)
print(models_list[n])
model <- readRDS(models_list[n])
#model_name <- paste(Reduce(paste, deparse(model$mu.formula)),model$fam, sep = "__")
print(paste0("family:", model$fam))
print(paste0("mu_formula:",Reduce(paste, deparse(model$mu.formula))))
print(paste0("sigma_formula:",Reduce(paste, deparse(model$sigma.formula))))
print(paste0("nu_formula:",Reduce(paste, deparse(model$nu.formula))))

