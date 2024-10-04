#Load libraries
rm(list=ls())
library(dplyr)
library(magrittr)
library(gamlss) #to fit model
library(mgcv) # helps with the gam models
library(tidygam) # helps with the gam models

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
  #method <- args[6]
}else{
  n = 1
  models_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_allPhen/"
  data_filename <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd_baseline_matchedTrain_selectVars_famfilter2024-10-04.csv"
  out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen/"
  output_file <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen/gamlss_fits_stats_allPhen.csv"
  #method <- quote(CG)
  }

#rename some of the variables and select baseline scans
rename_vec <- c(age = "PCW_at_scan", sex = "sex_baseline", site = "site_id_l", nonSingleton = "nonSingleton")

# #read in the dataframe and set variable types, subset for only baseline data
#subset for age (PCW at scan) not being NA (will be NA is gestAge was NA, should be N = 66)
#subset for nonSingleton not being NA(will be NA if the parent questionnaire was NA, should be N = 2)
#subset for sex %in% M or F, so no I in the model (should be N = 3 for I)
df <- read.csv(data_filename) %>% select(-1) %>% rename(., all_of(rename_vec)) %>% 
  subset(eventname == "baseline_year_1_arm_1" & !is.na(age) & !is.na(nonSingleton) & (sex %in% c("M", "F")))#4512 folks
df$sex <- as.factor(df$sex)
df$site <- as.factor(df$site)
df$age <- as.numeric(df$age)

#get list of models and choose the one specified
models_list <-  list.files(models_folder, pattern = "\\.rds$", full.names = TRUE)
model <- readRDS(models_list[n])
#model_name <- paste(Reduce(paste, deparse(model$mu.formula)),model$fam, sep = "__")
print(paste(Reduce(paste, deparse(model$mu.formula)),model$fam, sep = "__"))

nonSingleton <- ifelse(sum(grepl("nonSingleton", as.character(model$mu.formula))) != 0, "nonSingleton", NA)

#select variables for gamlss, including phenotype specified by the loaded model
df <- df %>% select(sex, age, site, nonSingleton, model$phenotype)

#modelname <- paste("gamlss_FIT",sub(".*/gamlss_model_(.*)", "\\1", model_name), sep = "__")
modelname <- paste("gamlssFIT", model$fam, model$phenotype, nonSingleton, sep = "__")
filename <- paste0(out_folder,modelname,".rds")

gamlss <- gamlss (formula = model$mu.formula,
                  sigma.formula = model$sigma.formula,
                  tau.formula = model$tau.formula,
                  family = model$fam,
                  method = CG(),
                  data = df,
                  control = gamlss.control(n_cyc = model$n_crit))

output <- data.frame(model = Reduce(paste, deparse(model$mu.formula)),
                     family_abbr = gamlss$family[1],
                     family = gamlss$family[2],
                     method = as.character(gamlss$method),
                     logLik = logLik(gamlss), 
                     AIC = AIC(gamlss), 
                     BIC = BIC(gamlss),
                     n_cyc = model$n_crit,
                     data = basename(data_filename),
                     warning = NA)
print(filename)
saveRDS(gamlss, file = filename)

if (!file.exists(output_file)) {
  write.table(output, file = output_file, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
} else {
  write.table(output, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
}
