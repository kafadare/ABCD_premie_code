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
}else{
  models_folder <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_models/"
  data_filename <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-07-15.csv"
  out_folder <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits_CG/"
  output_file <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/gamlss_fits_stats_CG.csv"
}

#rename some of the variables and select baseline scans
rename_vec <- c(age = "PCW_at_scan", sex = "sex_baseline", site = "site_id_l", twin = "twin_statusFAM")

#read in the dataframe and set variable types
df <- read.csv(data_filename) %>% select(-1) %>% rename(., all_of(rename_vec)) %>% subset(eventname == "baseline_year_1_arm_1")#9396 subjects!
df$sex <- as.factor(df$sex)
df$site <- as.factor(df$site)
df$age <- as.numeric(df$age)

#get list of models and choose the one specified
models_list <-  list.files(models_folder, pattern = "\\.rds$", full.names = TRUE)
model <- readRDS(models_list[n])
#model_name <- paste(Reduce(paste, deparse(model$mu.formula)),model$fam, sep = "__")
print(paste(Reduce(paste, deparse(model$mu.formula)),model$fam, sep = "__"))

twin <- ifelse(sum(grepl("twin", as.character(model$mu.formula))) != 0, "TWIN", NA)

#select variables for gamlss, including phenotype specified by the loaded model
df <- df %>% select(sex, age, site, twin, model$phenotype)

#modelname <- paste("gamlss_FIT",sub(".*/gamlss_model_(.*)", "\\1", model_name), sep = "__")
modelname <- paste("gamlssFIT", model$fam, model$phenotype, twin, sep = "__")
filename <- paste0(out_folder,modelname)

gamlss <- gamlss (formula = model$mu.formula,
                  sigma.formula = model$sigma.formula,
                  tau.formula = model$tau.formula,
                  family = model$fam,
                  method = CG(),
                  data = na.omit(df),
                  control = gamlss.control(n_cyc = model$n_crit))

output <- data.frame(model = Reduce(paste, deparse(model$mu.formula)),
                     family_abbr = gamlss$family[1],
                     family = gamlss$family[2],
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
