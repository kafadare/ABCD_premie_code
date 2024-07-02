#Load libraries
library(dplyr)
library(magrittr)
library(gamlss) #to fit model
library(mgcv) # helps with the gam models
library(tidymv) # helps with the gam models

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
  data_filename <- "~/Documents/Grad_School/BGDLab/ABCD_data/abcd5.1_long_selectVars_dxfilter_062924.csv"
  out_folder <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/"
  output_file <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/gamlss_fits_stats.csv"
}

models_list <-  list.files(models_folder, pattern = "\\.rds$", full.names = TRUE)

#rename some of the variables
rename_vec <- c(age = "PCW_at_scan", sex = "sex_baseline", site = "site_id_l")

df <- read.csv(data_filename) %>% select(-1) %>% rename(., all_of(rename_vec))
df$sex <- as.factor(df$sex)
df$site <- as.factor(df$site)
df$age <- as.numeric(df$age)

model <- readRDS(models_list[n])

df <- df %>% select(sex, age, site, model$phenotype)

gamlss <- gamlss (formula = model$mu.formula,
                  sigma.formula = model$sigma.formula,
                  tau.formula = model$tau.formula,
                  family = model$fam,
                  data = na.omit(df),
                  control = gamlss.control(n_cyc = model$n_crit))

output <- data.frame(model = as.character(gamlss$mu.formula),
                     family_abbr = gamlss$family[1],
                     family = gamlss$family[2],
                     logLik = logLik(gamlss), 
                     AIC = AIC(gamlss), 
                     BIC = BIC(gamlss),
                     n_cyc = model$n_crit,
                     data = basename(data_filename))

modelname <- paste("gamlss_FIT",sub(".*/gamlss_model_(.*)", "\\1", models_list[n]), sep = "_")
filename <- paste0(out_folder,modelname)
saveRDS(output, file = filename)

if (!file.exists(output_file)) {
  write.table(output, file = output_file, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
} else {
  write.table(output, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
}