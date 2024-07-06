#Load libraries
rm(list=ls())
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
  out_err_folder <- args[6]
}else{
  models_folder <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_models/"
  data_filename <- "~/Documents/Grad_School/BGDLab/ABCD_data/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-07-04.csv"
  out_folder <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/"
  output_file <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/gamlss_fits_stats.csv"
  out_err_folder <- "~/Documents/Grad_School/BGDLab/ABCD_data/gamlss_fits/messages/"
}

models_list <-  list.files(models_folder, pattern = "\\.rds$", full.names = TRUE)

#rename some of the variables
rename_vec <- c(age = "PCW_at_scan", sex = "sex_baseline", site = "site_id_l", twin = "twin_statusFAM")

df <- read.csv(data_filename) %>% select(-1) %>% rename(., all_of(rename_vec))
df$sex <- as.factor(df$sex)
df$site <- as.factor(df$site)
df$age <- as.numeric(df$age)

n <- length(models_list)

for (i in (n-1):n) {

model <- readRDS(models_list[i])

print(paste(Reduce(paste, deparse(model$mu.formula)),model$fam, sep = "__"))

df <- df %>% select(sex, age, site, twin, model$phenotype)

modelname <- paste("gamlss_FIT",sub(".*/gamlss_model_(.*)", "\\1", models_list[i]), sep = "_")
filename <- paste0(out_folder,modelname)
  
tryCatch(
    {
          gamlss <- gamlss (formula = model$mu.formula,
                            sigma.formula = model$sigma.formula,
                            tau.formula = model$tau.formula,
                            family = model$fam,
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
      
    }, 
    error = function(e) {
      # Write error message to output file
      writeLines(paste0("Model ", modelname, " ", model$fam, " crashed.\n", geterrmessage()), paste0(out_err_folder, "ERROR_", modelname, "_output.txt"))
    }
  )

}
  