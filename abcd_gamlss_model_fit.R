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
  #method <- args[6]
}else{
  n = 1
  models_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_linear"
  data_filename <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-09-20.csv"
  out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen/"
  output_file <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_allPhen/gamlss_fits_stats_allPhen"
  #method <- quote(CG)
}

#save individual .csv stats file for each model
#the wrapper shell script combines them at the end to avoid over-writing the same .csv file when parallel processes are running.
output_file <- paste0(output_file,"_", n, ".csv")

#rename some of the variables and select baseline scans
rename_vec <- c(age = "PCW_at_scan", sex = "sex_baseline", site = "site_id_l", nonSingleton = "nonSingleton")

#read in the dataframe and set variable types, subset for only baseline data
#subset for age (PCW at scan) not being NA (will be NA is gestAge was NA, should be N = 66)
#subset for nonSingleton not being NA(will be NA if the parent questionnaire was NA, should be N = 2)
#subset for sex %in% M or F, so no I in the model (should be N = 3 for I)
df <- read.csv(data_filename)
if (sum(names(df) %in% c("PCW_at_scan", "sex_baseline", "site_id_l", "eventname")) == 4){
df <- df %>% select(-1) %>% rename(., all_of(rename_vec)) %>% 
  subset(eventname == "baseline_year_1_arm_1" & !is.na(age) & !is.na(nonSingleton) & (sex %in% c("M", "F")))
} else{
  df <- df %>% select(-1) %>%
    subset(!is.na(age) & !is.na(nonSingleton) & (sex %in% c("M", "F")))#4512 folks
}
df$sex <- as.factor(df$sex)
df$site <- as.factor(df$site)
df$age <- as.numeric(df$age)

#get list of models and choose the one specified
models_list <-  list.files(models_folder, pattern = "\\.rds$", full.names = TRUE)
print(models_list[n])
model <- readRDS(models_list[n])
#model_name <- paste(Reduce(paste, deparse(model$mu.formula)),model$fam, sep = "__")
print(paste0("family:", model$fam))
print(paste0("mu_formula:",Reduce(paste, deparse(model$mu.formula))))
print(paste0("sigma_formula:",Reduce(paste, deparse(model$sigma.formula))))
print(paste0("nu_formula:",Reduce(paste, deparse(model$nu.formula))))

#nonSingleton <- ifelse(sum(grepl("nonSingleton", as.character(model$mu.formula))) != 0, "nonSingleton", NA)

#select variables for gamlss, including phenotype specified by the loaded model
if (sum(names(df) %in% c("PCW_between_t1t2")) == 1){
  ph.t1 <- model$phenotype %>% gsub(".t2", ".t1", .)
  df <- df %>% 
    select(sex, age, site, nonSingleton, model$phenotype, PCW_between_t1t2, ph.t1) %>% 
    drop_na()
} else {
  df <- df %>% 
    select(sex, age, site, nonSingleton, model$phenotype) %>% 
    drop_na()
}

#modelname <- paste("gamlss_FIT",sub(".*/gamlss_model_(.*)", "\\1", model_name), sep = "__")
#modelname <- paste("gamlssFIT", model$fam, model$phenotype, nonSingleton, sep = "__")
modelname <- (paste("gamlssFIT", Reduce(paste, deparse(model$mu.formula)), 
                    Reduce(paste,deparse(model$sigma.formula)),
                    Reduce(paste,deparse(model$nu.formula)),
                    model$fam, sep = "_"))

gamlss <- gamlss (formula = model$mu.formula,
                  sigma.formula = model$sigma.formula,
                  nu.formula = model$nu.formula,
                  family = model$fam,
                  method = RS(),
                  data = df,
                  control = gamlss.control(n.cyc = eval(model$n_crit)))

output <- data.frame(mu_formula = Reduce(paste, deparse(model$mu.formula)),
                     sigma_formula = Reduce(paste, deparse(model$sigma.formula)),
                     nu_formula = Reduce(paste, deparse(model$nu.formula)),
                     family_abbr = gamlss$family[1],
                     family = gamlss$family[2],
                     method = as.character(gamlss$method),
                     logLik = logLik(gamlss), 
                     AIC = AIC(gamlss), 
                     BIC = BIC(gamlss),
                     n_cyc = model$n_crit,
                     data = basename(data_filename),
                     warning = NA)

#Create new table for each model, will be combined in wrapper shell script when array is done.
write.table(output, file = output_file, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
print(paste0("Creating new stats table: ", output_file))

#Old code -- when for writing into the same .csv file, not good when running parallel processes.
# if (!file.exists(output_file)) {
#   write.table(output, file = output_file, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
#   print(paste0("Creating new stats table: ", output_file))
# } else {
#   write.table(output, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
#   print("New line in fits stats table.")
# }
#   

filename <- paste0(out_folder,modelname,".rds")
#filename <- paste0(out_folder,modelname,".RData")
#Saving the model only
print(filename)
saveRDS(gamlss, file = filename)
#if want to save the whole environment ... 
#rm(args, output_file, rename_vec, models_list, nonSingleton, modelname, output)
#save.image(file=filename)
