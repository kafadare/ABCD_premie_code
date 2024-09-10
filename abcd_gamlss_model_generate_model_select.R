## This is a script to save gamlss models to loop through, each saved as an .rds file.

#We want to do family selection. We will choose between three 3-parameter families.
#Box-Cox Cole and Green, Generalized Gamma, Generalized Inverse Gaussian, Exponential Gaussian, Power Exponential

out_folder <- out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_all_phen/"

#Using the generalized gamma distribution family
family_set <- c("GG")

age_formulas <- c("ns(age, 2)")

#smri_vol_cdk_total is for total cortical GM, this is from the Desikan parcellation. The rest are from Aseg parcellation. 
#Choose which regional and global phenotypes to run through. left/right separate or not?
data_filename <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-07-15.csv"
#rename some of the variables and select baseline scans
rename_vec <- c(age = "PCW_at_scan", sex = "sex_baseline", site = "site_id_l", twin = "twin_statusFAM")
#read in the dataframe and set variable types
df <- read.csv(data_filename) %>% select(-1) %>% rename(., all_of(rename_vec)) %>% subset(eventname == "baseline_year_1_arm_1")#9396 subjects!

#specify the set of phenotypes to fit models to
#might want to specify only total bilateral volumes. they include "total". Should check if there are any columns that include smri but don't include total in the var name that I want to include.
phenotype_set <- c(names(abcd_long)[grep("smri", names(abcd_long))], "totalWM_cb", "totalWM_crb", "totalGM_crb")

#read in the dataframe and set variable types
data_filename <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-07-15.csv"
df <- read.csv(data_filename) %>% select(-1) %>% rename(., all_of(rename_vec)) %>% subset(eventname == "baseline_year_1_arm_1")#9396 subjects!
df$sex <- as.factor(df$sex)
df$site <- as.factor(df$site)
df$age <- as.numeric(df$age)

n_crit = 200
#models without twin status
for (i in 1:length(family_set)) {
  for (j in 1:length(age_formulas)) {
    for (k in 1:length(phenotype_set)) {
      family <- family_set[i]
      age_term <- age_formulas[j]
      ph <- phenotype_set[k]
      model_specs <- list(
        mu.formula = as.formula(paste0(ph, " ~ 1 + sex + ",age_term," + random(site)")),
        sigma.formula = as.formula(paste0(ph, " ~ 1 + sex + ",age_term," + random(site)")),
        tau.formula = as.formula("~1"),
        fam = family,
        n_crit = n_crit,
        phenotype = ph)
      modelname <- paste("gamlss_model",ph,family,age_term,"cycles",as.character(n_crit),sep = "_")
      filename <- paste0(out_folder,modelname,".rds")
      saveRDS(model_specs, file = filename)
      rm(model_specs, modelname, filename)
    }
  }
}

#models with twin status
for (i in 1:length(family_set)) {
  for (j in 1:length(age_formulas)) {
    for (k in 1:length(phenotype_set)) {
      family <- family_set[i]
      age_term <- age_formulas[j]
      ph <- phenotype_set[k]
      model_specs <- list(
        mu.formula = as.formula(paste0(ph, " ~ 1 + sex + ",age_term," + twin + random(site)")),
        sigma.formula = as.formula(paste0(ph, " ~ 1 + sex + ",age_term," + twin + random(site)")),
        tau.formula = as.formula("~1"),
        fam = family,
        n_crit = n_crit,
        phenotype = ph)
      modelname <- paste("gamlss_model",ph,family,age_term,"cycles",as.character(n_crit), "TwinVar", sep = "_")
      filename <- paste0(out_folder,modelname,".rds")
      saveRDS(model_specs, file = filename)
      rm(model_specs, modelname, filename)
    }
  }
}
