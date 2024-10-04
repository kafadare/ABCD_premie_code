#Load libraries
rm(list=ls())
library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(ggplot2)

#Get the phenotype set and save it:
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/")

#Get the set of phenotypes
vars <- readRDS("ABCD_premie_code/vars_to_save_ABCD.rds")

phenotype_set <- grep("^smri(?!.*(mean|sum))", vars, value = TRUE, perl = TRUE)
saveRDS(phenotype_set, file = "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/ABCD_premie_code/phenotype_set.rds")
ph_exclude <- c("smri_vol_scs_inflatventlh", "smri_vol_scs_lesionlh", "smri_vol_scs_wmintlh",
                "smri_vol_scs_inflatventrh", "smri_vol_scs_lesionrh", "smri_vol_scs_wmintrh",
                 "smri_vol_scs_wmint")
phenotype_set <- setdiff(phenotype_set, ph_exclude)

out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_allPhen/"

age_formulas = c("ns(age, 2)")

covars = c("sex")

opt_covars = c("nonSingleton")

random_fx = c("random(site)")

family_set =  c("GG")

#for family selection: all 3 parameter families, sans RGE because that one doesn't fit.
#family_set <- c("GG", "exGAUS", "TF", "PE", "PE2", "BCCG", "GIG", "LNO", "NOF")

## This is a function to save gamlss models to loop through, each saved as an .rds file.
generate_gamlss_models <- function(phenotype_set, out_folder, family_set =  c("GG"), age_formulas = c("ns(age, 2)"), 
                                   covars = c("sex"),opt_covars = NULL, random_fx = c("random(site)"), n_crit = 200) {

  #models without optional covars
  for (i in 1:length(family_set)) {
    for (j in 1:length(age_formulas)) {
      for (k in 1:length(phenotype_set)) {
        family <- family_set[i]
        age_term <- age_formulas[j]
        ph <- phenotype_set[k]
        model_string <-paste0(ph, "~1+", paste(age_term, paste(covars, collapse = "+"),random_fx, sep = "+"))
        formula_mu <-as.formula(model_string)
        formula_sigma <-as.formula(model_string)
        model_specs <- list(
          mu.formula = formula_mu,
          sigma.formula = formula_sigma,
          tau.formula = as.formula("~1"),
          fam = family,
          n_crit = n_crit,
          phenotype = ph)
        modelname <- paste("gamlss_model",model_string,"cycles",as.character(n_crit),"family", family, sep = "_")
        filename <- paste0(out_folder,modelname,".rds")
        saveRDS(model_specs, file = filename)
        rm(model_specs, modelname, filename)
      }
    }
  } 


  #loop through generating models with the optional covariates. 
  #Right now will include all optional covariates, but if we want to include one by one can wrap this in a for loop too. 
  if (!is.null(opt_covars)){
    #models with twin status
    for (i in 1:length(family_set)) {
      for (j in 1:length(age_formulas)) {
        for (k in 1:length(phenotype_set)) {
          family <- family_set[i]
          age_term <- age_formulas[j]
          ph <- phenotype_set[k]
          model_string <-paste0(ph, "~1+", paste(age_term, paste(covars, collapse = "+"),
                                                 paste(opt_covars, collapse = "+"),random_fx, sep = "+"))
          formula_mu <-as.formula(model_string)
          formula_sigma <-as.formula(model_string)
          model_specs <- list(
            mu.formula = formula_mu,
            sigma.formula = formula_sigma,
            tau.formula = as.formula("~1"),
            fam = family,
            n_crit = n_crit,
            phenotype = ph)
          modelname <- paste("gamlss_model",model_string,"cycles",as.character(n_crit),"family", family,sep = "_")
          filename <- paste0(out_folder,modelname,".rds")
          saveRDS(model_specs, file = filename)
          rm(model_specs, modelname, filename)
        }
      }
    }
  }
  

}#end of function

generate_gamlss_models(phenotype_set = phenotype_set, out_folder = out_folder, 
                       family_set = family_set, age_formulas = age_formulas, covars = covars,
                       opt_covars=opt_covars, random_fx = random_fx)
