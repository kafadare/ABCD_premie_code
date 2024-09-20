#Get the phenotype set and save it:
setwd("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/")

#Get the set of phenotypes
vars <- readRDS("ABCD_premie_code/vars_to_save_ABCD.rds")

phenotype_set <- grep("^smri(?!.*(mean|sum))", vars, value = TRUE, perl = TRUE)

out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_allPhen/"

age_formulas = c("ns(age, 2)")

covars = c("sex")

opt_covars = c("nonSingleton")

random_fx = c("random(site)")

family_set =  c("GG")

generate_gamlss_models(phenotype_set, out_folder, family_set, age_formulas, covars, opt_covars, random_fx)

## This is a function to save gamlss models to loop through, each saved as an .rds file.
generate_gamlss_models <- function(phenotype_set, out_folder, family_set =  c("GG"), age_formulas = c("ns(age, 2)"), covars = c("sex"), 
                                   opt_covars = NULL, random_fx = c("random(site)"), n_crit = 200) {

  ph <- phenotype_set[1]
  age_term <- age_formulas[1]
  family <- family_set[1]
  model_string <-paste0(ph, "~1+", paste(age_term, paste(covars, collapse = "+"),random_fx, sep = "+"))
  formula_mu <-as.formula(model_string)
  formula_sigma <-as.formula(model_string)
  
  #models without optional covars
  for (i in 1:length(family_set)) {
    for (j in 1:length(age_formulas)) {
      for (k in 1:length(phenotype_set)) {
        family <- family_set[i]
        age_term <- age_formulas[j]
        ph <- phenotype_set[k]
        model_specs <- list(
          mu.formula = formula_mu,
          sigma.formula = formula_sigma,
          tau.formula = as.formula("~1"),
          fam = family,
          n_crit = n_crit,
          phenotype = ph)
        modelname <- paste("gamlss_model",model_string,"cycles",as.character(n_crit),sep = "_")
        filename <- paste0(out_folder,modelname,".rds")
        saveRDS(model_specs, file = filename)
        rm(model_specs, modelname, filename)
      }
    }
  } 


  #loop through generating models with the optional covariates. 
  #Right now will include all optional covariates, but if we want to include one by one can wrap this in a for loop too. 
  if (!is.null(opt_covars)){
    model_string <-paste0(ph, "~1+", paste(age_term, paste(covars, collapse = "+"),
                                                        paste(opt_covars, collapse = "+"),random_fx, sep = "+"))
    formula_mu <-as.formula(model_string)
    formula_sigma <-as.formula(model_string)
    
    #models with twin status
    for (i in 1:length(family_set)) {
      for (j in 1:length(age_formulas)) {
        for (k in 1:length(phenotype_set)) {
          family <- family_set[i]
          age_term <- age_formulas[j]
          ph <- phenotype_set[k]
          model_specs <- list(
            mu.formula = formula_mu,
            sigma.formula = formula_sigma,
            tau.formula = as.formula("~1"),
            fam = family,
            n_crit = n_crit,
            phenotype = ph)
          modelname <- paste("gamlss_model",model_string,"cycles",as.character(n_crit),sep = "_")
          filename <- paste0(out_folder,modelname,".rds")
          saveRDS(model_specs, file = filename)
          rm(model_specs, modelname, filename)
        }
      }
    }
  }
  

}#end of function