## This is a script to save gamlss models to loop through, each saved as an .rds file.

#We want to do family selection. We will choose between three 3-parameter families.
#Box-Cox Cole and Green, Generalized Gamma, Generalized Inverse Gaussian, Exponential Gaussian, Power Exponential

out_folder <- out_folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_models_family/"

#Only testing 3 parameter models.
#Up to BNB fits were included in comparison in the braincharts paper. Rest might have not converged and/or response variable constraints.
family_set <- c("GG", "ST2", "ST3", "ST1", "exGAUS", "ST4", "TF", "PE", "PE2", "DBURR12", "BNB",
                 "BCCG", "DEL", "GIG", "LNO", "NBF", "NET", "NOF", "RGE", "SI",
                "SICHEL", "ST5")

 
age_formulas <- c("ns(age, 2)")

#smri_vol_cdk_total is for total cortical GM, this is from the Desikan parcellation. The rest are from Aseg parcellation. 
phenotype_set <- c("smri_vol_cdk_total", "totalWM_cb","smri_vol_scs_subcorticalgv",
                   "smri_vol_scs_allventricles", "smri_vol_scs_wholeb")


#total whole brain cortical volume (desikan), wotal cerebral cortex WM (aseg), all ventricles (aseg), intracranial volume (aseg), whole brain vol (aseg)

n_crit = 200

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
