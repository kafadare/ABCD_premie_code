## This is a function to save gamlss models to loop through, each saved as an .rds file.
## Input args are vectors for family, mu terms, sigma terms, nu terms.
generate_gamlss_models <- function(phenotype_set, 
                                   out_folder, 
                                   family_set =  c("GG"),
                                   mu_terms = c("~1", "age", "sex"),
                                   sigma_terms = c("~1","age", "sex"),
                                   nu_terms = c("~1"),
                                   long = FALSE,
                                   tau_terms = NULL,
                                   n_crit = 200) {
  setwd(out_folder)
#looping through terms
    for (i in 1:length(family_set)) {
      family <- family_set[i]
        for (j in 1:length(phenotype_set)) {
          ph <- phenotype_set[j]
          #if generating specifications for longitudinal models, add phenotype from t1 to the terms
          if (long == TRUE){
            ph.t1 <- ph %>% gsub(".t2", ".t1", .)
            mu.terms <- c(mu_terms, ph.t1)
            sigma.terms <- c(sigma_terms, ph.t1)
            nu.terms <- c(nu_terms, ph.t1)
          }
          for (k in 1:length(mu.terms)) {
            mu_model_string <- paste0(ph, paste(mu.terms[1:k], collapse = "+"))
            
            for (l in 1:length(sigma.terms)) {
              if(l>k){
                l = k
              }
              sigma_model_string <- paste0(ph, paste(sigma.terms[1:l], collapse = "+"))
              for (m in 1: length(nu.terms)){
                if(m>l){
                  m = l
                }
                nu_model_string <- paste0(ph, paste(nu.terms[1:m], collapse = "+"))
                #check if fourth moment, tau has terms
                if (!is.null(tau_terms)){
                  for (n in 1: length(nu.terms)){
                    if(n>m){
                      n = m
                    }
                    tau_model_string <- paste0(ph, paste(tau_terms[1:n], collapse = "+"))
                    #convert strings to formulas
                    formula_mu <-as.formula(mu_model_string)
                    formula_sigma <-as.formula(sigma_model_string)
                    formula_nu <- as.formula(nu_model__string)
                    formula_tau <- as.formula(tau_model_string)
                    #define the model specifications
                    model_specs <- list(
                      mu.formula = formula_mu,
                      sigma.formula = formula_sigma,
                      nu.formula = formula_nu,
                      tau.formula = formula_tau,
                      fam = family,
                      n_crit = n_crit,
                      phenotype = ph)
                    modelname <- paste("gamlss_model",
                                       "MU", paste(mu.terms[1:k], collapse = "."),
                                       "SIGMA", paste(sigma.terms[1:l], collapse = "."),
                                       "NU", paste(nu.terms[1:m], collapse = "."), 
                                       "TAU", paste(tau_terms[1:n], collapse = "."),
                                       family_set[i],
                                       phenotype_set[j], sep = "_")
                    filename <- paste0(out_folder,modelname,".rds")
                    saveRDS(model_specs, file = filename)
                    rm(model_specs, modelname, filename)
                  }#end of  loop n
                }else{
                  #convert strings to formulas
                  formula_mu <-as.formula(mu_model_string)
                  formula_sigma <-as.formula(sigma_model_string)
                  formula_nu <- as.formula(nu_model_string)
                  #define the model specifications
                  model_specs <- list(
                    mu.formula = formula_mu,
                    sigma.formula = formula_sigma,
                    nu.formula = formula_nu,
                    fam = family,
                    n_crit = n_crit,
                    phenotype = ph)
                  #if generating specifications for longitudinal models, change the name of the term to "t1"
                  #to avoid "file name too long" error
                  if(long == TRUE){
                    mu.names <- mu.terms %>% sub(".*\\.t1$", "t1", .)
                    sigma.names <- sigma.terms %>% sub(".*\\.t1$", "t1", .)
                    nu.names <- nu.terms %>% sub(".*\\.t1$", "t1", .)
                  }
                  #define model name to save
                  modelname <- paste("gamlss_model",
                                     "MU", paste(mu.names[1:k], collapse = "."),
                                     "SIGMA", paste(sigma.names[1:l], collapse = "."),
                                     "NU", paste(nu.names[1:m], collapse = "."), 
                                     family_set[i],
                                     phenotype_set[j], sep = "_")
                  #do not save model if there is no random(site) effect in the mu term
                  #do not save if generating longitudinal models and there is no t1 phenotype in MU
                  if(long == TRUE){
                    if(grepl("MU.*\\.t1.*SIGMA", modelname)){
                      if(grepl("MU.*site.*SIGMA", modelname)){
                        print("saving longitudinal models")
                        filename <- paste0(modelname,".rds")
                        saveRDS(model_specs, file = filename)
                        rm(filename)
                      }
                    }
                  } else if (long == FALSE){
                    if(grepl("MU.*site.*SIGMA", modelname)){
                      print("saving non-longitudinal models")
                      filename <- paste0(modelname,".rds")
                      saveRDS(model_specs, file = filename)
                      rm(filename)
                    }
                  }
                  rm(model_specs, modelname)
            }#end of else
           }#end of loop m
         }#end of loop l
       }#end of loop k
    }#end of loop j
  }#end of loop i
}#end of fct brackets
