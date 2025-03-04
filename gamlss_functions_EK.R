## This is a function to save gamlss models to loop through, each saved as an .rds file.
## Input args are vectors for family, mu terms, sigma terms, nu terms.
generate_gamlss_models <- function(phenotype_set, 
                                   out_folder, 
                                   family_set =  c("GG"),
                                   mu_terms = c("~1", "age", "sex"),
                                   sigma_terms = c("~1","age", "sex"),
                                   nu_terms = c("~1"),
                                   tau_terms = NULL,
                                   long = FALSE,
                                   fixed_terms = FALSE,
                                   n_crit = 200) {
  setwd(out_folder)
#looping through terms
    for (i in 1:length(family_set)) {
      family <- family_set[i]
      print(family)
        for (j in 1:length(phenotype_set)) {
          ph <- phenotype_set[j]
          print(ph)
          n = 0
          #if generating specifications for longitudinal models, add phenotype from t1 to the terms
          if (fixed_terms == TRUE){
            #Use all terms specified in the function input, this is to generate one type of model per phenotype/family. Not for term selection.
            if(long == TRUE){
              ph.t1 <- ph %>% gsub(".t2", ".t1", .)
              mu.terms <- c(mu_terms, ph.t1) %>% {c(head(., 1), tail(., 1), .[-c(1, length(.))])}
            }else{
              mu.terms <- mu_terms
            }
            mu_combinations <- as.data.frame(paste(mu.terms, collapse = " + "))
            sigma_combinations <- as.data.frame(paste(sigma_terms, collapse = " + "))
            nu_combinations <- as.data.frame(paste(nu_terms, collapse = " + "))
            if(!is.null(tau_terms)){
              tau_combinations <- as.data.frame(paste(tau_terms, collapse = " + "))
            }
          }
          else if (fixed_terms == FALSE){
            if (long == TRUE){
              ph.t1 <- ph %>% gsub(".t2", ".t1", .)
              mu.terms <- c(mu_terms, ph.t1) %>% {c(head(., 1), tail(., 1), .[-c(1, length(.))])}
              sigma.terms <- c(sigma_terms, ph.t1) %>% {c(head(., 1), tail(., 1), .[-c(1, length(.))])}
              nu.terms <- c(nu_terms, ph.t1) %>% {c(head(., 1), tail(., 1), .[-c(1, length(.))])}
            } else{
              mu.terms <- mu_terms
              sigma.terms <- sigma_terms
              nu.terms <- nu_terms
            }
          #Generate combinations of mu terms
          mu_combinations <- rbindlist(lapply(1:length(mu.terms), function(k) {
            comb <- combn(mu.terms, k, simplify = FALSE)
            data.frame(combination = sapply(comb, paste, collapse = " + "))  # Combine into a data frame
          })) 
          mu_combinations <- mu_combinations[grep("random(site)",mu_combinations$combination, fixed = TRUE),]
          mu_combinations <- mu_combinations[grep("~1",mu_combinations$combination, fixed = TRUE),]
          if (long == TRUE){
            mu_combinations <- mu_combinations[grep(ph.t1,mu_combinations$combination, fixed = TRUE),]
          }
          #Generate combinations of sigma terms
          sigma_combinations <- rbindlist(lapply(1:length(sigma.terms), function(k) {
            comb <- combn(sigma.terms, k, simplify = FALSE)
            data.frame(combination = sapply(comb, paste, collapse = " + "))  # Combine into a data frame
          })) 
          sigma_combinations <- sigma_combinations[grep("~1",sigma_combinations$combination, fixed = TRUE),]
          #Generate combinations of nu terms
          nu_combinations <- rbindlist(lapply(1:length(nu.terms), function(k) {
            comb <- combn(nu.terms, k, simplify = FALSE)
            data.frame(combination = sapply(comb, paste, collapse = " + "))  # Combine into a data frame
          })) 
          nu_combinations <- nu_combinations[grep("~1",nu_combinations$combination, fixed = TRUE),]
          if (!is.null(tau.terms)){
            #Generate combinations of tau terms
            tau_combinations <- rbindlist(lapply(1:length(tau.terms), function(k) {
              comb <- combn(tau.terms, k, simplify = FALSE)
              data.frame(combination = sapply(comb, paste, collapse = " + "))  # Combine into a data frame
            })) 
            tau_combinations <- tau_combinations[grep("~1",tau_combinations$combination, fixed = TRUE),]
            }
          }
          for (k in 1:dim(mu_combinations)[1]) {
            mu_model_string <- paste0(ph, as.character(mu_combinations[k, 1]))
            #print(mu_model_string)
            for (l in 1:dim(sigma_combinations)[1]) {
              sigma_model_string <- paste0(ph, as.character(sigma_combinations[l, 1]))
              if(!all(strsplit(sigma_model_string, " \\+ ")[[1]] %in% strsplit(mu_model_string, " \\+ ")[[1]])){
                #print(mu_model_string)
                #print(sigma_model_string)
                break
              }
              for (m in 1: dim(nu_combinations)[1]){
                nu_model_string <- paste0(ph,as.character(nu_combinations[m, 1]))
                if(!all(strsplit(nu_model_string, " \\+ ")[[1]] %in% strsplit(sigma_model_string, " \\+ ")[[1]])){
                  #print(sigma_model_string)
                  #print(nu_model_string)
                  break
                }
                #check if fourth moment, tau has terms
                if (!is.null(tau_terms)){
                  for (p in 1: dim(tau_combinations)[1]){
                  tau_model_string <- paste0(ph,as.character(tau_combinations[p, 1]))
                  if(!all(strsplit(tau_model_string, " \\+ ")[[1]] %in% strsplit(nu_model_string, " \\+ ")[[1]])){
                    #print(nu_model_string)
                    #print(tau_model_string)
                    break
                  }
                    #convert strings to formulas
                    formula_mu <-as.formula(mu_model_string)#
                    formula_sigma <-as.formula(sigma_model_string)
                    formula_nu <- as.formula(nu_model_string)
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
                    n = n + 1
                    modelname <- paste("gamlss_model",n, 
                                       family_set[i],
                                       phenotype_set[j], sep = "_")
                    
                    print(paste0("saving model: ", modelname))
                    filename <- paste0(modelname,".rds")
                    saveRDS(model_specs, file = filename)
                    rm(filename,model_specs, modelname)
                  }#end of  loop p
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
                  n  = n + 1
                  modelname <- paste("gamlss_model",n, 
                                     family_set[i],
                                     phenotype_set[j], sep = "_")

                  print(paste0("saving model: ", modelname))
                  filename <- paste0(modelname,".rds")
                  saveRDS(model_specs, file = filename)
                  rm(filename,model_specs, modelname)
            }#end of else
           }#end of loop m
         }#end of loop l
       }#end of loop k
    }#end of loop j
  }#end of loop i
}#end of fct brackets
