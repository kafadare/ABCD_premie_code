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
          n = 0
          #if generating specifications for longitudinal models, add phenotype from t1 to the terms
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
          #Generate combinations of terms
          mu_combinations <- rbindlist(lapply(1:length(mu.terms), function(k) {
            comb <- combn(mu.terms, k, simplify = FALSE)
            data.frame(combination = sapply(comb, paste, collapse = " + "))  # Combine into a data frame
          })) 
          mu_combinations <- mu_combinations[grep("random(site)",mu_combinations$combination, fixed = TRUE),]
          mu_combinations <- mu_combinations[grep("~1",mu_combinations$combination, fixed = TRUE),]
          if (long == TRUE){
            mu_combinations <- mu_combinations[grep(ph.t1,mu_combinations$combination, fixed = TRUE),]
          }
          #Generate combinations of terms
          sigma_combinations <- rbindlist(lapply(1:length(sigma.terms), function(k) {
            comb <- combn(sigma.terms, k, simplify = FALSE)
            data.frame(combination = sapply(comb, paste, collapse = " + "))  # Combine into a data frame
          })) 
          #sigma_combinations <- sigma_combinations[grep("random(site)",sigma_combinations$combination, fixed = TRUE),]
          sigma_combinations <- sigma_combinations[grep("~1",sigma_combinations$combination, fixed = TRUE),]
          # if (long == TRUE){
          #   sigma_combinations <- sigma_combinations[grep(ph.t1,sigma_combinations$combination, fixed = TRUE),]
          # }
          #Generate combinations of terms
          nu_combinations <- rbindlist(lapply(1:length(nu.terms), function(k) {
            comb <- combn(nu.terms, k, simplify = FALSE)
            data.frame(combination = sapply(comb, paste, collapse = " + "))  # Combine into a data frame
          })) 
          #nu_combinations <- nu_combinations[grep("random(site)",nu_combinations$combination, fixed = TRUE),]
          nu_combinations <- nu_combinations[grep("~1",nu_combinations$combination, fixed = TRUE),]
          # if (long == TRUE){
          #   nu_combinations <- nu_combinations[grep(ph.t1,nu_combinations$combination, fixed = TRUE),]
          # }
          
          for (k in 1:dim(mu_combinations)[1]) {
            mu_model_string <- paste0(ph, as.character(mu_combinations[k, "combination"]))
            
            for (l in 1:dim(sigma_combinations)[1]) {
              sigma_model_string <- paste0(ph, as.character(sigma_combinations[l, "combination"]))
              if(!all(strsplit(sigma_model_string, " \\+ ")[[1]] %in% strsplit(mu_model_string, " \\+ ")[[1]])){
                #print(mu_model_string)
                #print(sigma_model_string)
                break
              }
              
              for (m in 1: dim(nu_combinations)[1]){
                nu_model_string <- paste0(ph,as.character(nu_combinations[m, "combination"]))
                if(!all(strsplit(nu_model_string, " \\+ ")[[1]] %in% strsplit(sigma_model_string, " \\+ ")[[1]])){
                  #print(sigma_model_string)
                  #print(nu_model_string)
                  break
                }
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
                  # if(long == TRUE){
                  #   mu.names <- mu_combinations$combination %>% sub(" [^ ]*\\.t1", " t1", .)
                  #   sigma.names <- sigma_combinations$combination %>% sub(" [^ ]*\\.t1", " t1", .)
                  #   nu.names <- nu_combinations$combination %>% sub(" [^ ]*\\.t1", " t1", .)
                  # }
                  #define model name to save
                  n = n + 1
                  modelname <- paste("gamlss_model",n, 
                                     family_set[i],
                                     phenotype_set[j], sep = "_")

                      print("saving models")
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
