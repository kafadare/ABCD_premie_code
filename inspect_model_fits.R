#Load libraries
#rm(list=ls())
library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(ggplot2)
library(gamlss) #to fit model

inspect_model_fits <- function(folder, fits_stats_filename, save = FALSE){
fits_stats <- read.csv(fits_stats_filename)
fits_output_folder <- paste0(folder,"out_messages/")
fits_stats$modelFam <- paste(fits_stats$mu_formula, 
                             fits_stats$sigma_formula, 
                             fits_stats$nu_formula, 
                             fits_stats$family_abbr, sep = "__")
#fits_stats$warning <- NULL

#get list of models and choose the one specified
output_list <-  list.files(fits_output_folder, pattern = "\\.txt$", full.names = TRUE)
fits_outputs <- as.data.frame(do.call(rbind, lapply(output_list, read_file))) %>% rename(., text = V1)
#fits_outputs$model <- lapply(fits_outputs$text, str_extract, pattern = "\\[1\\].*") %>% 
#sub("\\[1\\] \\\"","",.) %>% sub("__.*", "", .)
fits_outputs$model_index <- sapply(output_list, function(x) sub(".*?(\\d+)_output\\.txt$", "\\1", x))
fits_outputs$mu_formula <- lapply(fits_outputs$text, str_extract, pattern = "(?<=mu_formula:).*") %>% 
  gsub('[\\"\\\\]','',.)
fits_outputs$sigma_formula <- lapply(fits_outputs$text, str_extract, pattern = "(?<=sigma_formula:).*")%>% 
  gsub('[\\"\\\\]','',.)
fits_outputs$nu_formula <- lapply(fits_outputs$text, str_extract, pattern = "(?<=nu_formula:).*")%>% 
  gsub('[\\"\\\\]','',.)
fits_outputs$family_abbr <- lapply(fits_outputs$text, str_extract, pattern = "(?<=family:).*") %>% 
  gsub('[\\"\\\\]','',.) %>% gsub("__","",.)
fits_outputs$modelFam <- paste(fits_outputs$mu_formula,
                               fits_outputs$sigma_formula,
                               fits_outputs$nu_formula,
                               fits_outputs$family, sep = "__")

fits_outputs$phenotype <- as.character(lapply(fits_outputs$mu_formula, str_extract, pattern = ".*(?= ~)"))

fits_outputs$warning <- as.integer(lapply(fits_outputs$text, function(x) {sum(grepl(pattern = "Warning",x))}))
print(paste0("Number of fits with warnings: ", sum(fits_outputs$warning)))
fits_outputs$error <- as.integer(lapply(fits_outputs$text, function(x) {sum(grepl(pattern = "Error",x))}))
print(paste0("Number of fits with errors: ", sum(fits_outputs$error)))

#save warning text
fits_outputs$warning_text <- lapply(fits_outputs$text, str_extract_all, pattern = "Warning((?:.*\\n){0,2})")
fits_outputs$warning_text <- as.character(lapply(fits_outputs$warning_text, unlist))

#save error text
fits_outputs$error_text <- lapply(fits_outputs$text, str_extract_all, pattern = "Error((?:.*\\n){0,5})")
fits_outputs$error_text <- as.character(lapply(fits_outputs$error_text, unlist))

#find whether there is a convergence warning, and whether this is at the end of the fitting procedure (which implies there is no convergence once fitting is over.)
fits_outputs$convergence_warning <- as.integer(lapply(fits_outputs$text, function(x) {sum(grepl(pattern = "converg",x))}))
print(paste0("Number of fits with Convergence warnings: ", sum(fits_outputs$convergence_warning)))
fits_outputs$convergence_warn_end <- as.integer(lapply(fits_outputs$text, function(x) {sum(grepl(pattern = "Algorithm \\w{2} has not yet converged",x))}))#75
print(paste0("Number of fits with Convergence warnings at the END of fitting: ", sum(fits_outputs$convergence_warn_end)))

#Which models stats not saved
empty_fits <- fits_outputs$modelFam[which(!(fits_outputs$modelFam %in% fits_stats$modelFam))]
print(paste0("Number of models that have not saved in the stats table: ", length(empty_fits)))
print(paste0("Models: ", empty_fits))
#separate models with error:
error_fits <- subset(fits_outputs, error == 1)

#look at empty fits without an error
notSave_fits <- empty_fits[!(empty_fits %in% error_fits$modelFam)] #0
print(paste0("Number of models that have not saved but do NOT have error: ", length(notSave_fits)))
#print(paste0("Output text for models that did not save without error: ",fits_outputs[which(fits_outputs$modelFam %in% notSave_fits),"text"]))

##Save output file that has been constructed with errors and warnings and separate file with error fits
if(save == TRUE){
write.csv(fits_outputs, paste0(folder,"fits_outputs.csv"), row.names = F)
write.csv(error_fits, paste0(folder, "errors_fits.csv"), row.names = F)
}

#merge fits_stats with fits_outputs
fits <- merge(fits_stats, fits_outputs, by = c("mu_formula", 
                                               "sigma_formula", 
                                               "nu_formula", 
                                               "family_abbr", 
                                               "modelFam"), all.x = TRUE, all.y = TRUE)

return(fits)

} ## end of function
