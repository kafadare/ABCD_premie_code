#Load libraries
rm(list=ls())
library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(ggplot2)
library(gamlss) #to fit model
library(mgcv) # helps with the gam models
library(tidygam) # helps with the gam models

#path for RSE method results
#folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_v2/"
#load family fit stats
#fit_stats <- read.csv("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_v2/gamlss_family_fits_stats_v2.csv")

#paths for CG method results
folder <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_CG/new_fits_singleton/"
#load family fit stats
fit_stats <- read.csv("/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/gamlss_fits_family_CG/new_fits_singleton/gamlss_family_fits_stats_CG_new.csv")

fits_output_folder <- paste0(folder,"out_messages_new/")
fit_stats$modelFam <- paste(fit_stats$model, fit_stats$family_abbr, sep = "__")
fit_stats$warning <- NULL

#get list of models and choose the one specified
output_list <-  list.files(fits_output_folder, pattern = "\\.txt$", full.names = TRUE)
fits_outputs <- as.data.frame(do.call(rbind, lapply(output_list, read_file))) %>% rename(., text = V1)
fits_outputs$model <- lapply(fits_outputs$text, str_extract, pattern = "\\[1\\].*") %>% 
                      sub("\\[1\\] \\\"","",.) %>% sub("__.*", "", .)
fits_outputs$family_abbr <- lapply(fits_outputs$text, str_extract, pattern = "__[a-zA-Z0-9]*") %>% 
                      sub("__","",.)
fits_outputs$modelFam <- paste(fits_outputs$model, fits_outputs$family, sep = "__")
fits_outputs$phenotype <- as.character(lapply(fits_outputs$model, str_extract, pattern = ".*(?= ~)"))

fits_outputs$warning <- as.integer(lapply(fits_outputs$text, function(x) {sum(grepl(pattern = "Warning",x))}))#144
fits_outputs$error <- as.integer(lapply(fits_outputs$text, function(x) {sum(grepl(pattern = "Error",x))}))#49

#examine what kinds of errors
fits_outputs$error_text <- lapply(fits_outputs$text, str_extract_all, pattern = "Error((?:.*\\n){0,5})")
subset(fits_outputs, error == 1)$error_text

#examine what kind of warnings
fits_outputs$warning_text <- lapply(fits_outputs$text, str_extract_all, pattern = "Warning((?:.*\\n){0,2})")
subset(fits_outputs, warning == 1 & error == 0)$warning_text
fits_outputs$convergence_warning <- as.integer(lapply(fits_outputs$text, function(x) {sum(grepl(pattern = "converg",x))}))#130
fits_outputs$convergence_warn_end <- as.integer(lapply(fits_outputs$text, function(x) {sum(grepl(pattern = "Algorithm RS has not yet converged",x))}))#75
subset(fits_outputs, warning == 1 & convergence_warning == 0 & error == 0)$warning_text
###^^body (fun) argument is not a function warning. see below for which families, is there a pattern?
subset(fits_outputs, warning == 1 & convergence_warning == 0 & error == 0)$modelFam #(all LNO)

#Which models stats not saved
empty_fits <- fits_outputs$modelFam[which(!(fits_outputs$modelFam %in% fit_stats$modelFam))]
#which of those have error output
fits_outputs$warning_text <- as.character(lapply(fits_outputs$warning_text, unlist))
fits_outputs$error_text <- as.character(lapply(fits_outputs$error_text, unlist))
error_fits <- subset(fits_outputs, error == 1)
#write.csv(error_fits, paste0(folder, "errors_fits.csv"), row.names = F)

#look at empty fits without an error
sum(empty_fits %in% error_fits$modelFam) #24 - All fits with error haven't been saved (makes sense)
notSave_fits <- empty_fits[!(empty_fits %in% error_fits$modelFam)] #0
fits_outputs[which(fits_outputs$modelFam %in% notSave_fits),"text"] #unsaved due to time limit, 0 in version 2 - adjusted time limit
#write.csv(notSave_fits, paste0(folder,"timeLimit_fits.csv"), row.names = F)

##Save output file that has been constructed with errors and warnings
#write.csv(fits_outputs, paste0(folder,"fits_outputs.csv"), row.names = F)

#merge fit_stats with fits_outputs
fits <- merge(fit_stats, fits_outputs, by = c("model", "family_abbr", "modelFam"))

#filter fits for actually 3 param distributions (after noticing discrepancy between the gamlss 
#R package documentation and gamlss book -- ST distributions are actually 4 parameter,
#so do not include them in the plots/selection here)
fits <- fits %>%
  filter(family_abbr %in% c("GG", "exGAUS", "TF", "PE", "PE2",
                                            "BCCG", "GIG", "LNO", "NOF", "RGE"))


#separate out fits with nonSingleton status as categorical (the most complex model)
fits_nonSingleton <- fits %>% subset(., grepl("nonSingleton", model))#63
#fits without nonSingleton as a variable
fits_NOnonSingleton <- fits %>% subset(., !(grepl("nonSingleton", model)))#63


### Looking at most complex model, which includes "nonSingleton" status.

#how many models of each family available (5 max)
table(fits_nonSingleton$family_abbr)
#family_count_full <- as.data.frame(table(fits_nonSingleton$family_abbr)) %>% filter(Freq == 5) #12 families (exGAUS threw errors for 2/5)

#look at which families don't converge
table(fits_nonSingleton[which(fits_nonSingleton$convergence_warning == 1),"family_abbr"])
table(fits_nonSingleton[which(fits_nonSingleton$convergence_warn_end == 1),"family_abbr"])

##Get AIC and BIC plots

#For all families that fit, regardless of convergence warning. Get mean, max, and min.
#Also show plots by phenotype (currently the df does not have phenotype as a separate value)
#fits_nonSingleton <- fits_nonSingleton[which(fits_nonSingleton$family_abbr %in% family_count_full$Var1),]#65

#Mean AIC and BIC table isn't actually statistically useful to make decisions with
# fits_table_all <- fits_nonSingleton %>%
#               group_by(family_abbr) %>%
#               summarise(
#                 mean_AIC = mean(AIC),
#                 max_AIC = max(AIC),
#                 min_AIC = min(AIC),
#                 mean_BIC = mean(AIC),
#                 max_BIC = max(BIC),
#                 min_BIC = min(BIC)
#                 )
# min(fits_table$mean_BIC)

#separate plots for each of the 5 phenotypes in the models. Regardless of convergence warning.

#BIC_WMV <- 
  fits_nonSingleton %>% filter(phenotype == "totalWM_cb") %>% 
  ggplot(., aes(x = log(BIC - min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("WMV, all") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_GMV <- 
  fits_nonSingleton %>% filter(phenotype == "smri_vol_cdk_total") %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("cort GMV, all") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_scGMV <- 
  fits_nonSingleton %>% filter(phenotype == "smri_vol_scs_subcorticalgv") %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("subcort GMV, all") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_Vent <- 
  fits_nonSingleton %>% filter(phenotype == "smri_vol_scs_allventricles") %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("Ventricles, all") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_wholeB <- 
  fits_nonSingleton %>% filter(phenotype == "smri_vol_scs_wholeb") %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("Whole Brain, all") + labs(x = "scaled log BIC", y = "Distribution family")


#Only without end convergence warning

#BIC_WMV <- 
  fits_nonSingleton %>% filter(phenotype == "totalWM_cb"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("WMV, converged") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_GMV <- 
  fits_nonSingleton %>% filter(phenotype == "smri_vol_cdk_total"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("cort GMV, converged") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_scGMV <- 
  fits_nonSingleton %>% filter(phenotype == "smri_vol_scs_subcorticalgv"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("subcort GMV, converged") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_Vent <- 
  fits_nonSingleton %>% filter(phenotype == "smri_vol_scs_allventricles"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("Ventricles, converged") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_wholeB <- 
  fits_nonSingleton %>% filter(phenotype == "smri_vol_scs_wholeb"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("Whole Brain, converged") + labs(x = "scaled log BIC", y = "Distribution family")


### Looking at model without the nonSingleton status term, since this could be a weird term. 
### The purpose is to investigate if there are significant differences in family selection results when this term is not included.
  
  
#how many models of each family available (5 max)
table(fits_NOnonSingleton$family_abbr)
#family_count_full <- as.data.frame(table(fits_NOnonSingleton$family_abbr)) %>% filter(Freq == 5) #12 families (exGAUS threw errors for 2/5)

#look at which families don't converge
table(fits_NOnonSingleton[which(fits_NOnonSingleton$convergence_warning == 1),"family_abbr"])
table(fits_NOnonSingleton[which(fits_NOnonSingleton$convergence_warn_end == 1),"family_abbr"]) #same # of models give convergence warnings.

##Get AIC and BIC plots

#For all families that fit, regardless of convergence warning. Get mean, max, and min.
#Also show plots by phenotype (currently the df does not have phenotype as a separate value)
#fits_NOnonSingleton <- fits_NOnonSingleton[which(fits_NOnonSingleton$family_abbr %in% family_count_full$Var1),]#65

#Mean AIC and BIC table isn't actually statistically useful to make decisions with
# fits_table_all <- fits_NOnonSingleton %>%
#               group_by(family_abbr) %>%
#               summarise(
#                 mean_AIC = mean(AIC),
#                 max_AIC = max(AIC),
#                 min_AIC = min(AIC),
#                 mean_BIC = mean(AIC),
#                 max_BIC = max(BIC),
#                 min_BIC = min(BIC)
#                 )
# min(fits_table$mean_BIC)

#separate plots for each of the 5 phenotypes in the models. Regardless of convergence warning.

#BIC_WMV <- 
  fits_NOnonSingleton %>% filter(phenotype == "totalWM_cb") %>% 
  ggplot(., aes(x = log(BIC - min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("WMV, all") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_GMV <- 
  fits_NOnonSingleton %>% filter(phenotype == "smri_vol_cdk_total") %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("cort GMV, all") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_scGMV <- 
  fits_NOnonSingleton %>% filter(phenotype == "smri_vol_scs_subcorticalgv") %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("subcort GMV, all") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_Vent <- 
  fits_NOnonSingleton %>% filter(phenotype == "smri_vol_scs_allventricles") %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("Ventricles, all") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_wholeB <- 
  fits_NOnonSingleton %>% filter(phenotype == "smri_vol_scs_wholeb") %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr, fill = convergence_warn_end)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("Whole Brain, all") + labs(x = "scaled log BIC", y = "Distribution family")


#Only without end convergence warning

#BIC_WMV <- 
  fits_NOnonSingleton %>% filter(phenotype == "totalWM_cb"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("WMV, converged") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_GMV <- 
  fits_NOnonSingleton %>% filter(phenotype == "smri_vol_cdk_total"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("cort GMV, converged") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_scGMV <- 
  fits_NOnonSingleton %>% filter(phenotype == "smri_vol_scs_subcorticalgv"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("subcort GMV, converged") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_Vent <- 
  fits_NOnonSingleton %>% filter(phenotype == "smri_vol_scs_allventricles"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("Ventricles, converged") + labs(x = "scaled log BIC", y = "Distribution family")

#BIC_wholeB <- 
  fits_NOnonSingleton %>% filter(phenotype == "smri_vol_scs_wholeb"& convergence_warn_end == 0) %>% 
  ggplot(., aes(x = log(BIC -min(BIC)), y = family_abbr)) + 
  geom_histogram(stat = "identity") + 
  ggtitle("Whole Brain, converged") + labs(x = "scaled log BIC", y = "Distribution family")
  

  
### Get a rank of different families for both nonSingleton and no nonSingleton models.
  ## Assign a rank order within each phenotype and take an average of rank orders across phenotypes.

  #For models that have nonSingleton status
fits_nonSingleton <- fits_nonSingleton %>%
  group_by(phenotype) %>%
  mutate(rank_BIC = rank(BIC, ties.method = "min"))

mean_rank <- fits_nonSingleton %>%
  group_by(family) %>%
  summarize(mean_rank_BIC = mean(rank_BIC)) %>%
  arrange(mean_rank_BIC)

print(mean_rank)

#For models that do NOT include nonSingleton status
fits_NOnonSingleton <- fits_NOnonSingleton %>%
  group_by(phenotype) %>%
  mutate(rank_BIC = rank(BIC, ties.method = "min"))

mean_rank <- fits_NOnonSingleton %>%
  group_by(family) %>%
  summarize(mean_rank_BIC = mean(rank_BIC)) %>%
  arrange(mean_rank_BIC)

print(mean_rank)


## ONLY for models that have converged.

#For models that have nonSingleton status
fits_nonSingleton <- fits_nonSingleton %>% 
  filter(convergence_warn_end == 0) %>%
  group_by(phenotype) %>%
  mutate(rank_BIC = rank(BIC, ties.method = "min"))

mean_rank <- fits_nonSingleton %>%
  group_by(family) %>%
  summarize(mean_rank_BIC = mean(rank_BIC)) %>%
  arrange(mean_rank_BIC)

print(mean_rank)

#For models that do NOT include nonSingleton status
fits_NOnonSingleton <- fits_NOnonSingleton %>%
  filter(convergence_warn_end == 0) %>%
  group_by(phenotype) %>%
  mutate(rank_BIC = rank(BIC, ties.method = "min"))

mean_rank <- fits_NOnonSingleton %>%
  group_by(family) %>%
  summarize(mean_rank_BIC = mean(rank_BIC)) %>%
  arrange(mean_rank_BIC)

print(mean_rank)

