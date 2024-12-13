#Load libraries
library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(ggplot2)
library(gamlss) #to fit model
require(gamlss.ggplots)
library(tidyr)
library(gamlssTools) #Margaret's package
library(gridExtra)
#rename some of the variables and select baseline scans
rename_vec <- c(age = "PCW_at_scan", sex = "sex_baseline", site = "site_id_l", nonSingleton = "nonSingleton")

data_filename <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter2024-09-20.csv"
#data_filename <- "/mnt/isilon/bgdlab_processing/Eren/ABCD-braincharts/CSV/process_tables_5.1/abcd5.1_long_selectVars_NOdxfilter_famfilter_outliersRM2024-10-28.csv"

df <- read.csv(data_filename) %>% select(-1) %>% rename(., all_of(rename_vec)) %>% 
  subset(eventname == "baseline_year_1_arm_1" & !is.na(age) & !is.na(nonSingleton) & (sex %in% c("M", "F")))#4512 folks
df$sex <- as.factor(df$sex)
df$site <- as.factor(df$site)
df$age <- as.numeric(df$age)

df_model <- df %>% 
  select(sex, age, site, nonSingleton, smri_vol_scs_allventricles) %>% 
  drop_na()

gamlss_gg <- gamlss (formula = as.formula("smri_vol_scs_allventricles ~ 1 + age + sex + random(site) + nonSingleton"),
                  sigma.formula = as.formula("smri_vol_scs_allventricles ~ 1 + age + sex + random(site) + nonSingleton"),
                  nu.formula = as.formula("~1"),
                  #tau.formula = as.formula ("~1 + age"),
                  family = "GG",
                  method = RS(),
                  data = df_model,
                  control = gamlss.control(n.cyc = 200))

gamlss_bcto <- gamlss (formula = as.formula("smri_vol_scs_allventricles ~ 1 + age + sex + random(site) + nonSingleton"),
                  sigma.formula = as.formula("smri_vol_scs_allventricles ~ 1 + age + sex + random(site) + nonSingleton"),
                  nu.formula = as.formula("~1 + age"),
                  tau.formula = as.formula ("~1  + age"),
                  family = "BCTo",
                  method = RS(),
                  data = df_model,
                  control = gamlss.control(n.cyc = 200))

gamlss_refit <- refit(gamlss)

df_model$nonSingleton <- as.numeric(df_model$nonSingleton)

#inspecting model -- set ylim to show more points on the plot
wp.taki(gamlss_bcto, ylim.worm = 1.5)
wp.taki(gamlss_gg, ylim = 1.5)

#qq plot testing (from the boook Ch 13)
round(Q.stats(gamlss_gg, xvar = df_model$age), 3)
