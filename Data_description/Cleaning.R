#Libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(knitr)

#Loading the dataset
Df <- read_xlsx("Data_description/Codage_articles.xlsx")

#Cleaning the dataset
Df_clean <- Df |>
  select(-c(effect_raw, 
            se_raw,
            p_value, 
            ci_low, 
            ci_high, 
            estimated_ci, 
            n_regressor, 
            estimated_n_regressor, 
            pcc, 
            se_source, 
            elasticity, 
            test_calcul_elasticity, 
            verif_elasticity, 
            se_elasticity, 
            latent_variable, 
            CHECK, 
            poll_mean_elasticity, 
            poll_sd, 
            poll_min, 
            poll_max, 
            outcome_estimated_mean,
            outcome_sd, 
            outcome_min, 
            outcome_max, 
            outcome_objective, 
            instrument_used, 
            extract_origin, 
            coder, 
            coding_date, 
            double_coded, 
            notes
            )) |>
  mutate(pcc = pcc_reoriented, 
         elasticity = elast_reoriented
         )|>
  select(-c(pcc_reoriented, 
            elast_reoriented))