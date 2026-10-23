#Libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

#Loading the dataset
Df <- read_xlsx("Data_description/Codage_articles.xlsx")

#Cleaning the dataset
Df_clean <- Df |>
  select(-c(source_db,
            peer_reviewed,
            effect_raw, 
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
            test_calcul_se,
            latent_variable, 
            CHECK, 
            poll_mean_elasticity, 
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
         elasticity = elast_reoriented,
         outcome_category = `outcome category`
         )|>
  select(-c(pcc_reoriented, 
            elast_reoriented,
            "outcome category")) |>
  mutate(
    id_strategy = case_match(
      id_strategy,
      "LPM" ~ "OLS",
      .default = id_strategy
    ),
    data_structure = case_match(
      data_structure,
      "time serie" ~ "time_serie",
      .default = data_structure
    ),
    outcome_category = case_match(
      outcome_category,
      "financial index" ~ "financial_index",
      .default = outcome_category
    ),
    outcome_level = case_match(
      outcome_level,
      "individual" ~ "worker",
      .default = outcome_level
    )
  )

write.csv(
  Df_clean,
  "Data_description/Df_clean.csv",
  row.names = FALSE
)

Df_clean |>
  filter(t_stat>-1.96 & t_stat<1.96) |>
  nrow()