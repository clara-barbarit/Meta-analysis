#Packages
library(readr)
library(tidyverse)

#Bases de données
Backward <- read.csv("BddScreening/Forward&Backward/Backward_cleaned.csv")
Forward <- read_csv("BddScreening/Forward&Backward/Forward_cleaned.csv")
All <- bind_rows(Backward, Forward)

#Suppression des doublons
All_final <- All |>
  filter(
    !(has_doi(doi_n) & duplicated(doi_n))
  ) |>
  distinct(Title, .keep_all = TRUE)

#Export
write_csv(All_final, "BddScreening/Asreview/Second_Screening.csv")