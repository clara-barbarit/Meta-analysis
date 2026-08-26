#Packages
library(readr)
library(tidyverse)

#Bases de données
Backward <- read.csv("Backward/Backward_final.csv")
Snowballing <- read_csv("Snowballing.csv")
All <- bind_rows(Backward, Snowballing)

#Suppression des doublons
All_final <- All |>
  filter(
    !(has_doi(doi_n) & duplicated(doi_n))
  ) |>
  distinct(Title, .keep_all = TRUE)

#Export
write_csv(All_final, "ScreeningPart2.csv")