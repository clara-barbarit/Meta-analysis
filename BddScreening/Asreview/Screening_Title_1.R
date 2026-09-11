#Packages

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(knitr)
library(stringr)
library(readr)


# Loading screening databases 
Valentin <- bind_rows(read.csv("BddScreening/Asreview/Scopus_Valentin.csv"), read.csv("BddScreening/Asreview/WOS_EconLit_Valentin.csv"))
Clara <- bind_rows(read.csv("BddScreening/Asreview/Scopus_Clara.csv"), read.csv("BddScreening/Asreview/WOS_EconLit_Clara.csv"))

# Removing intra-bases duplicates
Valentin <- Valentin |>
  distinct(Title, .keep_all = TRUE)

Clara <- Clara |>
  distinct(Title, .keep_all = TRUE)

# Export dataset
Result_First_Screening <- Valentin |>
  inner_join(
    Clara,
    by = "Title",
    suffix = c("_Valentin", "_Clara")
  ) |>
  filter(
    asreview_label_Valentin == 1 |
      asreview_label_Clara == 1
  )

write.csv(
  Result_First_Screening,
  "BddScreening/Asreview/Result_First_Screening.csv",
  row.names = FALSE
)
