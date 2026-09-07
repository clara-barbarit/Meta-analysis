#1.Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)

#2.Database
bdd <- read_xlsx("Codage_articles.xlsx")
bdd_bis <- read_xlsx("coef_auteur.xlsx")

#3.Summary stats

#3.1.Development
bdd_bis |>
  group_by(development) |>
  summarise(number=n())

#3.2.Country
bdd_bis |>
  group_by(data_country) |>
  summarise(number=n())

#3.3.Endogeneity 
bdd_bis |>
  group_by(endogeneity_addressed) |>
  summarise(number=n())

#3.4.Weather
bdd_bis |>
  group_by(controls_weather) |>
  summarise(number=n())

#3.5.Exposure measure
bdd_bis |>
  group_by(exposure_measure) |>
  summarise(number=n())

#3.6.Indoor/Outdoor
bdd_bis |>
  group_by(indoor_outdoor) |>
  summarise(number=n())

#3.7.Outcome level
bdd_bis |>
  group_by(outcome_level) |>
  summarise(number=n())

#3.8.Secteur
bdd_bis |>
  group_by(sector) |>
  summarise(number=n())