#Packages
library(readr)
library(tidyverse)
library(knitr)

#Bases de données
Extraction <- read.csv("BddScreening/Asreview/Result_First_Screening.csv")
Forward <- read_csv("BddScreening/Forward&Backward/Raw_data/Forward.csv")

#Fonctions
normalize_title <- function(x) {
  x |>
    as.character() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

normalize_doi <- function(x) {
  x |>
    as.character() |>
    str_to_lower() |>
    str_remove("^https?://(dx\\.)?doi\\.org/") |>
    str_trim()
}

has_doi <- function(x) {
  !is.na(x) & x != ""
}

#Cleaning
Extraction_clean <- Extraction |>
  mutate(
    Title = normalize_title(Title),
    doi_n = normalize_doi(DOI_Valentin)
  )

Forward_clean <- Forward |>
  transmute(
    Authors.full.names = Author,
    Title = normalize_title(Title),
    Year = as.integer(`Publication Year`),
    Source.title = `Publication Title`,
    Cited.by = NA_integer_,
    DOI = DOI,
    Link = as.character(Url),
    Abstract = `Abstract Note`,
    Author.Keywords = NA_character_,
    Index.Keywords = NA_character_,
    Language.of.Original.Document = Language,
    Source = "Snowballing",
    Document.Type = `Item Type`
  ) |>
  mutate(
    doi_n = normalize_doi(DOI)
  )

Forward_clean <- Forward_clean |>
  filter(
    !(has_doi(doi_n) & duplicated(doi_n))
  ) |>
  distinct(Title, .keep_all = TRUE)


#Suppression des doublons par 1)DOI 2)Titre
Forward_filtered <- Forward_clean |>
  filter(
    !(has_doi(doi_n) & doi_n %in% Extraction_clean$doi_n)
  ) |>
  filter(
    !(Title %in% Extraction_clean$Title)
  )

#Stats
n_forward <- nrow(Forward_clean)

n_doublons <- n_forward - nrow(Forward_filtered)

summary_forward <- tibble(
  Indicateur = c(
    "Articles issus de forward searching",
    "Articles déjà présents dans BDD",
    "Nouveaux articles issus de forward"
  ),
  Valeur = c(
    n_forward,
    n_doublons,
    nrow(Forward_filtered)
  )
)

kable(summary_forward)

write_csv(Forward_filtered, "BddScreening/Forward&Backward/Forward_cleaned.csv")
