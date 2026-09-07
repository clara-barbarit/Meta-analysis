#Packages
library(readr)
library(tidyverse)

#Bases de données
Extraction <- read.csv("BDD.csv")
Snowballing <- read_csv("snowballing_extraction.csv")

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
    doi_n = normalize_doi(DOI)
  )

Snowballing_clean <- Snowballing |>
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

Snowballing_clean <- Snowballing_clean |>
  filter(
    !(has_doi(doi_n) & duplicated(doi_n))
  ) |>
  distinct(Title, .keep_all = TRUE)


#Suppression des doublons par 1)DOI 2)Titre
Snowballing_filtered <- Snowballing_clean |>
  filter(
    !(has_doi(doi_n) & doi_n %in% Extraction_clean$doi_n)
  ) |>
  filter(
    !(Title %in% Extraction_clean$Title)
  )

#Stats
n_snowballing <- nrow(Snowballing_clean)

n_doublons <- n_snowballing - nrow(Snowballing_filtered)

summary_snowballing <- tibble(
  Indicateur = c(
    "Articles issus du snowballing",
    "Articles déjà présents dans BDD",
    "Nouveaux articles issus du snowballing"
  ),
  Valeur = c(
    n_snowballing,
    n_doublons,
    nrow(Snowballing_filtered)
  )
)

kable(summary_snowballing)

write_csv(Snowballing_filtered, "Snowballing.csv")
