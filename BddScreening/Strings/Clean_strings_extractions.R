#Packages

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(knitr)
library(stringr)
library(readr)

#I - Load the databases

#1. Web of Science

WOS <- read_excel("BddScreening/Strings/Raw_data/WoS.xls")

#2. Scopus

Scopus <- read.csv("BddScreening/Strings/Raw_data/Scopus.csv")

#3.EconLit

EconLit <- bind_rows(
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026.csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (1).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (2).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (3).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (4).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (5).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (6).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (7).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (8).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (9).csv", col_types = cols(.default = col_character())),
  read_csv("BddScreening/Strings/Raw_data/EconLit/EBSCO-Metadata-06_29_2026 (10).csv", col_types = cols(.default = col_character()))
)

write_csv(EconLit, "BddScreening/Strings/Raw_data/EconLit.csv")

# II - Data cleaning

normalize_title <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |> #pour enlever les ponctuations
    str_squish()
}

normalize_doi <- function(x) {
  x |> 
    as.character() |> 
    str_to_lower() |>
    str_remove("^https?://(dx\\.)?doi\\.org/") |> 
    str_trim()
}

has_doi <- function(x) !is.na(x) & x != ""


#1. Scopus
Scopus_clean <- Scopus |>
  transmute(
    Authors.full.names = Author.full.names,
    Title = normalize_title(Title),
    Year = as.integer(Year),
    Source.title = Source.title,
    Cited.by = Cited.by,
    DOI = DOI,
    Link = as.character(Link),
    Abstract = Abstract,
    Author.Keywords = Author.Keywords,
    Index.Keywords = Index.Keywords,
    Language.of.Original.Document = Language.of.Original.Document,
    Source = "Scopus",
    Document.Type = Document.Type
  )

#2. EconLit
EconLit_clean <- EconLit |>
  transmute(
    Authors.full.names = contributors,
    Title = normalize_title(title),
    Year = as.integer(str_extract(publicationDate, "^\\d{4}")),
    Source.title = source,
    Cited.by = as.integer(citedByCount),
    DOI = doi,
    Link = as.character(plink),
    Abstract = abstract,
    Author.Keywords = subjects,
    Index.Keywords = subjects,
    Language.of.Original.Document = language,
    Source = "EconLit",
    Document.Type = docTypes
  )

#3. WOS
WOS_clean <- WOS |>
  transmute(
    Authors.full.names = `Author Full Names`,
    Title = normalize_title(`Article Title`),
    Year = as.integer(`Publication Year`),
    Source.title = `Source Title`,
    Cited.by = as.integer(`Times Cited, All Databases`),
    DOI = DOI,
    Link = as.character(`DOI Link`),
    Abstract = Abstract,
    Author.Keywords = `Author Keywords`,
    Index.Keywords = `Keywords Plus`,
    Language.of.Original.Document = Language,
    Source = "Web of Science",
    Document.Type = `Document Type`
  )


#4. Duplicates removing

Scopus_clean  <- Scopus_clean  |> mutate(doi_n = normalize_doi(DOI))
WOS_clean     <- WOS_clean     |> mutate(doi_n = normalize_doi(DOI))
EconLit_clean <- EconLit_clean |> mutate(doi_n = normalize_doi(DOI))

WOS_filtered <- WOS_clean |>
  filter(!(has_doi(doi_n) & doi_n %in% Scopus_clean$doi_n)) |>
  filter(!(Title %in% Scopus_clean$Title))

doi_deja   <- c(Scopus_clean$doi_n, WOS_filtered$doi_n)
titre_deja <- c(Scopus_clean$Title, WOS_filtered$Title)

EconLit_filtered <- EconLit_clean |>
  filter(!(has_doi(doi_n) & doi_n %in% doi_deja)) |>
  filter(!(Title %in% titre_deja))

# III - Final dataset from first strings screening

First_screening_cleaned <- bind_rows(Scopus_clean, WOS_filtered, EconLit_filtered)

write_csv(First_screening_cleaned, "BddScreening/Strings/First_screening_cleaned.csv")

# IV - Removing intern duplicates 
First_screening_cleaned <- First_screening_cleaned |>
  filter(!(has_doi(doi_n) & duplicated(doi_n))) |>
  distinct(Title, .keep_all = TRUE)

# V - Summary stats 

Merge <- nrow(First_screening_cleaned)

Total <- nrow(Scopus) + nrow(EconLit) + nrow(WOS)

summary_dedup <- tibble(
  Indicateur = c(
    "Total of screened articles (3 databases)",
    "Screened articles after removing duplicates",
    "Number of articles removed"),
  Valeur = c(
    Total,
    Merge,
    Total - Merge))

kable(summary_dedup)


