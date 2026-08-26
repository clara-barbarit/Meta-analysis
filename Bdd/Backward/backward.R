library(tidyverse)

# 1. Fonctions de normalisation

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

# 2. Import B1 à B37

B <- lapply(
  1:37,
  function(i) {
    read.csv(
      paste0("Backward/B", i, ".csv"),
      stringsAsFactors = FALSE
    )
  }
)

names(B) <- paste0("B", 1:37)


# 3. Cleaning des bases 

B_clean <- lapply(
  seq_along(B),
  function(i) {
    
    B[[i]] |>
      mutate(
        Title = normalize_title(Title),
        DOI = as.character(DOI),
        doi_n = normalize_doi(DOI),
        Source = paste0("B", i)
      )
  }
)

names(B_clean) <- paste0("B", 1:37)

# 4. Fusion des bases
Backward_all <- bind_rows(B_clean)

# 5. Suppression des doublons
Backward_final <- Backward_all |>
  filter(
    !(has_doi(doi_n) & duplicated(doi_n))
  ) |>
  distinct(Title, .keep_all = TRUE)

# 6. Export

write_csv(
  Backward_final,
  "Backward/Backward_final.csv"
)
