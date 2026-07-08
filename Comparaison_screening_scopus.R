library(dplyr)
library(knitr)
library(openxlsx)

# Import
Valentin <- read.csv("C:/Users/196408/Documents/Meta-analysis/Bdd/Asreview/Scopus.csv")
Clara <- read.csv("C:/Users/196408/Documents/Meta-analysis/Bdd/Asreview/Scopus_v2.csv")

# Comparaison des labels
Comparaison <- Valentin %>%
  select(EID, Title, DOI, Abstract, asreview_label) %>%
  inner_join(
    Clara %>% select(EID, asreview_label),
    by = "EID",
    suffix = c("_Valentin", "_Clara")
  )

# Base des désaccords
Desaccords <- Comparaison %>%
  filter(asreview_label_Valentin != asreview_label_Clara)

# Export
write.csv(
  Desaccords,
  "C:/Users/196408/Documents/Meta-analysis/Bdd/Asreview/Desaccords.csv",
  row.names = FALSE
)

write.xlsx(
  Desaccords,
  "C:/Users/196408/Documents/Meta-analysis/Bdd/Asreview/Desaccords.xlsx",
  row.names = FALSE
)

# Tableau récapitulatif
Summary <- data.frame(
  `Nombre total de papiers` = nrow(Comparaison),
  `Accords` = sum(Comparaison$asreview_label_Valentin == Comparaison$asreview_label_Clara),
  `Désaccords` = nrow(Desaccords),
  `Accords (label = 1)` = sum(
    Comparaison$asreview_label_Valentin == 1 &
      Comparaison$asreview_label_Clara == 1
  )
)

kable(Summary)