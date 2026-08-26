library(metafor)
library(readxl)
library(dplyr)

# Exemple avec un jeu de données intégré (essais sur la BCG)
data <- read_xlsx("G:/.shortcut-targets-by-id/1oHC0_9lNrurmNP0_71QomwzsiNZnRUzZ/Cobenefices_Clara/Codage/Codage_articles.xlsx")

data$se_elasticity <- as.numeric(data$se_elasticity)

data <- data |> filter(authors != "Kogel")
data <- data |> filter(se_elasticity != 0)

funnel(x = data$elasticity, 
       sei = 1/data$se_elasticity,
       xlab = "Élasticité",
       ylab = "Erreur standard",
       main = "Funnel Plot des élasticités")