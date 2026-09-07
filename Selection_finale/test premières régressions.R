library(readxl)

base <- read_xlsx("Codage_articles.xlsx")

base <- base |>
  mutate(elasticity = as.numeric(elasticity),
         se_elasticity = as.numeric(se_elasticity))

model1 <- lm(
  elasticity ~ se_elasticity + development + pollutant,
  data = base)

summary(model1)
