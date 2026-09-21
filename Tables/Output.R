library(tidyverse)
library(flextable)
library(officer)

# ============================================================
# 1. Base de données
# ============================================================

table_productivity <- tribble(
  ~Category, ~Measure, ~Studies, ~Definition,
  
  "Piece rate regime",
  "Output per hour per worker",
  "Naidenova et al. (2022); Wang et al. (2022); Hill et al. (2022)",
  "Workers' wages vary according to the quantity they produce over a given period.",
  
  "Piece rate regime",
  "Earning per hour per worker",
  "Zivin and Neidell (2012); Chang et al. (2016); Chen et al. (2021); Chen et al. (2025)",
  "",
  
  "Output per worker",
  "Value-added per year per worker",
  "Bai et al. (2026); Zhou and Zhang (2023); He et al. (2026); Fu et al. (2021); Fu et al. (2022); Fu et al. (2017); Majumder et al. (2024)",
  "Quantity produced by one unit of labor input over a given period, expressed either in physical output units or in monetary terms.",
  
  "Output per worker",
  "Output per year per worker",
  "Jiang et al. (2025); Wang et al. (2026); Hu et al. (2026); Jooste et al. (2022); Li et al. (2020)",
  "",
  
  "Output per worker",
  "Output per day per worker",
  "He et al. (2019); Batheja et al. (2025); Chang et al. (2019)",
  "",
  
  "Output per worker",
  "Output divided by targeted output per hour per worker",
  "Adhvaryu et al. (2019)",
  "",
  
  "Wage",
  "Wage and business income per year per worker",
  "Lin et al. (2024)",
  "Earnings an individual derives from their job.",
  
  "Wage",
  "Nominal wage per year per worker",
  "Drut and Mahieux (2015)",
  "",
  
  "Wage",
  "Hourly wage",
  "He and Ji (2021)",
  "",
  
  "Time spent per output",
  "Hearing length",
  "Samiento (2021)",
  "Time required to resolve a case or reach a judgment.",
  
  "Time spent per output",
  "Case length",
  "Kahn and Li (2020)",
  "",
  
  "Financial index",
  "Daily S&P return",
  "Heyes et al. (2016)",
  "Daily percentage return of the S&P 500 index.",
  
  "Financial index",
  "Abnormal trade performance",
  "Huang et al. (2020)",
  "Difference between the returns from stocks an investor buys and sells, after adjusting for expected returns.",
  
  "Cognitive performance",
  "Probability of correct call",
  "Archsmith et al. (2018)",
  "The number of correct calls divided by the total number of pitches called.",
  
  "Cognitive performance",
  "Flesch-Kincaid index",
  "Heyes et al. (2019)",
  "Measures speech complexity by the number of years of education required to understand it.",
  
  "Cognitive performance",
  "Number of correct answers",
  "Krebs and Luechinger (2024)",
  "Number of correct answers given by a worker in an arithmetic game.",
  
  "Cognitive performance",
  "Forecast accuracy",
  "Ali et al. (2025)",
  "Absolute difference between actual and forecasted EPS, scaled by the stock price, and multiplied by −1,000; the higher the accuracy index, the more accurate the forecast.",
  
  "Output",
  "Firm's airflow per day",
  "Goodenger et al. (2024)",
  "Airflow from the factory’s centralized air compressor.",
  
  "Output",
  "Number of administrative acts per day",
  "Kountouris (2020)",
  "Acts and decisions reported by the municipality per day.",
  
  "Physical performance",
  "Ball passes per match per player",
  "Zhang et al. (2024); Lichter et al. (2017); Li et al. (2024)",
  "The number of passes per match per player (goalkeepers excluded).",
  
  "Physical performance",
  "Shot attempts per match per player",
  "Bakhtar et al. (2026)",
  "The number of shot attempts per match per player.",
  
  "Physical performance",
  "Player efficiency rating",
  "Ni et al. (2023)",
  "Integrated index that assesses the efficiency of players, regarding their hit rate, free throws, three-pointers, assists, rebounds, blocks, steals, mistakes and fouls.",
  
  "Physical performance",
  "Finishing time",
  "Guo and Fu (2019); Naidenova et al. (2022)",
  "Time needed to complete the race.",
  
  "Physical performance",
  "Standardized endurance performance",
  "Mullins (2018)",
  "−(absolute difference between the world record and the athlete’s result) / standard deviation of results for the event.",
  
  "Other",
  "NA",
  "Xiao and Liu (2023)",
  "",
  
  "Other",
  "Regional GDP",
  "Zhou and Li (2021)",
  ""
)


# ============================================================
# 2. Définitions au niveau de la catégorie
# ============================================================

definitions <- c(
  "Piece rate regime" =
    "Workers' wages vary according to the quantity they produce over a given period.",
  
  "Output per worker" =
    "Quantity produced by one unit of labor input over a given period, expressed either in physical output units or in monetary terms.",
  
  "Wage" =
    "Earnings an individual derives from their job.",
  
  "Time spent per output" =
    "Time required to resolve a case or reach a judgment."
)

# On ne garde la définition que sur la première ligne
# de chaque catégorie concernée
table_productivity <- table_productivity |>
  group_by(Category) |>
  mutate(
    Definition = if_else(
      row_number() == 1 & Category %in% names(definitions),
      definitions[Category],
      Definition
    )
  ) |>
  ungroup()


# ============================================================
# 3. Création du tableau
# ============================================================

tab <- flextable(table_productivity)

# Noms des colonnes
tab <- set_header_labels(
  tab,
  Category = "Category",
  Measure = "Measure",
  Studies = "Studies",
  Definition = "Definition"
)

# ------------------------------------------------------------
# Fusion des catégories
# ------------------------------------------------------------

tab <- merge_v(
  tab,
  j = "Category"
)

# Fusion de la définition pour les catégories 1 à 4
# après regroupement des lignes correspondantes
for (cat in names(definitions)) {
  
  rows <- which(table_productivity$Category == cat)
  
  if (length(rows) > 1) {
    tab <- merge_at(
      tab,
      i = rows,
      j = 4
    )
  }
}


# ============================================================
# 4. Style "research paper / economics"
# ============================================================

# Police
tab <- font(
  tab,
  fontname = "Times New Roman",
  part = "all"
)

# Taille
tab <- fontsize(
  tab,
  size = 10,
  part = "all"
)

# En-tête
tab <- bold(
  tab,
  part = "header"
)

tab <- fontsize(
  tab,
  size = 10,
  part = "header"
)

# Alignement
tab <- align(
  tab,
  j = c("Measure", "Studies", "Definition"),
  align = "left",
  part = "all"
)

tab <- align(
  tab,
  j = "Category",
  align = "left",
  part = "all"
)

# Alignement vertical au milieu pour les cellules fusionnées
tab <- valign(
  tab,
  valign = "center",
  part = "all"
)


# ============================================================
# 5. Largeur des colonnes
# ============================================================

tab <- width(
  tab,
  j = "Category",
  width = 1.25
)

tab <- width(
  tab,
  j = "Measure",
  width = 1.80
)

tab <- width(
  tab,
  j = "Studies",
  width = 2.80
)

tab <- width(
  tab,
  j = "Definition",
  width = 3.00
)


# ============================================================
# 6. Espacement et lignes
# ============================================================

tab <- padding(
  tab,
  padding.top = 4,
  padding.bottom = 4,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

# Pas de quadrillage vertical
tab <- border_remove(tab)

# Ligne supérieure
tab <- hline_top(
  tab,
  border = fp_border(
    color = "black",
    width = 1.5
  ),
  part = "all"
)

# Ligne sous les noms de colonnes
tab <- hline(
  tab,
  i = 1,
  border = fp_border(
    color = "black",
    width = 0.75
  ),
  part = "header"
)

# Ligne inférieure
tab <- hline_bottom(
  tab,
  border = fp_border(
    color = "black",
    width = 1.5
  ),
  part = "all"
)


# ============================================================
# Lignes de séparation entre les catégories
# ============================================================

# Dernière ligne de chaque catégorie
category_end <- which(
  table_productivity$Category !=
    dplyr::lead(
      table_productivity$Category,
      default = ""
    )
)

# Bordure inférieure sur la dernière ligne de chaque catégorie
for (i in category_end[-length(category_end)]) {
  
  tab <- border(
    tab,
    i = i,
    j = 1:4,
    border.bottom = fp_border(
      color = "black",
      width = 0.5
    ),
    part = "body"
  )
}


# ============================================================
# 8. Affichage
# ============================================================

tab

save_as_docx(
  tab,
  path = "C:/Users/196408/Documents/Meta-analysis/Tables/productivity_measures_table.docx"
)

tab <- bg(
  tab,
  bg = "white",
  part = "all"
)

save_as_image(
  tab,
  path = "C:/Users/196408/Documents/Meta-analysis/Tables/productivity_measures_table.png"
)