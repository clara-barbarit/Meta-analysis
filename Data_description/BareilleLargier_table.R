library(tidyverse)
library(gt)

# ============================================================
# 1. LOAD AND PREPARE THE DATA
# ============================================================

Df_table2 <- read_csv("Data_description/Df_clean.csv") |>
  mutate(
    id_strategy = case_when(
      is.na(id_strategy) | id_strategy == "" ~ NA_character_,
      id_strategy %in% c("OLS", "IV")        ~ id_strategy,
      TRUE                                   ~ "Other"
    )
  ) |>
  filter(!is.na(study_id)) |>
  group_by(study_id) |>
  mutate(weight = 1 / n()) |>
  ungroup()

# Standard-error column
se_col <- "se_elasticity"

if (!se_col %in% names(Df_table2)) {
  stop("The column 'se_elasticity' was not found in Df_table2.")
}

# ============================================================
# 2. DEFINE VARIABLES AND CATEGORIES
# ============================================================

# PANEL A - STRUCTURAL CHARACTERISTICS

groups <- tribble(
  ~variable, ~subcategory, ~order,
  "data_country", "Country", 1,
  "development", "Development level", 2,
  "urban_rural", "Localisation (urban / rural)", 3,
  "indoor_outdoor", "Indoor or outdoor workers", 4,
  "age_mean", "Mean Age", 5,
  "pct_female", "Female", 6,
  
  "sector", "Sector", 7,
  "task_type", "Task type", 8,
  "outcome_category", "Productivity measures", 9,
  "outcome_level", "Niveau d'agrégation", 10,
  
  "pollutant", "Pollution measures", 11,
  "exposure_measure", "Pollution method measurement", 12,
  
  "data_structure", "Data structure", 13,
  "time_frequency_observation", "Frequency", 14,
  "effect_type_raw", "Estimation method", 15,
  "id_strategy", "Identification strategy", 16,
  
  "time_FE", "FE", 17,
  "worker_FE", "FE", 17,
  "firm_FE", "FE", 17,
  "localisation_FE", "FE", 17,
  
  "controls_weather", "Weather controls", 18,
  "control_precipitation", "Weather controls", 18,
  "control_temp", "Weather controls", 18,
  "control_radiation", "Weather controls", 18,
  "control_wind_speed", "Weather controls", 18,
  "control_wind_direction", "Weather controls", 18,
  "control_humidity", "Weather controls", 18,
  "control_air_pressure", "Weather controls", 18,
  "control_snow", "Weather controls", 18,
  "control_dew_points", "Weather controls", 18,
  "control_skycover", "Weather controls", 18,
  "control_sunshine", "Weather controls", 18,
  
  "controls_multipollutant", "Other pollutant controls", 19,
  
  "controls_worker_characteristics", "Other controls", 19,
  "attendance_bias_treated", "Other controls", 19,
  "controls_traffic_effect", "Other controls", 19,
  "controls_demand_effect", "Other controls", 19,
  "controls_wage_shirking", "Other controls", 19
)

# ============================================================
# 3. DEFINE VARIABLE TYPES
# ============================================================

num <- c("age_mean", "pct_female")

dummy_groups <- c("FE", "Weather controls", "Other pollutant controls", "Other controls")
dummy <- groups |>
  filter(subcategory %in% dummy_groups) |>
  pull(variable)

cat <- groups |>
  filter(!variable %in% c(num, dummy)) |>
  pull(variable)

#Detecting potential mismatch between Df_table2 and groups
missing_variables <- setdiff(
  groups$variable,
  names(Df_table2)
)

if (length(missing_variables) > 0) {
  stop(
    "The following variables are missing from Df_table2: ",
    paste(missing_variables, collapse = ", ")
  )
}

# ============================================================
# 4. VARIABLE LABELS
# ============================================================

variable_labels <- c(
  data_country = "Country",
  development = "Development level",
  urban_rural = "Localisation (urban / rural)",
  indoor_outdoor = "Indoor or outdoor workers",
  sector = "Sector",
  task_type = "Task type",
  outcome_category = "Productivity measures",
  outcome_level = "Niveau d'agrégation",
  exposure_measure = "Pollution method measurement",
  data_structure = "Data structure",
  time_frequency_observation = "Frequency",
  age_mean = "Mean Age",
  pct_female = "Female",
  
  pollutant = "Pollution measures",
  effect_type_raw = "Estimation method",
  id_strategy = "Identification strategy",
  
  time_FE = "Time FE",
  worker_FE = "Worker FE",
  firm_FE = "Firm FE",
  localisation_FE = "Localisation FE",
  
  controls_weather = "Weather controls",
  control_precipitation = "Precipitation",
  control_temp = "Temperature",
  control_radiation = "Radiation",
  control_wind_speed = "Wind speed",
  control_wind_direction = "Wind direction",
  control_humidity = "Humidity",
  control_air_pressure = "Air pressure",
  control_snow = "Snow",
  control_dew_points = "Dew point",
  control_skycover = "Sky cover",
  control_sunshine = "Sunshine",
  
  controls_multipollutant = "Multiple pollutants",
  
  controls_worker_characteristics = "Worker characteristics",
  attendance_bias_treated = "Attendance bias",
  controls_traffic_effect = "Traffic controls",
  controls_demand_effect = "Demand controls",
  controls_wage_shirking = "Wage / shirking controls"
)

# ============================================================
# 5. STATISTICS FUNCTIONS
# ============================================================

# ============================================================
# 5. STATISTICS FUNCTIONS
# ============================================================

elasticity_col <- "elasticity"   # nom de la colonne (chaîne de caractères)

if (!elasticity_col %in% names(Df_table2)) {
  stop("The column '", elasticity_col, "' was not found in Df_table2.")
}

weighted_mean_safe <- function(x, w) {
  valid <- is.finite(x) & is.finite(w) & w > 0
  if (!any(valid)) return(NA_real_)
  weighted.mean(x[valid], w[valid])
}

# Noms distincts de ceux de base : on n'écrase jamais mean/sd/min/max
mean_safe <- function(v) if (length(v) > 0) mean(v) else NA_real_
sd_safe   <- function(v) if (length(v) > 1) sd(v)   else NA_real_
min_safe  <- function(v) if (length(v) > 0) min(v)  else NA_real_
max_safe  <- function(v) if (length(v) > 0) max(v)  else NA_real_

calc_stats <- function(x, data, scale = 100, elasticity_stats = TRUE) {
  
  x    <- suppressWarnings(as.numeric(x))
  elas <- suppressWarnings(as.numeric(data[[elasticity_col]]))
  se   <- suppressWarnings(as.numeric(data[[se_col]]))
  
  valid_x <- is.finite(x)
  
  in_cat <- if (elasticity_stats) valid_x & x == 1 else rep(FALSE, length(x))
  e <- elas[in_cat & is.finite(elas)]
  s <- se[in_cat & is.finite(se)]
  
  tibble(
    pct_estimates = scale * mean_safe(x[valid_x]),
    pct_studies   = scale * weighted_mean_safe(x, data$weight),
    elas_mean     = if (elasticity_stats) mean_safe(e) else NA_real_,
    elas_sd       = if (elasticity_stats) sd_safe(e)   else NA_real_,
    elas_min      = if (elasticity_stats) min_safe(e)  else NA_real_,
    elas_max      = if (elasticity_stats) max_safe(e)  else NA_real_,
    se_sd         = if (elasticity_stats) sd_safe(s)   else NA_real_
  )
}

# ============================================================
# 6. STATISTICS BY VARIABLE TYPE
# ============================================================

# Motif repéré dans instrument_type (insensible à la casse) : à adapter
# après Df_table2 |> count(instrument_type)
thermal_pattern <- "thermal"

# Lignes "parmi les estimations IV"
iv_rows <- function(data) {
  
  d <- data |>
    filter(id_strategy == "IV") |>
    group_by(study_id) |>
    mutate(weight = 1 / n()) |>   # poids recalculé au sein du sous-échantillon IV
    ungroup()
  
  thermal <- str_detect(
    coalesce(as.character(d$instrument_type), ""),
    regex(thermal_pattern, ignore_case = TRUE)
  )
  
  bind_rows(
    bind_cols(
      tibble(Variable = "id_strategy", Category = "Thermal inversion",
             Level = 1L, Sub_rank = 1L),
      calc_stats(as.numeric(thermal), d)
    ),
    bind_cols(
      tibble(Variable = "id_strategy", Category = "Other instrument",
             Level = 1L, Sub_rank = 2L),
      calc_stats(as.numeric(!thermal), d)
    )
  )
}

stats_variable <- function(v, data) {
  
  if (v %in% num) {

    return(bind_cols(
      tibble(Variable = v, Category = ""),
      calc_stats(data[[v]], data, scale = 1, elasticity_stats = FALSE)
    ))
  }
  
  if (v %in% dummy) {
    return(bind_cols(
      tibble(Variable = v, Category = "Yes (= 1)"),
      calc_stats(data[[v]], data)
    ))
  }
  
  # Catégorielle : une ligne par modalité
  values <- as.character(data[[v]])
  values[is.na(values) | values == ""] <- "NA"
  
  out <- map_dfr(unique(values), function(mod) {
    bind_cols(
      tibble(Variable = v, Category = mod),
      calc_stats(as.numeric(values == mod), data)
    )
  })
  
  # Sous-lignes sous IV
  if (v == "id_strategy") out <- bind_rows(out, iv_rows(data))
  
  out
}

# ============================================================
# 7. BUILD THE RESULT ROWS (ordre de groups)
# ============================================================

groups_pos <- groups |>
  mutate(
    pos   = row_number(),
    block = match(subcategory, unique(subcategory))  # ordre d'apparition
  ) |>
  select(variable, subcategory, pos, block)

clean_label <- function(x) {
  x <- gsub("_", " ", x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

results <- map_dfr(groups$variable, stats_variable, data = Df_table2) |>
  left_join(groups_pos, by = c("Variable" = "variable")) |>
  mutate(
    Level    = coalesce(Level, 0L),
    Sub_rank = coalesce(Sub_rank, 0L),
    Variable_label = coalesce(unname(variable_labels[Variable]), Variable),
    Row_label = if_else(Category %in% c("", "Yes (= 1)"), Variable_label, Category),
    Row_label = clean_label(Row_label)
  ) |>
  # Les sous-lignes héritent de la position de la ligne IV
  mutate(
    iv_pct   = pct_estimates[Variable == "id_strategy" & Category == "IV"][1],
    sort_pct = if_else(Level == 1L, iv_pct, pct_estimates)
  ) |>
  arrange(block, pos, desc(sort_pct), Sub_rank)

# ============================================================
# 8. ADD CATEGORY HEADERS
# ============================================================

stat_cols <- c("pct_estimates", "pct_studies", "elas_mean",
               "elas_sd", "elas_min", "elas_max", "se_sd")

# Pas d'en-tête quand le bloc = une seule ligne portant le même libellé
# (ex. "Mean Age")
results <- results |>
  group_by(block) |>
  mutate(has_header = !(n() == 1 & Row_label == first(subcategory))) |>
  ungroup()

result_rows <- results |>
  mutate(
    is_header = !has_header,
    row_order = row_number(),
    Characteristic = case_when(
      Level == 1L ~ paste0("   ", strrep("\u00A0", 6), Row_label),  # sous-ligne décalée
      has_header  ~ paste0("   ", Row_label),
      TRUE        ~ Row_label
    )) |>
  select(block, Characteristic, is_header, row_order, all_of(stat_cols))

header_rows <- results |>
  filter(has_header) |>
  group_by(block) |>
  summarise(
    Characteristic = first(subcategory),
    row_order = min(row_number()) - 0.5,
    .groups = "drop"
  ) |>
  left_join(
    results |> group_by(block) |> summarise(r = min(row_number()), .groups = "drop"),
    by = "block"
  ) |>
  mutate(is_header = TRUE) |>
  select(block, Characteristic, is_header, row_order)

# row_order des en-têtes : juste avant la première ligne de leur bloc
first_row <- result_rows |>
  group_by(block) |>
  summarise(first_order = min(row_order), .groups = "drop")

header_rows <- header_rows |>
  select(-row_order) |>
  left_join(first_row, by = "block") |>
  mutate(row_order = first_order - 0.5) |>
  select(-first_order)

Table2_display <- bind_rows(header_rows, result_rows) |>
  arrange(row_order) |>
  select(Characteristic, is_header, all_of(stat_cols))

# ============================================================
# 9. FORMAT THE TABLE WITH GT
# ============================================================

Table2_gt <- Table2_display |>
  gt(rowname_col = "Characteristic") |>
  cols_hide(columns = is_header) |>
  cols_label(
    pct_estimates = "% of estimates",
    pct_studies   = "% of studies",
    elas_mean     = "Mean elasticity",
    elas_sd       = "SD elasticity",
    elas_min      = "Min",
    elas_max      = "Max",
    se_sd         = "SD (SE)"
  ) |>
  fmt_number(columns = c(pct_estimates, pct_studies), decimals = 1) |>
  fmt_number(columns = c(elas_mean, elas_sd, elas_min, elas_max, se_sd),
             decimals = 3) |>
  sub_missing(columns = everything(), missing_text = "") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = is_header)
  ) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#D3D3D3",
      weight = px(2)
    ),
    locations = cells_body(columns = elas_mean)
  ) |>
  tab_options(
    table.font.size = 11,
    data_row.padding = px(3),
    stub.border.style = "solid",
    stub.border.width = px(2),
    stub.border.color = "#D3D3D3"
  ) |>
  tab_source_note(
    "Note: under IV, the shares are computed among IV estimates only."
  )
# ============================================================
# 10. EXPORT AND DISPLAY
# ============================================================

gtsave(Table2_gt, "Data_description/Table2_heterogeneity.html")

Table2_gt