library(tidyverse)
library(gt)

# ============================================================
# 1. LOAD AND PREPARE THE DATA
# ============================================================

Df_table2 <- read_csv("Data_description/Df_clean.csv") |>
  filter(!is.na(study_id)) |>
  filter(!study_id=="Sarmiento2021") |>
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

structural_groups <- tribble(
  ~variable, ~subcategory, ~order,
  "data_country", "Country", 1,
  "development", "Development level", 2,
  "urban_rural", "Localisation (urban / rural)", 3,
  "indoor_outdoor", "Indoor or outdoor workers", 4,
  "sector", "Sector", 5,
  "task_type", "Task type", 6,
  "outcome_category", "Productivity measures", 7,
  "outcome_level", "Niveau d'agrégation", 8,
  "exposure_measure", "Pollution method measurement", 9,
  "data_structure", "Data structure", 10,
  "time_frequency_observation", "Frequency", 11,
  "age_mean", "Mean Age", 12,
  "pct_female", "Female", 13
)

# PANEL B - METHODOLOGICAL CHARACTERISTICS

method_groups <- tribble(
  ~variable, ~subcategory, ~order,
  "pollutant", "Pollution measures", 1,
  "effect_type_raw", "Estimation method", 2,
  "id_strategy", "Identification strategy", 3,
  "instrument_type", "Instrument", 4,
  
  "time_FE", "FE", 5,
  "worker_FE", "FE", 5,
  "firm_FE", "FE", 5,
  "localisation_FE", "FE", 5,
  
  "controls_weather", "Weather controls", 6,
  "control_precipitation", "Weather controls", 6,
  "control_temp", "Weather controls", 6,
  "control_radiation", "Weather controls", 6,
  "control_wind_speed", "Weather controls", 6,
  "control_wind_direction", "Weather controls", 6,
  "control_humidity", "Weather controls", 6,
  "control_air_pressure", "Weather controls", 6,
  "control_snow", "Weather controls", 6,
  "control_dew_points", "Weather controls", 6,
  "control_skycover", "Weather controls", 6,
  "control_sunshine", "Weather controls", 6,
  
  "controls_multipollutant", "Other pollutant controls", 7,
  "control_PM2.5", "Other pollutant controls", 7,
  "control_PM10", "Other pollutant controls", 7,
  "control_O3", "Other pollutant controls", 7,
  "control_CO", "Other pollutant controls", 7,
  "control_NO2", "Other pollutant controls", 7,
  "control_SO2", "Other pollutant controls", 7,
  
  "controls_worker_characteristics", "Other controls", 8,
  "attendance_bias_treated", "Other controls", 8,
  "controls_traffic_effect", "Other controls", 8,
  "controls_demand_effect", "Other controls", 8,
  "controls_wage_shirking", "Other controls", 8
)

# ============================================================
# 3. DEFINE VARIABLE TYPES
# ============================================================

structural_cat <- structural_groups |>
  filter(!variable %in% c("age_mean", "pct_female")) |>
  pull(variable)

structural_num <- c("age_mean", "pct_female")

method_cat <- c(
  "pollutant",
  "effect_type_raw",
  "id_strategy",
  "instrument_type"
)

method_dummy <- method_groups |>
  filter(!variable %in% method_cat) |>
  pull(variable)

all_variables <- unique(c(
  structural_cat,
  structural_num,
  method_cat,
  method_dummy
))

missing_variables <- setdiff(
  all_variables,
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
  instrument_type = "Instrument",
  
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
  `control_PM2.5` = "PM2.5",
  control_PM10 = "PM10",
  control_O3 = "O3",
  control_CO = "CO",
  control_NO2 = "NO2",
  control_SO2 = "SO2",
  
  controls_worker_characteristics = "Worker characteristics",
  attendance_bias_treated = "Attendance bias",
  controls_traffic_effect = "Traffic controls",
  controls_demand_effect = "Demand controls",
  controls_wage_shirking = "Wage / shirking controls"
)

# ============================================================
# 5. STATISTICS FUNCTIONS
# ============================================================

weighted_mean_safe <- function(x, w) {
  
  valid <- is.finite(x) &
    is.finite(w) &
    w > 0
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  weighted.mean(x[valid], w[valid])
}

calc_stats <- function(x, data) {
  
  x <- suppressWarnings(as.numeric(x))
  se <- suppressWarnings(
    as.numeric(data[[se_col]])
  )
  
  # Weights based on the number of observations per study
  w_n <- data$weight
  
  # Inverse-standard-error weights
  w_se <- ifelse(
    is.finite(se) & se > 0,
    1 / se,
    NA_real_
  )
  
  # Inverse-variance weights
  w_se2 <- ifelse(
    is.finite(se) & se > 0,
    1 / (se^2),
    NA_real_
  )
  
  valid_x <- is.finite(x)
  
  tibble(
    Mean = if (any(valid_x)) {
      mean(x[valid_x])
    } else {
      NA_real_
    },
    
    `Weighted mean (1/n)` =
      weighted_mean_safe(x, w_n),
    
    `Weighted mean (1/SE)` =
      weighted_mean_safe(x, w_se),
    
    `Weighted mean (1/SE^2)` =
      weighted_mean_safe(x, w_se2),
    
    SD = if (sum(valid_x) > 1) {
      sd(x[valid_x])
    } else {
      NA_real_
    }
  )
}

# ============================================================
# 6. CATEGORICAL VARIABLES
# ============================================================

stats_categorical <- function(data, variables, groups, panel) {
  
  map_dfr(variables, function(v) {
    
    values <- as.character(data[[v]])
    
    values[is.na(values) | values == ""] <- "Not reported"
    
    modalities <- unique(values)
    
    map_dfr(modalities, function(mod) {
      
      indicator <- as.numeric(values == mod)
      
      bind_cols(
        tibble(
          Panel = panel,
          Variable = v,
          Category = mod,
          Subcategory = groups$subcategory[
            match(v, groups$variable)
          ]
        ),
        calc_stats(indicator, data)
      )
    })
  })
}

# ============================================================
# 7. CONTINUOUS VARIABLES
# ============================================================

stats_continuous <- function(data, variables, groups, panel) {
  
  map_dfr(variables, function(v) {
    
    x <- suppressWarnings(as.numeric(data[[v]]))
    
    bind_cols(
      tibble(
        Panel = panel,
        Variable = v,
        Category = "",
        Subcategory = groups$subcategory[
          match(v, groups$variable)
        ]
      ),
      calc_stats(x, data)
    )
  })
}

# ============================================================
# 8. BINARY VARIABLES
# ============================================================

stats_dummies <- function(data, variables, groups, panel) {
  
  map_dfr(variables, function(v) {
    
    x <- data[[v]]
    
    if (is.logical(x)) {
      
      x <- as.numeric(x)
      
    } else if (is.character(x) || is.factor(x)) {
      
      x_char <- as.character(x)
      
      x <- case_when(
        x_char %in% c("1", "TRUE", "True", "Yes", "yes") ~ 1,
        x_char %in% c("0", "FALSE", "False", "No", "no") ~ 0,
        TRUE ~ NA_real_
      )
      
    } else {
      
      x <- suppressWarnings(as.numeric(x))
    }
    
    bind_cols(
      tibble(
        Panel = panel,
        Variable = v,
        Category = "Yes (= 1)",
        Subcategory = groups$subcategory[
          match(v, groups$variable)
        ]
      ),
      calc_stats(x, data)
    )
  })
}

# ============================================================
# 9. BUILD THE TWO PANELS
# ============================================================

panel_A <- bind_rows(
  stats_categorical(
    Df_table2,
    structural_cat,
    structural_groups,
    "Panel A - Structural characteristics"
  ),
  stats_continuous(
    Df_table2,
    structural_num,
    structural_groups,
    "Panel A - Structural characteristics"
  )
)

panel_B <- bind_rows(
  stats_categorical(
    Df_table2,
    method_cat,
    method_groups,
    "Panel B - Methodological characteristics"
  ),
  stats_dummies(
    Df_table2,
    method_dummy,
    method_groups,
    "Panel B - Methodological characteristics"
  )
)

# ============================================================
# 10. PREPARE RESULT ROWS
# ============================================================

prepare_panel <- function(data, groups) {
  
  data |>
    left_join(
      groups |>
        select(variable, subcategory, order),
      by = c("Variable" = "variable"),
      suffix = c("", "_group")
    ) |>
    mutate(
      Variable_label = coalesce(
        unname(variable_labels[Variable]),
        Variable
      ),
      
      Row_label = case_when(
        Category == "Yes (= 1)" ~ Variable_label,
        Category == "" ~ Variable_label,
        TRUE ~ Category
      )
    ) |>
    arrange(order, Variable, Category)
}

panel_A <- prepare_panel(panel_A, structural_groups)
panel_B <- prepare_panel(panel_B, method_groups)

# ============================================================
# 11. ADD CATEGORY HEADERS AND SORT MODALITIES
# ============================================================

add_category_headers <- function(data, panel_title) {
  
  # Result rows, sorted by descending unweighted mean
  results <- data |>
    mutate(
      Panel = panel_title,
      is_header = FALSE,
      Characteristic = paste0("   ", Row_label)
    ) |>
    select(
      Panel,
      order,
      Subcategory = subcategory,
      Characteristic,
      is_header,
      Mean,
      `Weighted mean (1/n)`,
      `Weighted mean (1/SE)`,
      `Weighted mean (1/SE^2)`,
      SD
    ) |>
    group_by(order, Subcategory) |>
    arrange(desc(Mean), .by_group = TRUE) |>
    mutate(row_order = row_number()) |>
    ungroup()
  
  # Category headers with empty statistics
  headers <- data |>
    distinct(order, subcategory) |>
    mutate(
      Panel = panel_title,
      Characteristic = subcategory,
      is_header = TRUE,
      Mean = NA_real_,
      `Weighted mean (1/n)` = NA_real_,
      `Weighted mean (1/SE)` = NA_real_,
      `Weighted mean (1/SE^2)` = NA_real_,
      SD = NA_real_,
      row_order = 0L
    ) |>
    select(
      Panel,
      order,
      Subcategory = subcategory,
      Characteristic,
      is_header,
      Mean,
      `Weighted mean (1/n)`,
      `Weighted mean (1/SE)`,
      `Weighted mean (1/SE^2)`,
      SD,
      row_order
    )
  
  bind_rows(headers, results) |>
    arrange(order, Subcategory, row_order) |>
    select(-row_order)
}

display_A <- add_category_headers(
  panel_A,
  "Panel A - Structural characteristics"
)

display_B <- add_category_headers(
  panel_B,
  "Panel B - Methodological characteristics"
)

# ============================================================
# 12. BUILD THE FINAL TABLE
# ============================================================

Table2_display <- bind_rows(display_A, display_B) |>
  mutate(
    across(
      c(
        Mean,
        `Weighted mean (1/n)`,
        `Weighted mean (1/SE)`,
        `Weighted mean (1/SE^2)`,
        SD
      ),
      ~ round(.x, 3)
    )
  )

# ============================================================
# 13. FORMAT THE TABLE WITH GT
# ============================================================

Table2_gt <- Table2_display |>
  gt(
    groupname_col = "Panel",
    rowname_col = "Characteristic"
  ) |>
  cols_hide(
    columns = c(is_header, order, Subcategory)
  ) |>
  cols_label(
    Mean = "Mean",
    `Weighted mean (1/n)` = "Weighted mean (1/n)",
    `Weighted mean (1/SE)` = "Weighted mean (1/SE)",
    `Weighted mean (1/SE^2)` = "Weighted mean (1/SE^2)",
    SD = "SD"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = is_header)
  ) |>
  sub_missing(
    columns = c(
      Mean,
      `Weighted mean (1/n)`,
      `Weighted mean (1/SE)`,
      `Weighted mean (1/SE^2)`,
      SD
    ),
    missing_text = ""
  ) |>
  tab_options(
    row_group.font.weight = "bold",
    table.font.size = 11,
    data_row.padding = px(3)
  )

# ============================================================
# 14. EXPORT AND DISPLAY
# ============================================================

gtsave(
  Table2_gt,
  "Data_description/Table2_heterogeneity.html"
)

Table2_gt
