# --- Libraries ---
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gt)
library(purrr)

# --- Loading the dataset ---
Df <- read.csv("Data_description/Df_clean.csv")

# --- Tables aesthetic ---
Aesthetic <- function(gt_tbl) {
  gt_tbl |>
    opt_table_font(font = list("Times New Roman", "Times", "serif")) |>
    tab_options(
      table.font.size = px(12),
      heading.title.font.size = px(13),
      heading.title.font.weight = "bold",
      heading.align = "left",
      column_labels.font.weight = "bold",
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      table.border.top.style = "hidden",
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(2),
      table.border.bottom.color = "black",
      table_body.hlines.style = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = px(1),
      table_body.border.bottom.color = "black",
      data_row.padding = px(4),
      column_labels.padding = px(4)
    ) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left", columns = 1) |>
    tab_style(
      style = cell_text(size = px(10), style = "italic"),
      locations = cells_source_notes()
    )
}

# --- Descriptive statistical calculations : Categorical variables --- 
stats_cat <- function(data, var, etude = NULL) {
  total_lignes <- nrow(data)
  total_etudes <- if (is.null(etude)) NA_integer_ else n_distinct(data[[etude]])
  
  out <- data |>
    mutate(modalite = coalesce(as.character(.data[[var]]), "Missing")) |>
    group_by(modalite) |>
    summarise(
      n_lignes = n(),
      n_etudes = if (is.null(etude)) NA_integer_ else n_distinct(.data[[etude]]),
      .groups = "drop"
    ) |>
    mutate(
      pct_lignes = round(100 * n_lignes / total_lignes, 1),
      pct_etudes = round(100 * n_etudes / total_etudes, 1)
    ) |>
    select(modalite, n_lignes, pct_lignes, n_etudes, pct_etudes) |>
    arrange(desc(n_lignes))
  
  if (is.null(etude)) out <- select(out, -n_etudes, -pct_etudes)
  out
}

# --- Descriptive statistical calculations : Numerical variables --- 
stats_num <- function(data, vars) {
  map_dfr(vars, function(v) {
    x <- data[[v]]
    tibble(
      variable = v,
      n        = sum(!is.na(x)),
      manquant = sum(is.na(x)),
      moy_et   = sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
      mediane  = sprintf("%.2f [%.2f–%.2f]", median(x, na.rm = TRUE),
                         quantile(x, .25, na.rm = TRUE), quantile(x, .75, na.rm = TRUE)),
      min_max  = sprintf("%.2f – %.2f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
    )
  })
}

# --- Descriptive statistical calculations : Dummy variables --- 

stats_dummy <- function(data, vars, etude = NULL) {
  total_lignes <- nrow(data)
  total_etudes <- if (is.null(etude)) NA_integer_ else n_distinct(data[[etude]])
  
  out <- map_dfr(vars, function(v) {
    x <- data[[v]]
    x <- if (is.logical(x)) as.numeric(x) else suppressWarnings(as.numeric(as.character(x)))
    pos <- !is.na(x) & x == 1
    
    tibble(
      variable   = v,
      n_lignes   = sum(pos),
      pct_lignes = round(100 * sum(pos) / total_lignes, 1),
      n_etudes   = if (is.null(etude)) NA_integer_ else n_distinct(data[[etude]][pos]),
      pct_etudes = round(100 * n_etudes / total_etudes, 1)
    )
  })
  
  if (is.null(etude)) out <- select(out, -n_etudes, -pct_etudes)
  out
}

# --- Table : publication status --- 

tableau_pub_status <-
  stats_cat(Df, "pub_status", etude = "study_id") |>
  mutate(modalite = recode(modalite,
                           "published"     = "Published",
                           "working_paper" = "Working paper",
                           "unpublished"   = "Unpublished",
                           "Missing"       = "Not reported")) |>
  gt() |>
  cols_label(
    modalite   = "Publication status",
    n_lignes   = "Number of Estimates",
    pct_lignes = "% of Estimates",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Publication Status Across Included Studies") |>
  Aesthetic()

tableau_pub_status

# --- Table : country --- 

tableau_country <-
  stats_cat(Df, "data_country", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Country",
    n_lignes   = "Number of Estimates",
    pct_lignes = "% of Estimates",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Publication Status Across Included Studies") |>
  Aesthetic()

tableau_country

# --- Table : development --- 

tableau_development <-
  stats_cat(Df, "development", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Country",
    n_lignes   = "Number of Estimates",
    pct_lignes = "% of Estimates",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Development Level Across Included Studies") |>
  Aesthetic()

tableau_development

# --- Table : Effect type --- 

tableau_effect_type <-
  stats_cat(Df, "effect_type_raw", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Effect type raw",
    n_lignes   = "Number of Estimates",
    pct_lignes = "% of Estimates",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Raw Effect Type Across Included Studies") |>
  tab_source_note("Note: a study can report several effect types") |>
  Aesthetic()

tableau_effect_type

# --- Table : estimates characteristics ---

tableau_num <- stats_num(Df, vars = c("t_stat", "n_obs", "dof", "pcc", 
                                      "se_pcc", "elasticity", "se_elasticity")) |>
  mutate(variable = recode(variable,
                           "poll_mean"   = "Mean PM2.5 (µg/m³)",
                           "pub_year"    = "Publication year",
                           "sample_size" = "Sample size")) |>
  gt() |>
  cols_label(
    variable = "Variable",
    n        = "N",
    manquant = "Missing",
    moy_et   = "Mean (SD)",
    mediane  = "Median [Q1–Q3]",
    min_max  = "Min – Max"
  ) |>
  tab_header(title = "Descriptive Statistics of Numeric Variables") |>
  Aesthetic()

tableau_num

# --- Table : pollutants --- 

tableau_pollutants <-
  stats_cat(Df, "pollutant", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Pollutants Across Included Studies") |>
  tab_source_note("Note: a study can report several pollutants.") |>
  Aesthetic()
  
tableau_pollutants

# --- Table : exposure measure --- 

tableau_exp_measure <-
  stats_cat(Df, "exposure_measure", etude = "study_id") |>
  mutate(modalite = recode(modalite,
                           "fixed_station" = "Fixed Station",
                           "satellite" = "Satellite",
                           "Missing"       = "NA")) |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Exposure Measure Across Included Studies") |>
  Aesthetic()

tableau_exp_measure

# --- Table : indoor/outdoor measure --- 

tableau_in_out <-
  stats_cat(Df, "indoor_outdoor_measure", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Indoor/Outdoor Measure Across Included Studies") |>
  Aesthetic()

tableau_in_out

# --- Table : outcome category --- 

tableau_outcome_cat <-
  stats_cat(Df, "outcome_category", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Outcome Category Across Included Studies") |>
  Aesthetic()

tableau_outcome_cat

# --- Table : outcome level --- 

tableau_outcome_level <-
  stats_cat(Df, "outcome_level", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Outcome Level Across Included Studies") |>
  Aesthetic()

tableau_outcome_level

# --- Table : Task Type --- 

tableau_task_type <-
  stats_cat(Df, "task_type", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Task Type Included Studies") |>
  Aesthetic()

tableau_task_type

# --- Table : sector --- 

tableau_sector <-
  stats_cat(Df, "sector", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Sector Across Included Studies") |>
  Aesthetic()

tableau_sector

# --- Table : urban / rural --- 

tableau_urban_rural <-
  stats_cat(Df, "urban_rural", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Rural / Urban Across Included Studies") |>
  Aesthetic()

tableau_urban_rural

# --- Table : sample characteristics ---

tableau_sample_chara <- stats_num(Df, vars = c("age_mean", "pct_female")) |>
  mutate(variable = recode(variable,
                           "poll_mean"   = "Mean PM2.5 (µg/m³)",
                           "pub_year"    = "Publication year",
                           "sample_size" = "Sample size")) |>
  gt() |>
  cols_label(
    variable = "Variable",
    n        = "N",
    manquant = "Missing",
    moy_et   = "Mean (SD)",
    mediane  = "Median [Q1–Q3]",
    min_max  = "Min – Max"
  ) |>
  tab_header(title = "Descriptive Statistics of Sample Characteristics") |>
  Aesthetic()

tableau_sample_chara 

# --- Table : time frequency --- 

tableau_time_freq <-
  stats_cat(Df, "time_frequency_observation", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Time Frequency Across Included Studies") |>
  Aesthetic()

tableau_time_freq

# --- Table : data structure --- 

tableau_data_structure <-
  stats_cat(Df, "data_structure", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Data Structure Across Included Studies") |>
  Aesthetic()

tableau_data_structure

# --- Table : identification strategy --- 

tableau_id_strat <-
  stats_cat(Df, "id_strategy", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Identification Strategy Across Included Studies") |>
  Aesthetic()

tableau_id_strat

# --- Table : instrument type --- 

tableau_instrument <-
  stats_cat(Df, "instrument_type", etude = "study_id") |>
  gt() |>
  cols_label(
    modalite   = "Pollutant",
    n_lignes   = "Number of Estimators",
    pct_lignes = "% of Estimators",
    n_etudes   = "Number of Studies",
    pct_etudes = "% of Studies"
  ) |>
  tab_header(title = "Distribution of Instrument Types Across Included Studies") |>
  Aesthetic()

tableau_instrument

# --- Tables : Fixed Effects / Controls ---
# --- 1. Groups ---

grp_fe <- c(
  time_FE         = "Time",
  worker_FE       = "Worker",
  firm_FE         = "Firm",
  localisation_FE = "Location"
)

grp_weather <- c(
  controls_weather       = "Any weather control",
  control_precipitation  = "Precipitation",
  control_temp           = "Temperature",
  control_radiation      = "Radiation",
  control_wind_speed     = "Wind speed",
  control_wind_direction = "Wind direction",
  control_humidity       = "Humidity",
  control_air_pressure   = "Air pressure",
  control_snow           = "Snow",
  control_dew_points     = "Dew point",
  control_skycover       = "Sky cover",
  control_sunshine       = "Sunshine"
)

grp_pollutants <- c(
  controls_multipollutant = "Any other pollutant control",
  control_PM2.5           = "PM2.5",
  control_PM10            = "PM10",
  control_O3              = "O3",
  control_CO              = "CO",
  control_NO2             = "NO2",
  control_SO2             = "SO2"
)

grp_controls <- c(
  controls_worker_characteristics = "Worker characteristics",
  attendance_bias_treated         = "Attendance bias treated",
  controls_traffic_effect         = "Traffic effect",
  controls_demand_effect          = "Demand effect",
  controls_wage_shirking          = "Wage / shirking"
)

# --- 2. Fonction ---

tableau_dummy_gt <- function(data, groupe, titre, premiere_col, etude = "study_id") {
  stats_dummy(data, vars = names(groupe), etude = etude) |>
    mutate(variable = unname(groupe[variable])) |>
    gt() |>
    cols_label(
      variable   = premiere_col,
      n_lignes   = "Number of Estimates",
      pct_lignes = "% of Estimates",
      n_etudes   = "Number of Studies",
      pct_etudes = "% of Studies"
    ) |>
    tab_header(title = titre) |>
    Aesthetic()
}

# --- 3. Tables ---

tab_fe <- tableau_dummy_gt(Df, grp_fe,
                           "Fixed Effects Included in the Estimates",
                           "Fixed effects")
tab_fe

tab_weather <- tableau_dummy_gt(Df, grp_weather,
                                "Weather Controls Included in the Estimates",
                                "Weather control")
tab_weather

tab_pollutants <- tableau_dummy_gt(Df, grp_pollutants,
                                   "Other Pollutant Controls Included in the Estimates",
                                   "Pollutant control")
tab_pollutants

tab_controls <- tableau_dummy_gt(Df, grp_controls,
                                 "Other Controls Included in the Estimates",
                                 "Control")
tab_controls