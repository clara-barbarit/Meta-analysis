library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(patchwork)

Df <- read_xlsx("Data_description/Codage_articles.xlsx")

#Nb d'études et d'éstimations dans le temps

Df_year <- Df |>
  group_by(pub_year) |>
  summarise(
    n_etudes = n_distinct(study_id),
    n_estimations = n(),
    .groups = "drop"
  ) |>
  pivot_longer(-pub_year, names_to = "type", values_to = "n")

ggplot(Df_year, aes(x = pub_year, y = n, colour = type)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_colour_manual(
    values = c(n_etudes = "#2C7BB6", n_estimations = "#D7191C"),
    labels = c("Estimations", "Etudes")
  ) +
  labs(x = "Année", y = "Nombre", colour = NULL) +
  theme_minimal()

Df_year <- Df |>
  group_by(pub_year) |>
  summarise(
    n_etudes = n_distinct(study_id),
    n_obs = n(), 
    .groups = "drop"
  )

## Graphique 1 : nombre d'études par an
p_etudes <- ggplot(Df_year, aes(x = pub_year, y = n_etudes)) +
  geom_col(fill = "#2C7BB6") +
  labs(x = "Année", y = "Nombre d'études",
       title = "Nombre d'études par an") +
  theme_minimal()

## Graphique 2 : nombre d'observations par an
p_obs <- ggplot(Df_year, aes(x = pub_year, y = n_obs)) +
  geom_col(fill = "#D7191C") +
  labs(x = "Année", y = "Nombre d'observations",
       title = "Nombre d'observations par an") +
  theme_minimal()

# Nombre d'études par continent / pays
police <- "Times New Roman"
robin  <- "+proj=robin"

Df <- Df |>
  mutate(iso3 = countrycode(data_country, origin = "country.name", destination = "iso3c"))

monde <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name != "Antarctica")

continents <- monde |>
  st_drop_geometry() |>
  distinct(adm0_a3, continent)

Df_ok <- Df |> filter(!is.na(iso3))

# Sous-base PM2.5 uniquement (tolère "PM2.5", "PM25", "pm 2.5", etc.)
Df_pm25 <- Df_ok |>
  filter(pollutant == "PM2.5") |>
  mutate(poll_mean = readr::parse_number(as.character(poll_mean),
                                         locale = readr::locale(decimal_mark = "."))) |>
  filter(!is.na(poll_mean))

nrow(Df_pm25)
summary(Df_pm25$poll_mean)

# ---- Comptages par pays (toute la base) ----
df_pays <- Df_ok |>
  group_by(iso3) |>
  summarise(n_etudes = n_distinct(study_id),
            n_est    = n(),
            .groups = "drop")

# PM2.5 moyen par pays : moyenne par étude, puis moyenne entre études
poll_pays <- Df_pm25 |>
  group_by(iso3, study_id) |>
  summarise(m_etude = mean(poll_mean), .groups = "drop") |>
  group_by(iso3) |>
  summarise(poll_moy = mean(m_etude), .groups = "drop")

df_pays <- df_pays |> left_join(poll_pays, by = "iso3")

# ---- Comptages par continent ----
df_cont <- Df_ok |>
  left_join(continents, by = c("iso3" = "adm0_a3")) |>
  group_by(continent) |>
  summarise(n_etudes = n_distinct(study_id),
            n_est    = n(),
            .groups = "drop")

poll_cont <- Df_pm25 |>
  left_join(continents, by = c("iso3" = "adm0_a3")) |>
  group_by(continent, study_id) |>
  summarise(m_etude = mean(poll_mean), .groups = "drop") |>
  group_by(continent) |>
  summarise(poll_moy = mean(m_etude), .groups = "drop")

df_cont <- df_cont |> left_join(poll_cont, by = "continent")

carte <- monde |>
  left_join(df_pays, by = c("adm0_a3" = "iso3"))

# ---- Position des étiquettes ----
ancres <- tribble(
  ~continent,       ~nom,               ~lon, ~lat, ~cote, ~dy,
  "North America",  "Amérique du Nord", -100,  45,  "L",   0,
  "South America",  "Amérique du Sud",   -60, -15,  "L",   0,
  "Europe",         "Europe",             15,  50,  "T",   0,
  "Asia",           "Asie",               90,  45,  "R",   0,
  "Africa",         "Afrique",            20,   5,  "R",  -2.5e6
)

xy <- ancres |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(robin) |>
  st_coordinates()

xout <- 17.5e6
ytop <- 9.3e6

# ---- Fonction générique ----
faire_carte <- function(var, unite, titre_legende,
                        palette = "Blues", trans = "sqrt", digits = 0) {
  lab <- ancres |>
    mutate(x = xy[, 1], y = xy[, 2]) |>
    left_join(df_cont, by = "continent") |>
    mutate(
      val   = .data[[var]],
      texte = ifelse(is.na(val), "n.d.",
                     paste0(formatC(val, format = "f", digits = digits), " ", unite)),
      label = paste0(nom, "\n", trimws(texte)),
      xl = case_when(cote == "L" ~ -xout, cote == "R" ~ xout, TRUE ~ x),
      yl = case_when(cote == "T" ~ ytop, TRUE ~ y + dy),
      hj = case_when(cote == "L" ~ 1, cote == "R" ~ 0, TRUE ~ 0.5)
    )
  
  ggplot() +
    geom_sf(data = carte, aes(fill = .data[[var]]),
            colour = "grey40", linewidth = 0.1) +
    geom_text(data = lab, aes(x = xl, y = yl, label = label, hjust = hj),
              family = police, size = 3.8, lineheight = 0.95) +
    scale_fill_distiller(
      palette = palette, direction = 1,
      na.value = "grey95", trans = trans,
      name = titre_legende
    ) +
    coord_sf(crs = robin, xlim = c(-23.5e6, 23.5e6), ylim = c(-7.5e6, 10.5e6),
             expand = FALSE) +
    theme_void(base_family = police) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1.5, "cm"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
}

p_etudes <- faire_carte("n_etudes", "études",      "Nombre d'études")
p_est    <- faire_carte("n_est",    "estimations", "Nombre d'estimations")
p_poll   <- faire_carte("poll_moy", "µg/m³",       "PM2.5 moyen (µg/m³)", trans = "identity", digits = 1)

p_etudes; p_est; p_poll