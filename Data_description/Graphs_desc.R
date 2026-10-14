# --- Libraries --------------------------------------
library(tidyverse)
library(patchwork)
library(scales)
library(sf)
library(rnaturalearth)
library(countrycode)

# --- Loading the dataset ----------------------------
Df <- read.csv("Data_description/Df_clean.csv")

# --- Graphs aesthetic -------------------------------

police <- "Times New Roman"

# Palette sobre
col_etudes <- "#1F4E79"   # bleu profond
col_estim  <- "#C0504D"   # rouge brique
gris       <- "grey35"

theme <- function(base_size = 12, base_family = police) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text                  = element_text(colour = "grey15"),
      plot.title            = element_text(face = "bold", size = base_size + 3, hjust = 0),
      plot.subtitle         = element_text(colour = gris, margin = margin(b = 10)),
      plot.caption          = element_text(colour = "grey45", size = base_size - 3,
                                           hjust = 0, margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin           = margin(12, 16, 8, 12),
      panel.grid.major.x    = element_blank(),
      panel.grid.minor      = element_blank(),
      panel.grid.major.y    = element_line(colour = "grey88", linewidth = 0.3),
      axis.line.x           = element_line(colour = "grey20", linewidth = 0.4),
      axis.ticks.x          = element_line(colour = "grey20", linewidth = 0.4),
      axis.title            = element_text(size = base_size - 1, colour = gris),
      axis.text             = element_text(colour = "grey25"),
      legend.position       = "top",
      legend.justification  = "left",
      legend.title          = element_blank(),
      strip.text            = element_text(face = "bold", hjust = 0, size = base_size)
    )
}

# --- Nb of estimators and studies evolution --- 

Df_year <- Df |>
  group_by(pub_year) |>
  summarise(n_etudes = n_distinct(study_id),
            n_estimations = n(), .groups = "drop")

src <- "Source: Authors' calculations."

p_etudes <- ggplot(Df_year, aes(pub_year, n_etudes)) +
  geom_col(fill = col_etudes, width = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(subtitle = "Number of studies", x = NULL, y = NULL) +
  theme_ofce()

p_obs <- ggplot(Df_year, aes(pub_year, n_estimations)) +
  geom_col(fill = col_estim, width = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     labels = label_number(big.mark = ",")) +
  labs(subtitle = "Number of estimates", x = "Publication year", y = NULL) +
  theme_ofce()

fig_temps <- (p_etudes / p_obs) +
  plot_annotation(
    title   = "Evolution of the number of estimates and studies over time",
    caption = src,
    theme   = theme_ofce()
  )
fig_temps