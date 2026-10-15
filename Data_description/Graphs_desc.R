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

theme_graph <- function(base_size = 12, base_family = police) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      text                  = ggplot2::element_text(colour = "grey15"),
      plot.title            = ggplot2::element_text(
        face = "bold", size = base_size + 3, hjust = 0
      ),
      plot.subtitle         = ggplot2::element_text(
        colour = gris, margin = ggplot2::margin(b = 10)
      ),
      plot.caption          = ggplot2::element_text(
        colour = "grey45",
        size = base_size - 3,
        hjust = 0,
        margin = ggplot2::margin(t = 10)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin           = ggplot2::margin(12, 16, 8, 12),
      panel.grid.major.x    = ggplot2::element_blank(),
      panel.grid.minor      = ggplot2::element_blank(),
      panel.grid.major.y    = ggplot2::element_line(
        colour = "grey88", linewidth = 0.3
      ),
      axis.line.x           = ggplot2::element_line(
        colour = "grey20", linewidth = 0.4
      ),
      axis.ticks.x          = ggplot2::element_line(
        colour = "grey20", linewidth = 0.4
      ),
      axis.title            = ggplot2::element_text(
        size = base_size - 1, colour = gris
      ),
      axis.text             = ggplot2::element_text(colour = "grey25"),
      legend.position       = "top",
      legend.justification  = "left",
      legend.title          = ggplot2::element_blank(),
      strip.text            = ggplot2::element_text(
        face = "bold", hjust = 0, size = base_size
      )
    )
}

# --- Nb of estimators and studies evolution ---

Df_year <- Df |>
  group_by(pub_year) |>
  summarise(
    n_etudes = n_distinct(study_id),
    n_estimations = n(),
    .groups = "drop"
  )

src <- "Source: Authors' calculations."

p_etudes <- ggplot(Df_year, aes(pub_year, n_etudes)) +
  geom_col(fill = col_etudes, width = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(subtitle = "Number of studies", x = NULL, y = NULL) +
  theme_graph()

p_obs <- ggplot(Df_year, aes(pub_year, n_estimations)) +
  geom_col(fill = col_estim, width = 0.75) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = label_number(big.mark = ",")
  ) +
  labs(subtitle = "Number of estimates",
       x = "Publication year", y = NULL) +
  theme_graph()

fig_temps <- (p_etudes / p_obs) +
  plot_annotation(
    title = "Evolution of the number of estimates and studies over time",
    caption = src,
    theme = theme_graph()
  )

fig_temps


library(forcats)

graph_se_etude <- function(data, var, titre, xlab, couleur = col_etudes,
                           etude = "study_id", label = etude) {
  
  df_se <- data |>
    mutate(se = suppressWarnings(readr::parse_number(as.character(.data[[var]])))) |>
    filter(!is.na(se)) |>
    group_by(.data[[etude]]) |>
    summarise(se_moy = mean(se),
              n_est  = n(),
              lab    = first(as.character(.data[[label]])),
              .groups = "drop") |>
    mutate(lab = fct_reorder(lab, se_moy))
  
  ggplot(df_se, aes(se_moy, lab)) +
    geom_segment(aes(x = 0, xend = se_moy, yend = lab),
                 colour = "grey75", linewidth = 0.4) +
    geom_point(aes(size = n_est), colour = couleur, alpha = 0.9) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.08)),
                       labels = scales::label_number(big.mark = ",")) +
    scale_size_continuous(name = "Number of estimates", range = c(1.5, 5)) +
    ggplot2::labs(
      title = titre, x = xlab, y = NULL,
      caption = "Source: Authors' calculations. Dashed line: mean across studies."
    ) +
    theme_graph() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        colour = "grey88", linewidth = 0.3
      ),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.line.x = ggplot2::element_line(
        colour = "grey20", linewidth = 0.4
      ),
      legend.title = ggplot2::element_text(
        size = 10, colour = "grey35"
      )
    )
}

fig_se_pcc <- graph_se_etude(Df, "se_pcc",
                             "Mean standard error of the PCC by study",
                             "Mean SE of PCC")

fig_se_elast <- graph_se_etude(Df, "se_elasticity",
                               "Mean standard error of the elasticity by study",
                               "Mean SE of elasticity",
                               couleur = col_estim)

fig_se_pcc
fig_se_elast

library(forcats)

graph_se_pays <- function(data, var, titre, xlab, couleur = col_etudes,
                          etude = "study_id", pays = "data_country") {
  
  df_se <- data |>
    mutate(se = suppressWarnings(readr::parse_number(as.character(.data[[var]]))),
           pays = as.character(.data[[pays]])) |>
    filter(!is.na(se), !is.na(pays)) |>
    group_by(pays, etude = .data[[etude]]) |>        # moyenne par étude (au sein d'un pays)
    summarise(se_moy = mean(se), n_est = n(), .groups = "drop") |>
    arrange(pays, se_moy) |>
    mutate(cle  = paste(pays, etude, sep = "___"),   # clé unique étude x pays
           cle  = factor(cle, levels = unique(cle)),
           pays = factor(pays, levels = sort(unique(pays))))   # ordre alphabétique
  
  libelles <- setNames(as.character(df_se$etude), df_se$cle)
  
  ggplot(df_se, aes(se_moy, cle)) +
    geom_segment(aes(x = 0, xend = se_moy, yend = cle),
                 colour = "grey80", linewidth = 0.4) +
    geom_point(aes(size = n_est), colour = couleur, alpha = 0.9) +
    facet_grid(pays ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_y_discrete(labels = libelles) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.08)),
                       labels = scales::label_number(big.mark = ",")) +
    scale_size_continuous(name = "Number of estimates", range = c(1.5, 5)) +
    labs(title = titre, x = xlab, y = NULL,
         caption = "Source: Authors' calculations. Dashed line: mean across studies.") +
    theme_graph() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
          panel.spacing.y    = unit(0.5, "lines"),     # plus d'air entre pays
          strip.placement    = "outside",
          strip.text.y.left  = element_text(angle = 0, hjust = 1, face = "bold", size = 8),
          axis.text.y        = element_text(size = 6.5),
          legend.title       = element_text(size = 10, colour = "grey35"))
}

fig_se_pays <- graph_se_pays(Df, "se_elasticity",
                             "Mean standard error of the elasticities by study, grouped by country",
                             "Mean SE of elasticity")
fig_se_pays

n_lignes <- Df |>
  mutate(se = suppressWarnings(readr::parse_number(as.character(se_pcc)))) |>
  filter(!is.na(se), !is.na(data_country)) |>
  distinct(data_country, study_id) |>
  nrow()
ggsave("fig_se_pays.png", fig_se_pays, width = 8,
       height = max(5, 0.2 * n_lignes + 1.5), dpi = 300, bg = "white")