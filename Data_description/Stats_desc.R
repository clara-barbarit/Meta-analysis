tableau_pollutants <- bdd |>
  group_by(pollutant) |>
  summarise(
    nb_lignes = n(),
    nb_etudes = n_distinct(study_id)
  ) |>
  arrange(desc(nb_etudes))

tab <- tableau_pollutants |>
  gt() |>
  cols_label(
    pollutant  = "Pollutant",
    nb_lignes  = "Number of Records",
    nb_etudes  = "Number of Studies"
  ) |>
  tab_header(
    title = "Distribution of Pollutants Across Included Studies"
  ) |>
  # Police Times New Roman partout
  opt_table_font(
    font = list(
      google_font("Times New Roman"),
      "Times New Roman", "Times", "serif"
    )
  ) |>
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
  cols_align(align = "center", columns = c(nb_lignes, nb_etudes)) |>
  cols_align(align = "left", columns = pollutant) |>
  tab_style(
    style = cell_text(size = px(10), style = "italic"),
    locations = cells_source_notes()
  )