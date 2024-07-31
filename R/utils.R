gt_theme_dlr_ap <- function(data, ...) {
  library(gt)

  data %>%
    # Relabel columns
    cols_label(
      starts_with("buy_now") ~ "Buy Now",
      starts_with("credit") ~ "Credit Model",
      starts_with("meter") ~ "Metered",
      starts_with("within_age") ~ "Within Age Download",
      starts_with("total") ~ "Total",
      name = ""
    ) %>%
    # if missing, replace NA w/ ---
    sub_missing() %>%
    # add exact color from PFF table to spanners
    # hide spanner with transparent color
    # Change font color and weight for numeric col
    # tab_style(
    #   style = list(
    #     cell_text(color = "#3a3d42", weight = "bold")
    #   ),
    #   locations = cells_body(
    #     columns = 5:9
    #   )
    # ) %>%
    # Add pound sign in front of numbers
    # text_transform(
    #   locations = cells_body(
    #     columns = vars(number)
    #   ),
    #   fn = function(x) {
    #     paste0("#", x)
    #   }
    # ) %>%
    # Make column labels and spanners all caps
    opt_all_caps() %>%
    # add row striping
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "#585d63",
      table_body.hlines.color = "transparent",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "transparent",
      row.striping.background_color = "#f9f9fb",
      data_row.padding = px(3),
      ...
    ) %>%
    # cols_width(
    #   1 ~ px(75),
    #   2 ~ px(125),
    #   3 ~ px(30),
    #   4 ~ px(40),
    #   everything() ~ px(60)
    # ) %>%
    # change color of border separating the text from the sourcenote
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "#585d63", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = nrow(data$`_data`)
      )
    ) %>%
    # change font to Lato throughout (note no need to have Lato locally!)
    # opt_table_font(
    #   font = c(
    #     google_font(name = "Lato"),
    #     default_fonts()
    #   )
    # ) %>%
    # add source note
    tab_source_note(
      source_note = md("Data can be used as an approximate estimate of royalties, but may not match later reports due to refunds, fraud, etc. (get more language)")
    )
}
