#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd


rename_asset_actions <- function(x){
  x %>%
    mutate(asset_action_id = case_when(
      asset_action_id %in% c(2, 4) ~ "buy_now",
      asset_action_id == 21 ~ "credit",
      asset_action_id == 8 ~ "within_age_download",
      asset_action_id == 1 ~ "metered",
      # authoring a textbook, which could take years,
      # but you only pay if they end up in the final copy
      # then they will pay out from those pictures
      # sales rep manually sets the price for each one
      # download status = downloaded not bought
      # if = billed then it was payed out
      asset_action_id == 6 ~ "OAEnabled",

      asset_action_id == 16 ~ "eCom",
      asset_action_id == 18 ~ "price_metered",
      asset_action_id == 19 ~ "price_metered_overage",
      TRUE ~ as.character(asset_action_id)
    ))
}



# 1. title wrapper function --------------------------------------------------
# purpose: ensures titles not cut-off when downloading ggplot as png
title_wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}


savechart_button <- function(outputId, label = "Save chart", class=NULL, disabled = FALSE){

  tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
         href = "", target = "_blank", download = NA, icon("image"), label)


}


plot_nodata <- function(height_plot = 450) {
  text_na <-
    list(x = 5,y = 5,
         text = "No data available" ,
         size = 20,
         xref = "x",
         yref = "y",
         showarrow = FALSE
    )

  plot_ly(height = height_plot) %>%
    layout(
      annotations = text_na,
      #empty layout
      yaxis = list(
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        fixedrange = TRUE
      ),
      xaxis = list(
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        fixedrange = TRUE
      ),
      font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')
    ) %>%
    config(displayModeBar = FALSE) # taking out plotly logo and collaborate button
}


append_geo <- function(data){
  large <- data %>%
    mutate(sales_country = ifelse(sales_country == 'U.A.E.', "United Arab Emirates", sales_country),
           sales_country = ifelse(sales_country == 'Korea', 'South Korea', sales_country),
           sales_country = ifelse(sales_country == 'Macau S.A.R.', 'Macao S.A.R.', sales_country)) %>%
    inner_join(downloadReport::large_countries, by = 'sales_country')

  small <- data %>%
    mutate(sales_country = ifelse(sales_country == 'U.A.E.', "United Arab Emirates", sales_country),
           sales_country = ifelse(sales_country == 'Korea', 'South Korea', sales_country),
           sales_country = ifelse(sales_country == 'Macau S.A.R.', 'Macao S.A.R.', sales_country)) %>%
    inner_join(downloadReport::small_countries, by = 'sales_country')

  return(list(small_geo = small, large_geo = large))
}

