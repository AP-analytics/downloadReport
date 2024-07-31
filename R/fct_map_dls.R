#' map_dls
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


map_dls <- function(map_df, map_bins, map_color){

  pal <- leaflet::colorBin(map_color, domain = map_df$n, bins = map_bins,
                           na.color = "lightgrey")

  # include label for countries that weren't observed in the dataset
  # otherwise they will say NA
  labels <- sprintf(
    "<strong>%s</strong><br/>%g downloads",
    map_df$sales_country, case_when(is.na(map_df$n) ~ 0, TRUE ~ map_df$n)
  ) %>% lapply(htmltools::HTML)

  leaflet::leaflet(map_df) %>%
    leaflet::addTiles(options = leaflet::providerTileOptions(minZoom = 1)) %>%
    leaflet::addPolygons(fillColor = ~pal(n), weight = 2,
                         color = 'black',
                         label = labels,

                         opacity = 0.2,
                         fillOpacity = 0.7,
                         highlightOptions = leaflet::highlightOptions(color = "white", weight = 2)) %>%
    leaflet::addLegend(pal = pal, values = n, position = "bottomleft",
                       title = "Downloads",
                       labFormat = leaflet::labelFormat(big.mark = ','),
                       opacity = 0.9) %>%
    suppressWarnings()


}
