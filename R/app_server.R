#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  #   title = "StormChasingVideo.com",
  #   product_number = '100879',
  #   eAP_product_id = '45883',
  #   last_date = '2024-05-31',
  #   source = 'stormch'

  library(DBI)
  library(dbplyr)
  # this source is abbreviated because I'm using the like operator
    get_data <- purrr::map(c("downloads", "uploads"), ~pull_sql_data(source = 'stormch',
                                              eAP_product_id = '45883',
                                              last_date = Sys.Date(),
                                              product_num = '100879',
                                              type = .x))

    mod_sunburst_server("sunburst_1", data = get_data[[1]])
    mod_homePage_server("homePage_1")
    mod_upload_ratio_server("upload_ratio_1", tbl_d = get_data[[1]], tbl_u = get_data[[2]])
    mod_dl_trend_server("dl_trend_1", data = get_data[[1]])
    mod_summary_server("summary_1", data = get_data[[1]])
    mod_map_server("map_1", data = get_data[[1]])
    mod_top_stories_server("top_stories_1", data = get_data[[1]])
    mod_dl_data_server("dl_data_1", data = get_data[[1]])
}
