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
    get_data <- pull_sql_data(source = 'stormch',
                                              eAP_product_id = '45883',
                                              last_date = '2024-07-31',
                                              product_num = '100879',
                                              type = "downloads")

    mod_sunburst_server("sunburst_1", data = get_data)
    mod_homePage_server("homePage_1")
    mod_upload_ratio_server("upload_ratio_1", data = get_data)
    mod_dl_trend_server("dl_trend_1", data = get_data)
    mod_summary_server("summary_1", data = get_data)
    mod_map_server("map_1", data = get_data)
    mod_top_stories_server("top_stories_1", data = get_data)
    mod_dl_data_server("dl_data_1", data = get_data)
}
