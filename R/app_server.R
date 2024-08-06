#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session,
                       title, product_num, eAP,
                       scr) {
  # Your application server logic

  #   title = "StormChasingVideo.com",
  #   product_number = '100879',
  #   eAP_product_id = '45883',
  #   source = 'stormch'

  #   title = "Latin America News Agency",
  #   eAP_product_id = c('46865', '46866') # first is Eng sec is Span
  #   product_number = c('100973', '100974')
  #   source = 'lana'

  library(DBI)
  library(dbplyr)

  # get both download and upload data as needed
    get_data <- map(c("downloads", "uploads"), ~pull_sql_data(source = golem::get_golem_options("scr"),
                                              eAP_product_id = golem::get_golem_options(which = "eAP"),
                                              last_date = Sys.Date(),
                                              product_num = golem::get_golem_options(which = "prod_num"),
                                              type = .x))


    # order doesn't matter, this just sources the server functions
    # for the other modules
    mod_sunburst_server("sunburst_1", data = get_data[[1]])
    mod_homePage_server("homePage_1")
    mod_upload_ratio_server("upload_ratio_1", tbl_d = get_data[[1]], tbl_u = get_data[[2]])
    mod_dl_trend_server("dl_trend_1", data = get_data[[1]])
    mod_summary_server("summary_1", data = get_data[[1]])
    mod_map_server("map_1", data = get_data[[1]])
    mod_top_stories_server("top_stories_1", data = get_data[[1]])
    mod_dl_data_server("dl_data_1", data = get_data[[1]])



    # Keeps the shiny app from timing out quickly
    autoInvalidate <- reactiveTimer(10000)
    observe({
      autoInvalidate()
      cat(".")
    })

}

