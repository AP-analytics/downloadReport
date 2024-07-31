#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

    get_data <- downloadReport::pull_sql_data()

    mod_sunburst_server("sunburst_1", data = )
    mod_homePage_server("homePage_1")
    mod_upload_ratio_server("upload_ratio_1", data = downloadReport::nflr_05_2024)
    mod_dl_trend_server("dl_trend_1", data = downloadReport::nflr_05_2024)
    mod_summary_server("summary_1", data = downloadReport::nflr_05_2024)
    mod_map_server("map_1", data = downloadReport::nflr_05_2024)
    mod_top_stories_server("top_stories_1", data = downloadReport::nflr_05_2024)
    mod_dl_data_server("dl_data_1", data = downloadReport::nflr_05_2024)
}
