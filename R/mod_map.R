#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tabPanel(div(
    div(class="fa fa-globe", role = "navigation"),
    'Map'),
    value = "trend",
      fluidPage(


        sidebarPanel(

          shinyWidgets::pickerInput(
            inputId = ns("time_frame"),
            label = "Time Frame",
            choices = c("Past Year", "Past Month", "Past Week")
          ),
          hr(),
          p("Grey indicates 0 downloads.")
        ),
        mainPanel(
          h4("Download Concentration by Country"),
          h5(textOutput(ns('title_map'))),
          shinycssloaders::withSpinner(leaflet::leafletOutput(ns("download_map"))
        )

      )))
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, data){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    reac <- reactiveValues()


    observe({
      if(input$time_frame == "Past Year"){
        reac$map_pal <- "YlGnBu"

        if(all(data$sources == "stormchasingvideo.com")){
          reac$bin_nums <- c(1, 15, 30, 45, Inf)
        } else {
          reac$bin_nums <- c(1, 10, 50, 100, 500, 1000, Inf)
        }

        reac$days <- 365.25
      } else if (input$time_frame == "Past Month") {
        reac$map_pal <- "Blues"
        reac$bin_nums <- c(1, 10, 20, 30, 40, Inf)
        reac$days <- 30
      } else if (input$time_frame == "Past Week"){
        reac$map_pal <- "Purples"
        reac$bin_nums <- c(1, 4, 8, 12, Inf)
        reac$days <- 7
      }


    })



    dl_data <- reactive({data %>%
      filter(download_date >= as.Date(Sys.Date()) - reac$days)%>%
      count(sales_country)})

    df <- reactive({downloadReport::large_countries %>%
      left_join(dl_data())%>%
      sf::st_sf()})

    output$title_map <- renderText({paste0("From ",
                                       as.Date(Sys.Date()) - reac$days, " to ",
                                       as.Date(Sys.Date()))})


    output$download_map <- leaflet::renderLeaflet({

      map_dls(map_df = df(), map_bins = reac$bin_nums, map_color = reac$map_pal)})



  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
