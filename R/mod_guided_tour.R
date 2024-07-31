#' guided_tour UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_guided_tour_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$details(
      summary("Get a Guided Tour"),
      tags$div(
        class = "innerrounded rounded",
        align = "center",
        fluidRow(
          col_12(
            actionButton(
              ns("get"),
              "How to use this dashboard",
              class = "modbutton"
            )
          )
        )
      )
    )

  )
}

#' guided_tour Server Functions
#'
#' @noRd
mod_guided_tour_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_guided_tour_ui("guided_tour_1")

## To be copied in the server
# mod_guided_tour_server("guided_tour_1")
