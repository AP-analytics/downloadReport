#' top_stories UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_top_stories_ui <- function(id){
  ns <- NS(id)

  tabPanel(
    div(
      div(class="fa fa-medal", role = "navigation"),
      'Top Stories'),
    value = "stories",

    fluidPage(
      titlePanel(
    h3("Explore the most downloaded stories across all content users")
    ),

    sidebarPanel(
      shinyWidgets::pickerInput(
        inputId = ns("time"),
        label = "Time Frame",
        choices = c("Past Year" = "year", "Past Month" = "month", "Past Week" = "week")
      ),
      br(),
      p(HTML("<b>Important note:</b> This display works best if the slugs assigned to content at ingestion are more generic categories and not a replica of the headline. Click the button below to add or remove slug from the tables as needed.")
               ),
      fluidRow(shinyWidgets::switchInput(
        inputId = ns("slug"),
        label = "Include Slug?",
        onLabel = "Yes",
        offLabel = "No",
        labelWidth = "120px",
        value = T
      ), align = 'center')
    ),

    mainPanel(
      br(),
      h4(textOutput(ns("title"))),
      DT::DTOutput(ns("stories"))

    )

    )
  )

}

#' top_stories Server Functions
#'
#' @noRd
mod_top_stories_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    length <- reactive({input$time})

    time <- reactive({switch(input$time,
                     "year" = 365.25,
                     "month" = 30,
                     "week" = 7)})


    slug_yn <- reactive({input$slug})


    output$stories <- DT::renderDT({top_stories(data, length(), slug_yn())})

    output$title <- renderText({paste0("Top stories from ",
                                       as.Date(max(data$download_date)) - time(), " to ",
                                       as.Date(max(data$download_date)))})

  })
}

## To be copied in the UI
# mod_top_stories_ui("top_stories_1")

## To be copied in the server
# mod_top_stories_server("top_stories_1")
