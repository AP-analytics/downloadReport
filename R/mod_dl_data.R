#' dl_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dl_data_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    titlePanel(h3(" ")),
    br(),
    br(),
    mainPanel(
      fluidRow(align = 'center', DT::DTOutput(ns("tbl_data"))
    )
  )
  )
}

#' dl_data Server Functions
#'
#' @noRd
mod_dl_data_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    tbl <- data %>%
      select(headline, slug, organisation_name, sales_country, news_categories)%>%
      mutate(across(everything(), ~ifelse(is.na(.), "Missing", .)))

    names(tbl) <- c("Headline", "Slug", "Organization", "Country", "News Category")

    output$tbl_data <- DT::renderDT(
        tbl
        , extensions = "Buttons",
        options = list(pagingType = 'full',
                       scrollX=TRUE,
                       searching = TRUE,
                       ordering = TRUE,
                       pageLength = 10,
                       lengthMenu = c(10, 15, 20),
                       dom = 'Blfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf'))


    )


  })
}

## To be copied in the UI
# mod_dl_data_ui("dl_data_1")

## To be copied in the server
# mod_dl_data_server("dl_data_1")
