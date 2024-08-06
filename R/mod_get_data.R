#' get_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_get_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' get_data Server Functions
#'
#' @noRd 
mod_get_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_get_data_ui("get_data_1")
    
## To be copied in the server
# mod_get_data_server("get_data_1")
