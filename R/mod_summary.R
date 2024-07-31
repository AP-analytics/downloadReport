#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ui <- function(id){
  ns <- NS(id)
  tabPanel(

    div(
      div(class="fa fa-list-ul", role = "navigation"),
      "Summary"),

      sidebarPanel(width=4,
                column(12,
                        # select indicator from list
                        div(title="Select from the list below to see more information. Click in this box, hit backspace and start to type if you want to quickly find a variable.",
                            shinyWidgets::awesomeCheckboxGroup(ns("var_choice"), shiny::HTML("<p>Step 1. Select an indicator <br/></p>"),
                                        choices=c("Downloads",
                                          "Users",
                                          "Stories",
                                                  "Countries",
                                                  "Regions",
                                                  "Markets",
                                                  "Organizations",
                                          "Company Types"),
                                        select = 'Downloads')),
                       hr(),
                       div(selectInput(ns("time_sum_choice"), shiny::HTML("<p>Step 2. Select time frames to investigate <br/></p>"),
                                       choices=c("one week", "one month", "one year", "two years"), selected = "one month")
                       ),
                       br(),
                       actionButton(ns("clear"), label = "Clear all filters",  icon ("eraser"), class = "down"),
                       downloadButton(ns("download_table_csv"),
                                      'Download data', class = "down")
                )),
    mainPanel(width = 8, #Main panel
              shinycssloaders::withSpinner(gt::gt_output(ns("tbl1"))),
              textOutput(ns("txt"))
    )

)

}

#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    time <- reactive({switch(input$time_sum_choice, "one year" = 1*365.25,
                   "two years" = 2*365.25,
                   "one month" = 30,
                   "one week" = 7)})

    title <- reactive({
      list(as.Date(max(data$download_date)) - time(),
           as.Date(max(data$download_date)))

    })

    table <- reactive({
      data %>%
        select(download_date, asset_action_id,
               user_name, story_number, company_type, organisation_name, sales_region, geo)%>%
        filter(download_date > as.Date(max(download_date)) - time())%>%
        group_by(asset_action_id)%>%
        summarise(across(everything(), ~unique(.) %>% length()),
                  Downloads = n()) %>%
        ungroup()%>%
        rename_asset_actions()%>%
        pivot_longer(!c(asset_action_id))%>%
        pivot_wider(names_from = 'asset_action_id',
                    values_from = 'value')%>%
        left_join(
          data %>%
            filter(download_date > as.Date(max(download_date)) - time())%>%
            select(user_name, story_number, company_type, organisation_name, sales_region, geo) %>%
            summarise(across(everything(), ~unique(.) %>% length()),
                      Downloads = n()) %>%
            pivot_longer(everything())
        ) %>%
        rename(total = value)%>%
        mutate(name = fct_relevel(as.factor(name), "Downloads", "organisation_name",
                                  "user_name", "story_number", "company_type",
                                  "sales_region", "geo")%>%
                 fct_recode(., "Organizations" = 'organisation_name','Users' = 'user_name',
                            'Stories' = 'story_number', 'Company Types' = 'company_type',
                            'Regions' = 'sales_region', 'Markets' = 'geo'))%>%
        filter(name %in% c(input$var_choice)) %>%
        arrange(name)
        })



    # group_by(asset_action_id)%>%
    #   summarise(across(everything(), ~unique(.) %>% length()),
    #             Downloads = n()) %>%
    #   ungroup()%>%
    #   rename_asset_actions()%>%
    #   pivot_longer(!c(asset_action_id))%>%
    #   pivot_wider(names_from = 'asset_action_id',
    #               values_from = 'value')%>%
    #   left_join(
    #     data %>%
    #       select(input$sum_var_choice) %>%
    #       summarise(across(everything(), ~unique(.) %>% length()),
    #                 Downloads = n()) %>%
    #       pivot_longer(everything())
    #   ) %>%
    #   rename(total = value)


    output$tbl1 <- gt::render_gt({table()%>%
        gt::gt()%>%
        gt_theme_dlr_ap() %>%
        gt::tab_header(
          title = html("Summary of Video Content"),
          subtitle = html(paste0("From ", title()[[1]],
                                 " to ", title()[[2]]))) })


      observeEvent(input$clear, {
        shinyWidgets::updateAwesomeCheckboxGroup(session, "var_choice",
                                                 label = NULL, selected = 'Downloads')

        updateSelectInput(session, "time_sum_choice", selected = "one month")

        })

  # Downloading data in csv format

  #The filters the user applies in the data table will determine what data they download - indicator tab table
  output$download_table_csv <- downloadHandler(
    filename ="data_extract.csv",
    content = function(file) {
      write.csv(table(),
                file, row.names=FALSE) })


})

}

## To be copied in the UI
# mod_summary_ui("summary_1")

## To be copied in the server
# mod_summary_server("summary_1")
