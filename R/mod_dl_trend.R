#' dl_trend UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dl_trend_ui <- function(id){
  ns <- NS(id)
  tabPanel(div(
    div(class="fa fa-line-chart", role = "navigation"),
    "Trend"),
    value = "trend",
    sidebarPanel(width=4,
                 column(6,
                        # define help button
                        actionButton(ns("help_trend"),label="Help", icon= icon('question-circle'), class ="down")),
                 column(6,
                        # define definition button
                        actionButton(ns("defs_trend"), label="Definitions", icon= icon('info'), class ="down")),
                 column(12,
                        # horizontal break
                        shiny::hr(),
                        # select indicator from list
                        div(selectInput(ns("indic_trend"), shiny::HTML("<p>Select length of time."),
                                        choices=c("one year", "two years", "three years"), selected = "one year")),
                        # horizontal break
                        shiny::hr(),
                 column(12,
                        downloadButton(ns('trend_data'), 'Download data', class = "down"),
                        actionButton(ns('save_plot'), 'Save plot', icon = icon("image"), class = "down")))),
    mainPanel(width = 8, #Main panel
              br(),
              br(),
              shinycssloaders::withSpinner(plotly::plotlyOutput(ns("trend_plot")))
    )
  ) #Tab panel bracket
   #TagList
}

#' dl_trend Server Functions
#'
#' @noRd
mod_dl_trend_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    library(shinyBS)

    observeEvent(input$help_trend, {

      showModal(modalDialog(
        title = "How to use this chart",
        p("The trend chart is designed to explore how download concentration has changed over time."),

                 p("First select the time frame you'd like to see."),
                 p(HTML("The black line shows the <b>thirty-day rolling average number of downloads</b>.")),
                 p("The blue line shows a smoothed trend that makes it easier to see high and low points."),
                 p("Hover over the plot to see individual data points."),
                 p("To download the chart as a PNG file, hover over the upper right corner and click the camera icon."),
        size = "l", easyClose = TRUE, fade=FALSE,footer = modalButton("Close")))
    })


    observeEvent(input$save_plot, {
      showModal(modalDialog(p("To download the chart as a PNG file, hover over the upper right corner and click the camera icon."),
                            easyClose = T, footer = modalButton("Close")))
    })


    ###############################################.
    ## Reactive data ----
    ###############################################.



    time <- reactive({switch(input$indic_trend,
                   'one year' = 1,
                   'two years' = 2,
                   'six months' = 0.5,
                   'three years' = 3)})


    trend_data <- reactive({

      data %>%
        select(slug, download_date)%>%
        filter(download_date >= as.Date(max(download_date, na.rm = T)) - 365.25*time()) %>%
        timetk::tk_augment_timeseries_signature() %>%
        suppressMessages()%>%
        mutate(download_date = as.Date(download_date)) })






    #####################.
    # Creating plot ----
    #####################.

    #####################.
    #Plot

    create_plot <- function(){
      if(!is.data.frame(trend_data()) && nrow(trend_data) == 0){
        plot_nodata()
      } else {
        trend_data() %>%
          group_by(download_date)%>%
          summarise(n = n()) %>%
          ungroup() %>%
          # Apply Sliding Function
          mutate(rolling_avg_30 = timetk::slidify_vec(n,  ~ mean(., na.rm = T),
                                                      .period = 30, .partial = TRUE))%>%
          timetk::plot_time_series(download_date, rolling_avg_30,
                                   .interactive = T)
      }
       }

    #Creating time trend plot

    output$trend_plot <- plotly::renderPlotly({create_plot() %>%
        layout(title = HTML(paste0("Trend in 30-day rolling average of downloads<br><sup> From ",
                                   as.Date(max(trend_data()$download_date)) - time(), " to ",
          as.Date(max(trend_data()$download_date)), "</sup>")),
               xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                 title = "Time",      # xaxis's title: /r/reference/#layout-xaxis-title
                 showgrid = F # removes vertical grid lines
                 ),
               yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                 title = "Average downloads",
                 rangemode = 'tozero')) %>%
        config(modeBarButtons = list(list('toImage')), displaylogo = F)
      })

    trend_csv <- reactive({df <- create_plot() %>% plotly::plotly_data() %>%
        select(1, 2, 3)

        names(df) <- c("Date", "Number of downloads", "30-day rolling average of downloads")

        df})

    output$trend_data <- downloadHandler(filename =  'timetrend_data.csv',
                                            content = function(file) {write.csv(trend_csv(), file, row.names=FALSE)})

})
}


    ###############################################.
    ## Downloads ----
    ###############################################.
    #Downloading data
    # trend_csv <- reactive({ format_csv(trend_data()) })
    #
    #
    #
    # # Downloading chart
    # output$download_trendplot <- downloadHandler(
    #   filename = 'trend.png',
    #   content = function(file){
    #     export(p = plot_trend_chart() %>%
    #              layout(title = paste0(input$indic_trend), margin = list(t = 140)),
    #            file = file, zoom = 3)
    #   })


## To be copied in the UI
# mod_dl_trend_ui("dl_trend_1")

## To be copied in the server
# mod_dl_trend_server("dl_trend_1")
