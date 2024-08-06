#' sunburst UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sunburst_ui <- function(id){
  ns <- NS(id)

  library(plotly)

  fluidPage(

    # Application title
    titlePanel("Download Breakdown"),

    sidebarLayout(


      # Sidebar with a slider input
      sidebarPanel(

        fluidRow(id = 'row1_sun',
                 column(12, align = 'center', id = 'buttons',
                        actionButton(ns("help_sun"),label="Help", icon= icon('question-circle'), class ="down"),
                        actionButton(ns("def_sun"), label = "Definitions", icon = icon("info"), class = "down"),
                        hr())),
        div(selectInput(ns("sun_time"), shiny::HTML("<p>Select length of time."),
                        choices=c("one month", "six months", "one year", "two years"), selected = "one year"), align = 'center')


      ),

      # Show a plot of the generated distribution
      mainPanel(
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns('plot')))
      )
    )
  )

}

#' sunburst Server Functions
#'
#' @noRd
mod_sunburst_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$help_sun, {

            showModal(modalDialog(
              title = "How to use this chart",
              p("A sunburst chart is a more interactive version of a pie chart."),

                       p("Each ring represents a characteristic of the downloads."),
                       p("The inner ring will show the proportion of downloads that were used by TV vs Internet companies."),
                       p("The second ring shows what region the download occured in."),
                       p("As you move away from the center, you can investigate how these two variables relate to one another."),
                       p("For example, if you trace your cursor to the TV segment, and then move up to the second ring and see a Sub-Saharan Africa segment, you've found all downloads in the time period you selected that were both TV and within Sub-Saharan Africa."),
                       p("Let's say you are only interested in TV downloads. You can click on the TV segment of the first ring and the chart will automatically update. This is where you would find percentages of downloads that are in a specific region that only use TV downloads in their calculation."),
                       p("To return to the first version of the chart, click in the center circle."),
              easyClose = T, footer = modalButton("Close")
              ))
          })


    observeEvent(input$def_sun, {

      showModal(modalDialog(
        title = "Definitions of variables",
        p("Broadcast company type highlights were content was distributed. Options are TV or Internet."),
        p("Possible regions of the world include E. Asia/Pacific, Europe/Central Asia, Latin America/the Caribbean,
        Middle East & North Africa, North America, South Asia, and Sub-Saharan Africa."),
        easyClose = T, footer = modalButton("Close")
      ))
    })



    time <- reactive({switch(input$sun_time,
                             "one month" = 1/12,
                             "six months" = 0.5,
                             "one year" = 1,
                             "two years" = 2,
                             "three years" = 3)})

    sun_data <- reactive({

      data %>%
        filter(download_date >= as.Date(Sys.Date()) -
                 365.25*time())%>%
        dplyr::mutate(content = case_when(
          # will need to double check that this logic works but for now
          company_type == "Television" ~ "TV",
          TRUE ~ "Internet"
        ),
        sales_country = ifelse(is.na(sales_country), "Missing", sales_country))%>%
        left_join(large_countries %>%
                    as.data.frame()%>%
                    select(sales_country, region_wb) %>%
                    rbind(small_countries %>%
                            as.data.frame()%>%
                            select(sales_country, region_wb)) %>%
                    distinct())%>%
        mutate(region_wb = ifelse(is.na(region_wb) & sales_country == 'Missing', 'Missing',
                                  region_wb))%>%
        dplyr::mutate(seqs = paste(content, region_wb, sep = "-"))


    })


    output$plot <- plotly::renderPlotly(
      plot_ly(
        labels = c("All",
                   count(sun_data(), content) %>% pull(content),
                   count(sun_data(), content, region_wb, seqs) %>% pull(region_wb)),
        ids = c("All",
                count(sun_data(), content) %>% pull(content),
                count(sun_data(), content, region_wb, seqs) %>% pull(seqs)),
        parents = c("", rep("All", length(count(sun_data(), content) %>% pull(content))),
                    count(sun_data(), content, region_wb, seqs) %>% pull(content)),
        values = c(sun_data() %>% nrow(),
                   count(sun_data(), content) %>% pull(n),
                   count(sun_data(), content, region_wb, seqs) %>% pull(n)),
        type = 'sunburst',
        branchvalues = 'total',
        hoverinfo="label+percent root",
        insidetextorientation='radial',
        width = 600,
        height = 600
        # ,
        # hovertemplate = 'Downloads: %{values}'
      ) %>%
        layout(title = HTML(paste0("Downloads from ",
                                   as.Date(max(sun_data()$download_date)) - 365.25*time(), " to ",
                                   as.Date(max(sun_data()$download_date)), "<br><br>")))%>%
        config(modeBarButtons = list(list('toImage')), displaylogo = F)
  )
  }
)
}

## To be copied in the UI
# mod_sunburst_ui("sunburst_1")

## To be copied in the server
# mod_sunburst_server("sunburst_1")
