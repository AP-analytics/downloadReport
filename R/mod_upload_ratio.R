#' upload_ratio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_ratio_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    h3("Downloads by Date"),
    p("You can click the buttons to change the time span, or drag the slider below for a more custom time frame."),
    mainPanel(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns('dl_ul_bar')))
  )
  )

}

#' upload_ratio Server Functions
#'
#' @noRd
mod_upload_ratio_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data2 <- data %>%
      # Brad wants a darker section for the weekend to make it
      # easier to see what is going on with the 30 day trend
      filter(download_date >= as_date(max(download_date)) - 30*3)%>%
      timetk::tk_augment_timeseries_signature()%>%
      # removes the sec, min, hr part of the date that was messing up aggregate #s
      mutate(download_date = as.Date(download_date))%>%
      group_by(download_date, wday.lbl)%>%
      reframe(n = n())%>%
      ungroup()%>%
      mutate(type = case_when(
               startsWith(as.character(wday.lbl), "S") ~ "Weekend",
               TRUE ~ "Work Day"
             )) %>%
      suppressMessages()

    if (all(data2$type == 'Work Day')){
      cols <- rev(c("#F8766D", '#00BFC4'))
    } else {
      cols <- c("#F8766D", '#00BFC4')
    }


    output$dl_ul_bar <- plotly::renderPlotly({

      fig1 <- plotly::plot_ly(
        data = data2,
        x = ~download_date,
        y = ~n,
        color  = ~type,
        type = 'bar',
        colors = c("#F8766D", '#00BFC4')
      )

      fig2 <- plotly::plot_ly(
        data = uploads_nflr_05_2024%>%
          filter(arrival_date_time >= as_date(max(data2$download_date)) - 90)%>%
          mutate(download_date = as.Date(arrival_date_time))%>%
          group_by(arrival_date_time)%>%
          reframe(n = n())%>%
          ungroup(),
        x = ~arrival_date_time,
        y = ~n,
        type = 'bar',
        marker = list(color = 'rgb(158,202,225)'),
        showlegend=F
      ) %>%
        layout(
          yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
            title = "Uploads"))

      subplot(fig1, fig2, nrows = 2, heights = c(0.6,0.4), shareX = T, margin = 0.03)%>%
        layout(xaxis = list(title = "Date",
                            rangeselector=list(default = '1m',
                                               buttons=list(
                                                 list(count=1, label="1 mo.", step="month", stepmode="backward"),
                                                 list(count=3, label="3 mo.", step="month", stepmode="backward"))),
                            type = "date"),
               yaxis2 = list(title = "Uploads"),
               yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                 title = "Downloads"),
               legend = list(title = list(text = "Day Type")))%>%
        config(modeBarButtons = list(list("toImage")), displaylogo = F)

        })
    # p2 <- ggplot(data = uploads) +
    #   geom_bar(data = uploads, mapping = aes(x = arrival_date_time))+
    #   theme_classic()+
    #   xlab("Upload Date")+
    #   ylab("# Uploads")+
    #   scale_x_date(limits = c(as_date(params$last_date) - 30, as_date(params$last_date)), breaks = c(as_date(params$last_date) - 30, as_date(params$last_date) - 20, as_date(params$last_date) - 10, as_date(params$last_date)), date_labels = "%b %d")+
    #   scale_y_continuous(expand = c(0,0))

    # p1/p2 + plot_layout(ncol = 1, heights = c(3, 1))


  })
}

## To be copied in the UI
# mod_upload_ratio_ui("upload_ratio_1")

## To be copied in the server
# mod_upload_ratio_server("upload_ratio_1")
