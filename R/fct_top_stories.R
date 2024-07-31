#' top_stories
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

top_stories <- function(data, interval = c("year", "month", "week"), include_slug = T){

  # set length of time
  num_days <- switch(interval, year = 365.25,
                     month = 30,
                     week = 7)

  if(include_slug){
    top_ytd_cap <- data %>%
      filter(download_date >= as_date(max(.data$download_date)) - num_days)%>%
      select(slug, headline)%>%
      count(slug, headline)%>%
      arrange(desc(n))
  } else {
    top_ytd_cap <- data %>%
      filter(download_date >= as_date(max(.data$download_date)) - num_days)%>%
      select(headline)%>%
      count(headline)%>%
      arrange(desc(n))
  }

  top_ytd_cap %>%
    DT::datatable(., extensions = "Buttons",
                  options = list(pagingType = 'full',
                                 scrollX=TRUE,
                                 searching = TRUE,
                                 ordering = TRUE,
                                 pageLength = 10,
                                 lengthMenu = c(10, 15, 20),
                                 dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf')))

}
