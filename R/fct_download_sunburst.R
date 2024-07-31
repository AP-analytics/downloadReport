#' download_sunburst
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

download_sunburst <- function(data2){


  catData <- data2 %>%
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


  circle1 <- count(catData, content)

  circle2 <- count(catData, content, region_wb, seqs)

  return(list(circle1 = circle1, circle2 = circle2))

}
