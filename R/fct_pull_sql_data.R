#' pull_sql_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.



pull_sql_data <- function(source, eAP_product_id, product_num,
                          last_date, type = c("downloads", "uploads")){


  if (source != 'newsflare'){
    con <- dbConnect(odbc::odbc(),
                   driver = 'SQL Server',
                   server = Sys.getenv("SQL_SERV"),
                   database = Sys.getenv("SQL_DB"),
                   , uid = Sys.getenv("SQL_USER"), pwd = Sys.getenv("SQL_PASS"),
                   timeout = 10)

    if (type == "downloads"){

      save <- tbl(con, "vw_VideoHubDownloads") %>%
        janitor::clean_names()  %>%
        filter(sql(paste0("eAP_ProductID in (", paste0(eAP_product_id, collapse = ", "), ") or productNumber in (",
                          product_num, ")")))%>%
        filter(sql(paste0("downloadDate between dateadd(year, -3, '", last_date, "') and dateadd(day, 1, '", last_date, "')")))%>%
        filter(is_duplicate != TRUE & asset_action_id != 11 & !download_status %in% c("Voided", "Failed"))%>%
        select(slug, organisation_id,
               organisation_name, story_number,
               company_type, sales_country,
               download_date, headline, user_name,
               geo, sales_region, news_categories,
               asset_action_id, sources,
               partner_id) %>%
        collect() %>%
        mutate(sources = stringr::str_to_lower(stringr::str_trim(sources)),
               download_date = as.Date(download_date))

    } else if (type == "uploads"){

      tbl(con, "vw_VideoHubContent") %>%
        janitor::clean_names()%>%
        filter(sql(paste0("(productList like '%", parse_number(eAP_product_id), "%' or productList like '%",
                          word(eAP_product_id, 2), ",%') and sources like '%", source, "%'")) & sql(paste0("ArrivalDateTime between dateadd(year, -2, '",
                                                                                                           last_date, "') and '", last_date, "'"))) %>%
        select(arrival_date_time) %>%
        collect()%>%
        mutate(arrival_date_time = make_date(year(arrival_date_time), month(arrival_date_time),
                                             day(arrival_date_time))) %>%
        mutate(weekday = as.character(wday(arrival_date_time, label = T)),
               weekend = case_when(
                 startsWith(weekday, "S") ~ "Weekend",
                 TRUE ~ "Work Day"
               ))


    }

  } else {

    if (type == 'downloads'){
      nflr_05_2024
    } else {
      uploads_nflr_05_2024
    }

  }





}
