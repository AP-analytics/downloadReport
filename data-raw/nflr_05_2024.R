## code to prepare `nflr_05_2024` dataset goes here

library(DBI)
library(dbplyr)
library(tidyverse)


params <- list(
  product_number = '100766',
  eAP_product_id = '45451',
  last_date = Sys.Date(),
  source = "newsflare"
)

con <- dbConnect(odbc::odbc(),
                 driver = 'SQL Server',
                 server = Sys.getenv("SQL_SERV"),
                 database = Sys.getenv("SQL_DB"),
                 , uid = Sys.getenv("SQL_USER"), pwd = Sys.getenv("SQL_PASS"),
                 timeout = 10)

nflr_data <- tbl(con, "vw_VideoHubDownloads") %>%
  janitor::clean_names()  %>%
  dplyr::select(story_number, partner_id, created_date_utc, user_name, slug, news_categories, topics, organisation_name, organisation_id, download_format, geo, company_type, sales_country, sales_region, title, item_id, download_date, headline, creation_date_time, caption, sources, organisation_type, e_ap_product_id, usage_id, service_keys, product_number, product_name, rate, is_duplicate, asset_action_id, download_status) %>%
  filter(sql(paste0("eAP_ProductID in (", paste0(params$eAP_product_id, collapse = ", "), ")")))%>%
  filter(sql(paste0("downloadDate between dateadd(year, -3, '", params$last_date, "') and dateadd(day, 1, '", params$last_date, "')")))%>%
  filter(is_duplicate != TRUE & asset_action_id != 11 & !download_status %in% c("Voided", "Failed"))

# to collect you can only grab 20 vars at a time, so bind them together
nflr_data <- cbind(
  nflr_data %>% select(1:20) %>% collect(),
  nflr_data %>% select(21:last_col()) %>% collect()
) %>%
  mutate(download_date = as.Date(download_date))

uploads_nflr_data <- tbl(con, "vw_VideoHubContent") %>%
  janitor::clean_names()%>%
  filter(sql(paste0("(productList like '%", parse_number(params$eAP_product_id), "%' or productList like '%",
                    word(params$eAP_product_id, 2), ",%') and sources like '%", params$source, "%'")) &
           sql(paste0("ArrivalDateTime between dateadd(year, -2, '",
                      params$last_date, "') and '", params$last_date, "'"))) %>%
  mutate(arrival_date_time = as.Date(arrival_date_time))%>%
  select(arrival_date_time) %>%
  collect()

uploads_nflr_data <- uploads_nflr_data %>%
  mutate(arrival_date_time = make_date(year(arrival_date_time), month(arrival_date_time),
                                       day(arrival_date_time))) %>%
  mutate(weekday = as.character(wday(arrival_date_time, label = T)),
         weekend = case_when(
           startsWith(weekday, "S") ~ "Weekend",
           TRUE ~ "Work Day"
         ))




usethis::use_data(nflr_data, overwrite = TRUE)
usethis::use_data(uploads_nflr_data, overwrite = TRUE)
