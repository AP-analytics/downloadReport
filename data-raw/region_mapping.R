
library(rworldmap)

# currently using GEO3 but adding Middle East
large_region <- rnaturalearthdata::countries110 %>%
  mutate(sales_country = case_when(
    # goes from their version to APs version of the name
    # but sometimes AP will spell things both ways like for Macau/Macao
    admin == 'Brunei' ~ 'Brunei Darussalam',
    admin == 'Ivory Coast' ~ "Cote d'Ivoire",
    admin == 'Gaza' ~ 'Gaza Strip',
    admin == 'Pakistan' ~ 'Islamic Republic of Pakistan',
    admin == 'Korea' ~ 'South Korea',
    admin == 'Macau S.A.R.' ~ 'Macao S.A.R.',
    admin == 'North Macedonia' ~ "Macedonia (FYROM)",
    admin == 'China' ~ "People's Republic of China",
    admin == 'Republic of Serbia' ~ 'Serbia',
    admin == 'Saint Lucia' ~ 'St. Lucia',
    admin == 'Palestine' ~ 'Gaza Strip',
    admin == 'United States of America' ~ 'United States',
    admin == 'Czechia' ~ formal_en,
    TRUE ~ admin)
  )

attr(large_region, 'sf_column') <- 'geometry'

small_region <- rnaturalearthdata::tiny_countries110 %>%
  mutate(sales_country = case_when(
    admin == 'Brunei' ~ 'Brunei Darussalam',
    admin == 'Ivory Coast' ~ "Cote d'Ivoire",
    admin == 'Gaza' ~ 'Gaza Strip',
    admin == 'Pakistan' ~ 'Islamic Republic of Pakistan',
    admin == 'Korea' ~ 'South Korea',
    admin == 'Macau S.A.R.' ~ 'Macao S.A.R.',
    admin == 'Macedonia' ~ "Macedonia (FYROM)",
    admin == 'China' ~ "People's Republic of China",
    admin == 'Republic of Serbia' ~ 'Serbia',
    admin == 'Saint Lucia' ~ 'St. Lucia',
    admin == 'Palestine' ~ 'Gaza Strip',
    admin == 'United States of America' ~ 'United States',
    TRUE ~ admin)
  )

attr(small_region, 'sf_column') <- 'geometry'

################## to check which countries existed in last 6 years ###########
################## and AP naming conventions ##################################
# con <- DBI::dbConnect(odbc::odbc(),
#                  driver = 'SQL Server',
#                  server = Sys.getenv("SQL_SERV"),
#                  database = Sys.getenv("SQL_DB"),
#                  , uid = Sys.getenv("SQL_USER"), pwd = Sys.getenv("SQL_PASS"),
#                  timeout = 10)
#
#
# region_mapping <- tbl(con, "vw_VideoHubDownloads") %>%
#   janitor::clean_names()%>%
#   filter(download_date >= max(download_date) - 6*365.25)%>%
#   count(sales_country, sales_region) %>%
#   select(-n)%>%
#   collect()



abbrev_reg <- function(data){
  data %>%
    mutate(region_wb = case_when(
      region_wb == 'East Asia & Pacific' ~ "E. Asia/Pacific",
      region_wb == 'Europe & Central Asia' ~ "Euro./Cent. Asia",
      region_wb == 'Latin America & Caribbean' ~ "Lat. Amer./Caribbean",
      region_wb == 'Middle Ease & North Africa' ~ "Middle E./N. Afr.",
      region_wb == 'North America' ~ "N. Amer.",
      region_wb == 'South Asia' ~ "S. Asia",
      TRUE ~ region_wb

    ))
}



large_countries <- large_region %>%
  select(sales_country, region_wb)%>%
  filter(!sf::st_is_empty(geometry)) %>%
  abbrev_reg()

# small ones only have points not multipolygon shape files
small_countries <- small_region %>%
  select(sales_country, region_wb) %>%
  abbrev_reg()





usethis::use_data(large_countries, overwrite = TRUE)
usethis::use_data(small_countries, overwrite = TRUE)
