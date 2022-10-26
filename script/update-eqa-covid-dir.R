library(readr)
library(tidyr)
library(dplyr)
library(salesforcer)
library(fuzzyjoin)

sf_auth(
  username = Sys.getenv("FIND_SALESFORCE_USERNAME"),
  password = Sys.getenv("FIND_SALESFORCE_PASSWORD"),
  security_token = Sys.getenv("FIND_SALESFORCE_SECURITY_TOKEN")
)


all_reports <- sf_query("SELECT Id, Name FROM Report")
eqa_report_id <- '00O6900000CGKWKEA5'

sf_data <- sf_run_report(eqa_report_id)

meta_cols <-
  readr::read_csv("data/eqa_covid_testdir_meta_cols.csv", show_col_types = FALSE) |>
  filter(salesforce_name %in% colnames(sf_data))

data_raw <-
  sf_data |>
  select({ meta_cols$salesforce_name }) |>
  rename_with(~ meta_cols$id, meta_cols$salesforce_name)


country_info <- shinyfind::get_country_info()

country_map <-
  countrycode::codelist %>%
  as_tibble() %>%
  select(
    regex = country.name.en.regex, alpha3 = iso3c
  ) |>
  left_join(select(country_info, alpha3, country = name, continent), by = "alpha3")


extract_link <- function(x) {
  x <- gsub("^.+title=\"", "", x)
  x <- gsub(" \\(New Window\\).+", "", x)
  x
}


data <-
  data_raw |>
  mutate(across(everything(), ~na_if(., "-"))) |>
  mutate(permalink = extract_link(permalink)) |>
  mutate(permalink = if_else(startsWith(permalink, "http"), permalink, paste0("https://", permalink)))

write_csv(data, "data/eqa_covid_testdir.csv")
