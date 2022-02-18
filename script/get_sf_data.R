library(readr)
library(tidyr)
library(dplyr)
library(salesforcer)
library(fuzzyjoin)

# Authenticate using username, password, and security token ...
sf_auth(username = Sys.getenv("FIND_SALESFORCE_USERNAME"),
        password = Sys.getenv("FIND_SALESFORCE_PASSWORD"),
        security_token = Sys.getenv("FIND_SALESFORCE_SECURITY_TOKEN"))

# find a report in your org and run it
# all_reports <- sf_query("SELECT Id, Name FROM Report")

# all_reports |>
#   filter(grepl("Cov", Name, ignore.case = TRUE))

covid_report_id <- '00O6900000CRNLeEAP'
sf_data <- sf_run_report(covid_report_id)
# sf_data


meta_cols <-
  readr::read_csv("../shinytestdir/test_directory/data/meta_cols.csv", show_col_types = FALSE) |>
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
  left_join(select(country_info, alpha3, country = name, who_region), by = "alpha3")

data <-
  data_raw |>
  mutate(across(everything(), ~ gsub("-", NA_character_, .))) |>
  rename(name = country) |>
  fuzzyjoin::regex_left_join(country_map, by = c("name" = "regex"), ignore_case = TRUE) |>
  select(-name, -region, -alpha3) |>
  rename(region = who_region)

write_csv(data, "../shinytestdir/test_directory/data/data.csv")
