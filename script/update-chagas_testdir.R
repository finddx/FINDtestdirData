library(readr)
library(tidyr)
library(dplyr)
library(fuzzyjoin)

sf_data <- readr::read_csv("https://raw.githubusercontent.com/finddx/FINDtestdirData/report/chagas_testdir.csv")
meta_cols <-
  readr::read_csv("data/ntd/chagas_testdir_meta_cols.csv", show_col_types = FALSE) |>
  filter(salesforce_name %in% colnames(sf_data))

data_raw <-
  sf_data |>
  select({ meta_cols$salesforce_name }) |>
  rename_with(~ meta_cols$id, meta_cols$salesforce_name)


# country_info <- shinyfind::get_country_info()
#
# country_map <-
#   countrycode::codelist %>%
#   as_tibble() %>%
#   select(
#     regex = country.name.en.regex, alpha3 = iso3c
#   ) |>
#   left_join(select(country_info, alpha3, country = name, continent), by = "alpha3")


extract_link <- function(x) {
  x <- gsub("^.+title=\"", "", x)
  x <- gsub(" \\(New Window\\).+", "", x)
  x
}


data <-
  data_raw |>
  mutate(across(everything(), ~as.character(.))) |>
  mutate(across(everything(), ~na_if(., "-"))) |>
  # rename(name = country) |>
  # fuzzyjoin::regex_left_join(country_map, by = c("name" = "regex"), ignore_case = TRUE) |>
  # select(-name, -region, -alpha3) |>
  # rename(region = continent) |>
  mutate(permalink = extract_link(permalink)) |>
  mutate(permalink = if_else(startsWith(permalink, "http"), permalink, paste0("https://", permalink)))


d <-
  data |>
  mutate(country_tmp = gsub("Korea, Republic of", "South Korea", country)) |>
  mutate(city = gsub("Unknown", NA, city)) |>
  #mutate(city2 = coalesce(city, country)) |>
  mutate(city2 = if_else(is.na(city), country_tmp, paste0(city, ", ", country_tmp)))

geo_data <-
  d |>
  tidygeocoder::geocode(city2, method = 'bing', lat = lat , long = lng)

geo_data <-
  geo_data |>
  mutate(city2 = gsub("South Korea", "Korea, Republic of", city2))

raw <-
  geo_data |>
  mutate(sensitivity = gsub("^0$", NA, sensitivity)) |>
  mutate(specificity = gsub("^0$", NA, specificity)) |>
  distinct() |>
  filter(!is.na(manufacturer)) |>
  mutate(target_analyte = replace_na(target_analyte, "Unknown")) |>
  mutate(validated_sample_types = stringr::str_replace_all(validated_sample_types, c("Feces" = "Faeces")))

write_csv(raw, "data/ntd/chagas_testdir.csv")

