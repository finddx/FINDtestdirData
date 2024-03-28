library(readr)
library(tidyr)
library(dplyr)
library(fuzzyjoin)

sf_data <- readr::read_csv("https://raw.githubusercontent.com/finddx/FINDtestdirData/report/testdir.csv")

meta_cols <-
  readr::read_csv("data/covid19/testdir_meta_cols.csv", show_col_types = FALSE) |>
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


#COVARIANTS#
#Get all unique covariants per row in a new column
data$impact_all <- apply(data[, c("no_expected_impact", "potential_impact", "no_impact", "impact")], 1, function(x) paste(unique(x), collapse = "; "))
#create a vector listing all the covariants
covariants <- data |>
  select(impact_all) |>
  separate_rows(impact_all, sep="; ") |>
  unique() |>
  pull()
covariants <- setdiff(covariants, c("None","Not applicable"))
#Compare  the unique covariants per row column against the list of covariants and assign in a new column the covariants not listed in the column
data$impact_unk <- sapply(strsplit(data$impact_all, "; "), function(x) paste(setdiff(covariants, x), collapse = "; "))
#Transform None and Not applicable to NA
data <- data |>
  select(-c(impact_all)) |>
  mutate_at(c("no_expected_impact", "potential_impact", "no_impact", "impact"), ~ gsub("None|Not applicable", NA, .)) |>
  mutate(impact_unk = ifelse(impact_unk=="", NA, impact_unk))
data <- data |>
  distinct()
#Transform impact data to long format
data <- data |>
  pivot_longer(cols=c(no_expected_impact, potential_impact, no_impact, impact, impact_unk),
               names_to = "impact_type",
               values_to = "impact_value") |>
  separate_rows(impact_value, sep="; ") |>
  arrange(submission_id, impact_value) |>
  filter(!is.na(impact_value))
#Re transform impact data to wide format
data <- data |>
  pivot_wider(names_from = impact_value,
              names_prefix = "sc2_impact_",
              values_from = impact_type)
#Rename impact values
data <- data |>
  mutate(across(starts_with("sc2_impact_"), ~case_when(
      .=="no_expected_impact" ~ "No expected impact (in silico analyses)",
      .=="potential_impact" ~ "Potential impact (in silico analyses)",
      .=="impact" ~ "Impact (analytical/clinical studies)",
      .=="no_impact" ~ "No impact (analytical/clinical studies conducted)",
      .=="impact_unk" ~ "Unknown",
      TRUE ~ .
    )
  ))
#Rename vars
names(data) <- gsub("\\+", "_plus", names(data))
names(data) <- gsub("\\.|\\(|\\)", "", names(data))
names(data) <- gsub(" ", "_", names(data))
names(data) <- tolower(names(data))

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
  mutate(disease_target = "Covid-19") |>
  distinct() |>
  filter(!is.na(manufacturer)) |>

  mutate(target_analyte = replace_na(target_analyte, "Unknown")) |>
  mutate(scov2_variants = replace_na(scov2_variants, "Unknown")) |>
  mutate(validated_sample_types = stringr::str_replace_all(validated_sample_types, c("Feces" = "Faeces")))

write_csv(raw, "data/covid19/testdir.csv")
