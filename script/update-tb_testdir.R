library(readr)
library(tidyr)
library(dplyr)
#library(salesforcer)
library(fuzzyjoin)
library(stringr)

set.seed(5)


# Authenticate using username, password, and security token ...
# sf_auth(
#   username = Sys.getenv("FIND_SALESFORCE_USERNAME"),
#   password = Sys.getenv("FIND_SALESFORCE_PASSWORD"),
#   security_token = Sys.getenv("FIND_SALESFORCE_SECURITY_TOKEN")
# )
# # find a report in your org and run it
sf_data <- readr::read_csv("https://raw.githubusercontent.com/finddx/FINDtestdirData/report/tb_testdir.csv")

colnames(sf_data) <- str_replace(string = colnames(sf_data), replacement = '', 'Parent Submission: ')


meta_cols <-
  readr::read_csv("data/tb/tb_testdir_meta_cols.csv", show_col_types = FALSE) |>
  filter(salesforce_name %in% colnames(sf_data))

data_raw <-
  sf_data |>
  select({ meta_cols$salesforce_name }) |>
  rename_with(~ meta_cols$id, meta_cols$salesforce_name)

#
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
  # mutate(region=continent)|>
  # select(-name, -alpha3, -continent) |>#!rcontinent removed, not renamed as region already existed
  # rename(region = continent) |>
  mutate(permalink = extract_link(permalink)) |>
  mutate(permalink = if_else(startsWith(permalink, "http"), permalink, paste0("https://", permalink))) |>
  relocate(submission_id, .before = submission_title)


data <- data |>
  filter(test_to_be_listed_on_finds_web_page=="Yes")

data <- unique(data)


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
  distinct() |>
  filter(!is.na(manufacturer)) |>
  mutate(target_analyte = replace_na(target_analyte, "Unknown")) |>
  mutate(validated_sample_types = stringr::str_replace_all(validated_sample_types, c("Feces" = "Faeces")))|>
  mutate(assay_aim=case_when(
    assay_aim=="ID"~"Tuberculosis",
    assay_aim=="AMR"~"Drug Resistance",
    assay_aim=="ID-AMR"~"Tuberculosis + Drug Resistance",
    assay_aim == "Prediction AST" ~ "Drug Resistance",
    assay_aim == "ID-Prediction AST" ~ "Tuberculosis + Drug Resistance",
    TRUE~assay_aim
  )) |>
  #IF multipick contains##
  mutate(who_technology_class = case_when(
    (target_analyte != "Interferon gamma") &
      (test_format == "Cartridge-based processing" | test_format == "Rapid diagnostic test (strip or cassette)") &
      (assay_target != "Antibody (serological)") & (lab_vs_poc != "Lab-based") ~ "Low complexity",
    type_of_technology=="Molecular" & technology_principle!="Hybridization" & test_format=="NAT reagent kit (proprietary platform)"  ~ "Moderate complexity",
    type_of_technology=="Molecular" & technology_principle=="Hybridization" & test_format=="NAT reagent kit (proprietary platform)" ~ "High complexity",
    TRUE~ "Not applicable"
  )) |>
  mutate(who_pipe=ifelse(grepl("WHO", regulatory_status),"WHO","Pipe")) |>
  mutate(technology_principle=ifelse(who_pipe=="Pipe", "Private", technology_principle),
         target_analyte=ifelse(who_pipe=="Pipe", "Private", target_analyte),
         resistance_marker=ifelse(who_pipe=="Pipe", "Private", resistance_marker)
  )

write_csv(raw, "data/tb/tb_testdir.csv")


