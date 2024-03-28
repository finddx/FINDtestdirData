library(readr)
library(tidyr)
library(dplyr)
#library(salesforcer)
library(fuzzyjoin)
library(stringr)

set.seed(5)

sf_data <- readr::read_csv("https://raw.githubusercontent.com/finddx/FINDtestdirData/report/amr_testdir.csv")

colnames(sf_data) <- str_replace(string = colnames(sf_data), replacement = '', 'Parent Submission: ')


meta_cols <-
  readr::read_csv("data/amr/amr_testdir_meta_cols.csv", show_col_types = FALSE) |>
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
  # mutate(region=continent)|>
  # select(-name, -alpha3, -continent) |>#!rcontinent removed, not renamed as region already existed
  mutate(permalink = extract_link(permalink)) |>
  mutate(permalink = if_else(startsWith(permalink, "http"), permalink, paste0("https://", permalink))) |>
  relocate(submission_id, .before = submission_title)


data <- data |>
  filter(test_to_be_listed_on_finds_web_page=="Yes")

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

#remove_pathogens <- "Adenovirus; |Ancylostoma spp.; |Ascaris lumbricoides; |Brugia malayi; |Chikungunya virus; |Cowpox virus; |Crimean Congo Hemorrhagic Fever virus; |Dengue virus; |Ebola virus; |Enterovirus; Filovirus; |Human Coronavirus; |Human Metapneumovirus; |Human Rhinovirus; |Influenza A; |Influenza B; |Lassa virus; |Leishmania parasites; |Marburg virus; |Measles morbillivirus; |Middle East Respiratory Coronavirus; |Mpox virus; |Mycobacterium Tuberculosis; |Mycobacterium Tuberculosis Complex Species; |Mycobacterium ulcerans; |Necator americanus.; |Nipah virus; |Onchocera volvulus; |Orthopoxvirus; |Parainfluenza virus; |Plasmodium Falciparum; |Plasmodium Knowlesi; |Plasmodium Malariae; |Plasmodium Ovale; |Plasmodium Species; |Plasmodium Vivax; |Pox viruses (unspecified); |Respiratory Syncytial Virus \\(RSV\\)|Rift Valley Fever virus; |Rubella virus; |SARS-CoV-2; |Schistosoma mansoni; |Taenia solium; |Trichuris trichiura; |Trypanosoma brucei; |Trypanosoma cruzi; |Typhoid; Vaccinia virus; |Variola virus; |West Nile virus; |Wuchereria bancrofti; |Yellow Fever virus; |Zika virus; |Leishmania\U00A0parasites; "

raw <-
  geo_data |>
  distinct() |>
  filter(!is.na(manufacturer)) |>
  mutate(target_analyte = replace_na(target_analyte, "Unknown")) |>
  mutate(validated_sample_types = stringr::str_replace_all(validated_sample_types, c("Feces" = "Faeces"))) |>
  mutate(assay_aim=case_when(
    assay_aim=="AMR"~"Resistance",
    assay_aim=="ID-AMR"~"ID-Resistance",
    TRUE~assay_aim
  )) |>
  mutate(syndromes=gsub("Enteric","Gastro-intestinal Infections",syndromes)) |>
  mutate(type_of_technology=gsub("Physical","Physics based",type_of_technology)) #|>
  #mutate(target_pathogen = gsub(remove_pathogens, "", target_pathogen))

write_csv(raw, "data/amr/amr_testdir.csv")


