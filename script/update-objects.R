library(salesforcer)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)
library(readr)

#Define Salesforce credentials
sf_auth(username = Sys.getenv("SF_USER"),
        password = Sys.getenv("SF_PASS"),
        security_token = Sys.getenv("SF_TOKEN"))

sf_objects <- sf_list_metadata(list(type='CustomObject'))

sf_names <- c("Assay__c","Instrument__c","Performance_Detail_Submission__c","Software__c","Test_Directory_Package__c","Account","Technology_Submission__c")


sf_list_dfs <- list()

for (sf_name in sf_names){

  sf_metadata  <- tryCatch({
    sf_describe_object_fields(object_name = sf_name) |> select(name,label,everything())
  }, error = function(e) {
    return(NULL)
  })
  if (!is.null(sf_metadata )) {

    sf_metadata <- sf_metadata |>
      mutate(sf_object=sf_name)

    #Get all the names of the data object
    capture.output(name_fields <- paste(sf_metadata$name |> dput(), collapse = ", "), file='NULL')
    #Read all the data from the object (SELECT * not possible)
    names_query <- paste0("SELECT ", name_fields, " FROM ", sf_name)
    object_records <- sf_query(names_query)

    sf_list_dfs[[sf_name]] <- object_records

  }

}

map(sf_names, ~ assign(.x, sf_list_dfs[[.x]], envir = .GlobalEnv))
# for (sf_name in sf_names) {
#   assign(sf_name, sf_list_dfs[[sf_name]])
# }

#Add prefix to objects
names(Account) <- paste0("Account_", names(Account))
#Select necessary manufacturer data
Account <- Account |>
  select(Account_Id,Account_Name,Account_BillingCity,Account_BillingCountry,Account_WHO_Region__c,Account_Website, Account_Type_of_Certified_QMS__c)

#Join account objects
Assay__c <- Assay__c |>
  left_join(Account, by=c("Company_Institution_Name__c"="Account_Id"))
Instrument__c <- Instrument__c |>
  left_join(Account, by=c("Company_Institution_Name__c"="Account_Id"))
Software__c <- Software__c |>
  left_join(Account, by=c("Company_Institution_Name__c"="Account_Id"))


names(Assay__c) <- paste0("Assay_", names(Assay__c))
names(Instrument__c) <- paste0("Instrument_", names(Instrument__c))
names(Software__c) <- paste0("Software_", names(Software__c))
names(Performance_Detail_Submission__c) <- paste0("Performance_", names(Performance_Detail_Submission__c))
names(Test_Directory_Package__c) <- paste0("Package_", names(Test_Directory_Package__c))


df_all_assays <- Test_Directory_Package__c |>
  left_join(Assay__c, by=c("Package_Assay__c"="Assay_Id")) #|>
  # left_join(Instrument__c, by=c("Package_Instrument__c"="Instrument_Id")) |>
  # left_join(Software__c, by=c("Package_Software__c"="Software_Id"))
df_all_assays <- df_all_assays |>
  left_join(Performance_Detail_Submission__c, by=c("Package_Id"="Performance_Package__c")) |>
  mutate(directory="Assays")


df_all_instruments <- Test_Directory_Package__c |>
  left_join(Instrument__c, by=c("Package_Instrument__c"="Instrument_Id")) |>
  mutate(directory="Instruments")

#Temporarly append assay and instruments
df_all <- bind_rows(df_all_assays, df_all_instruments)


#Read metacols data and filter for listed variables
meta_cols_assays <-readr::read_csv("data/testdir_explorer/all_meta_cols_assays.csv", show_col_types = FALSE)
meta_cols_instruments <-  readr::read_csv("data/testdir_explorer/all_meta_cols_instruments.csv", show_col_types = FALSE)
meta_cols <- bind_rows(meta_cols_assays, meta_cols_instruments)
meta_cols <- meta_cols |>
  # readr::read_csv("data/testdir_explorer/all_meta_cols.csv", show_col_types = FALSE) |>
  filter(salesforce_name %in% names(df_all)) |>
  select(id, description, raw_id,salesforce_name, salesforce_name_report) |>
  distinct()

df_all <- df_all |>
  select({ meta_cols$salesforce_name }) |>
  rename_with(~ meta_cols$raw_id, meta_cols$salesforce_name)

#Add COVID in Website area
# df_all <- df_all |>
#   mutate(assay_find_website_area = ifelse(grepl("Covid-19", assay_disease_target), paste0(assay_find_website_area, ";COVID"), assay_find_website_area))
#Remove COVID-19
df_all <- df_all |>
  mutate(assay_find_website_area = str_replace(assay_find_website_area, ";COVID-19", ""))


# meta_cols <- readr::read_csv("data/testdir_explorer/all_meta_cols.csv", show_col_types=FALSE)
# map(sf_names, ~ assign(.x, select(get(.x), any_of(meta_cols$salesforce_name)), envir=.GlobalEnv))
# map(sf_names, ~ {
#   cols_to_rename <- meta_cols$salesforce_name[meta_cols$salesforce_name %in% names(get(.x))]
#   id_to_use <- meta_cols$id[meta_cols$salesforce_name %in% cols_to_rename]
#   assign(.x, rename_with(get(.x), ~ id_to_use, all_of(cols_to_rename)), envir=.GlobalEnv)
# })



extract_link <- function(x) {
  x <- gsub("^.+title=\"", "", x)
  x <- gsub(" \\(New Window\\).+", "", x)
  x
}


data <-
  df_all |>
  mutate(across(everything(), ~as.character(.))) |>
  mutate(across(everything(), ~na_if(., "-"))) |>
  # mutate(permalink = extract_link(permalink)) |>
  # mutate(permalink = if_else(startsWith(permalink, "http"), permalink, paste0("https://", permalink))) |>
  mutate_at(vars(ends_with("permalink")), extract_link) |>
  mutate_at(vars(ends_with("permalink")),  ~if_else(startsWith(., "http"), ., paste0("https://", .))) |>
  relocate(assay_id, .before = assay_name)

#Filter conditions
# data <- data |>
#   # filter(assay_test_to_be_listed_on_finds_web_page=="Yes") |>
#   # filter(instrument_test_to_be_listed_on_finds_web_page=="Yes") |>
#   filter_at(vars(ends_with("test_to_be_listed_on_finds_web_page")), all_vars(. == "Yes" | is.na(.))) |>
#   filter_at(vars(ends_with("stage_of_development")), all_vars(. != "Decommissioned" | is.na(.))) |>
#   filter_at(vars(ends_with("planned_market_entry")), all_vars(. != "Discontinued" | is.na(.))) |>
#   filter_at(vars(ends_with("confidentiality_level")), all_vars(. != "Level 2" & . != "Level 3" | is.na(.))) |>
#   filter_at(vars(ends_with("primary_use_case")), all_vars(. != "EQA" & . != "Quality Control/Validation" |is.na(.))) |>
#   filter_at(vars(ends_with("type_of_technology")), all_vars(. != "Sample Collection" | is.na(.)))

d <-
  data |>
  # mutate(assay_country_tmp = gsub("Korea, Republic of", "South Korea", assay_country)) |>
  mutate(across(ends_with("_country"), ~gsub("Korea, Republic of", "South Korea", .), .names = "{col}_tmp")) |>
  # mutate(assay_city = gsub("Unknown", NA, assay_city)) |>
  mutate(across(ends_with("_city"), ~gsub("Unknown", NA, .))) |>
  # mutate(assay_city2 = if_else(is.na(assay_city), assay_country_tmp, paste0(assay_city, ", ", assay_country_tmp)))
  mutate(across(ends_with("_city"), ~if_else(is.na(.), get(sub("_city", "_country_tmp", cur_column())), paste0(., ", ", get(sub("_city", "_country_tmp", cur_column())))), .names = "{col}2"))


geo_data <-
  d |>
  tidygeocoder::geocode(assay_city2, method = 'bing', lat = assay_lat , long = assay_lng)|>
  tidygeocoder::geocode(instrument_city2, method = 'bing', lat = instrument_lat , long = instrument_lng)#|>
  # tidygeocoder::geocode(software_city2, method = 'bing', lat = software_lat , long = software_lng)

geo_data <-
  geo_data |>
  # mutate(assay_city2 = gsub("South Korea", "Korea, Republic of", assay_city2))
  mutate(across(ends_with("_city2"), ~gsub("Korea, Republic of", "South Korea", .)))

raw <-
  geo_data |>
  # filter(!is.na(manufacturer)) |>
  filter(!if_all(c(assay_manufacturer_id, instrument_manufacturer_id), ~is.na(.))) |>#, software_manufacturer_id
  distinct() |>
  # mutate(target_analyte = replace_na(target_analyte, "Unknown")) |>
  # mutate(validated_sample_types = stringr::str_replace_all(validated_sample_types, c("Feces" = "Faeces")))
  filter(assay_find_website_area!="Pending" | is.na(assay_find_website_area)) |>
  filter(instrument_find_website_area!="Pending" | is.na(instrument_find_website_area) )

raw <-
  raw |>
  mutate(
         assay_disease_target = gsub("Dengue fever", "Dengue", assay_disease_target),
         assay_disease_target = gsub("Ebola fever", "Ebola virus disease", assay_disease_target),
         assay_disease_target = gsub("Marburg", "Marburg virus disease", assay_disease_target),
         assay_disease_target = gsub("Nipah", "Nipah virus disease", assay_disease_target),
         assay_disease_target = gsub("Lassa fever", "Lassa", assay_disease_target)
  )

#Split data by objects
raw_assays <- raw |>
  filter(directory=="Assays")
meta_cols_assays <-  meta_cols_assays |>
  filter(raw_id %in% names(raw_assays))
raw_assays <- raw_assays |>
  select({ meta_cols_assays$raw_id }) |>
  rename_with(~ meta_cols_assays$id, meta_cols_assays$raw_id)
raw_assays <- raw_assays |>
  filter(test_to_be_listed_on_finds_web_page == "Yes") |>
  filter(!(stage_of_development %in% c("Decommissioned"))) |>
  filter(!(planned_market_entry %in% c("Discontinued"))) |>
  # filter(!(confidentiality_level %in% c("Level 2", "Level 3"))) |>
  filter(!(primary_use_case %in% c("EQA", "Quality Control/Validation"))) |>
  filter(!(type_of_technology %in% c("Sample Collection")))|>
  filter(!(lab_vs_poc %in% c("Laboratory-developed test"))) |>
  filter(purpose_of_submission =="Test Directory") |>
  mutate_all(as.character) |>
  mutate(across(-c(assay_id, directory, test_to_be_listed_on_finds_web_page, find_website_area, assay_target, disease_target, lab_vs_poc, type_of_technology, stage_of_development, region, confidentiality_level), ~ ifelse(confidentiality_level  %in% c("Level 2", "Level 3"), "Private Information", .))) |>
  mutate(lat=ifelse(lat=="Private Information", NA, lat),
         lng=ifelse(lng=="Private Information", NA, lng))
#package_id, performance_id, instrument_id, software_id, technology_submission_id, manufacturer_id, data_to_be_published_on_finds_web_page,

# d <-
#   d |>
#   mutate_all(as.character) |>
#   mutate(across(-c(package_id, performance_id, instrument_id, software_id, technology_submission_id, directory, assay_id, assay_manufacturer_id, instrument_manufacturer_id, assay_test_to_be_listed_on_finds_web_page, instrument_test_to_be_listed_on_finds_web_page, performance_data_to_be_published_on_finds_web_page, assay_find_website_area, instrument_find_website_area, assay_lab_vs_poc, assay_type_of_technology, assay_stage_of_development, assay_region, assay_confidentiality_level, instrument_lab_vs_poc, instrument_type_of_technology, instrument_stage_of_development, instrument_region, instrument_confidentiality_level,  assay_stage_of_development, assay_planned_market_entry, assay_primary_use_case, assay_type_of_technology, assay_lab_vs_poc, assay_purpose_of_submission, instrument_stage_of_development, instrument_planned_market_entry, instrument_confidentiality_level, instrument_type_of_technology, instrument_lab_vs_poc , instrument_purpose_of_submission), ~ ifelse(assay_confidentiality_level  %in% c("Level 2", "Level 3") | instrument_confidentiality_level  %in% c("Level 2", "Level 3"), "Private Information", .)))


raw_instruments <- raw |>
  filter(directory=="Instruments")
meta_cols_instruments <- meta_cols_instruments |>
  filter(raw_id %in% names(raw_instruments))
raw_instruments <- raw_instruments |>
  select({ meta_cols_instruments$raw_id }) |>
  rename_with(~ meta_cols_instruments$id, meta_cols_instruments$raw_id)
raw_instruments <- raw_instruments |>
  filter(test_to_be_listed_on_finds_web_page == "Yes") |>
  filter(!(stage_of_development %in% c("Decommissioned"))) |>
  filter(!(planned_market_entry %in% c("Discontinued"))) |>
  # filter(!(confidentiality_level %in% c("Level 2", "Level 3"))) |>
  # filter(!(primary_use_case %in% c("EQA", "Quality Control/Validation"))) |>
  filter(!(type_of_technology %in% c("Sample Collection")))|>
  filter(!(lab_vs_poc %in% c("Laboratory-developed test"))) |>
  filter(purpose_of_submission =="Test Directory") |>
    mutate_all(as.character) |>
    mutate(across(-c(instrument_id, directory, test_to_be_listed_on_finds_web_page, find_website_area, disease_target, lab_vs_poc, type_of_technology, stage_of_development, region, confidentiality_level), ~ ifelse(confidentiality_level  %in% c("Level 2", "Level 3"), "Private Information", .))) |>
  mutate(lat=ifelse(lat=="Private Information", NA, lat),
         lng=ifelse(lng=="Private Information", NA, lng))
#package_id, performance_id, software_id, technology_submission_id,  manufacturer_id, assay_id,

#Filter for MPOC and Lab vs POC
raw_instruments <- raw_instruments |>
  filter((find_website_area != "Molecular POC Instrument") | (find_website_area=="Molecular POC Instrument" & lab_vs_poc=="True Point of Care"))

# write_csv(raw, "data/testdir_explorer/data_all_testdir.csv")
write_csv(raw_assays, "data/testdir_explorer/data_all_testdir_assays.csv")
write_csv(raw_instruments, "data/testdir_explorer/data_all_testdir_instruments.csv")


# raw_unnested_assays <-
#   raw_assays |>
#   separate_rows(assay_regulatory_status, sep = ";") |>
#   separate_rows(assay_target_analyte, sep = ";") |>
#   separate_rows(assay_validated_sample_types, sep = ";") |>
#   separate_rows(assay_serovar_subtype, sep = ";") |>
#   separate_rows(assay_type_of_technology, sep = ";") |>
#   separate_rows(assay_disease_target, sep = ";") |>
#   separate_rows(assay_target_pathogen, sep = ";") |>
#   separate_rows(assay_resistance_marker, sep = ";") |>
#   separate_rows(assay_primary_use_case, sep = ";") |>
#   separate_rows(assay_Drug_resistance_target, sep = ";") |>
#   separate_rows(assay_antimicrobial_resistance_class, sep = ";")  |>
#   separate_rows(assay_syndromes, sep = ";") |>
#   separate_rows(assay_organism_classes, sep = ";") |>
#   separate_rows(assay_scov2_variants, sep = ";") |>
#   separate_rows(assay_find_website_area, sep = ";")

# raw_unnested_instruments <-
#   raw_instruments |>
#   separate_rows(instrument_regulatory_status, sep = ";") |>
#   separate_rows(instrument_disease_target, sep = ";") |>
#   separate_rows(instrument_find_website_area, sep = ";")|>
#   separate_rows(instrument_assay_menu, sep = ", |,|/")

