library(salesforcer)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)
library(openxlsx)
library(readr)

#Define Salesforce credentials
sf_auth(username = Sys.getenv("user"),
        password = Sys.getenv("pass"),
        security_token = Sys.getenv("token"))

sf_objects <- sf_list_metadata(list(type='CustomObject'))

#
sf_names <- c("Assay__c","Instrument__c","Performance_Detail_Submission__c","Software__c","Test_Directory_Package__c","Account","Technology_Submission__c")


sf_list_dfs <- list()

for (sf_name in sf_names){

  sf_metadata  <- tryCatch({
    sf_describe_object_fields(object_name = sf_name) %>% select(name,label,everything())
  }, error = function(e) {
    return(NULL)
  })
  if (!is.null(sf_metadata )) {

    sf_metadata <- sf_metadata %>%
      mutate(sf_object=sf_name)

    #Get all the names of the data object
    capture.output(name_fields <- paste(sf_metadata$name %>% dput(), collapse = ", "), file='NULL')
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


meta_cols <- readr::read_csv("data/testdir_explorer/all_meta_cols.csv", show_col_types=FALSE)

map(sf_names, ~ assign(.x, select(get(.x), any_of(meta_cols$salesforce_name)), envir=.GlobalEnv))

Account <- Account %>%
  rename(Account_Name=Name) %>%
  select(-CreatedDate)
Assay__c <- Assay__c %>%
  rename(Assay_Name=Name)
Instrument__c <- Instrument__c %>%
  select(Id, Name) %>%
  rename(Instrument_Name=Name)
Software__c <- Software__c %>%
  select(Id, Name) %>%
  rename(Software_Name=Name)
Test_Directory_Package__c <- Test_Directory_Package__c %>%
  select(-c(CreatedDate, Technology_Submission__c)) %>%
  rename(Package_Name=Name)
Performance_Detail_Submission__c <- Performance_Detail_Submission__c %>%
  select(-c(Assay__c, Instrument__c, Technology_Submission__c, CreatedDate, Assay_Target__c, Disease_Target__c, Drug_Resistance_Target__c, Serovar_subtype__c, Target_Pathogen__c)) %>%
  rename(Performance_Name=Name)



map(sf_names, ~ {
  cols_to_rename <- meta_cols$salesforce_name[meta_cols$salesforce_name %in% names(get(.x))]
  id_to_use <- meta_cols$id[meta_cols$salesforce_name %in% cols_to_rename]
  assign(.x, rename_with(get(.x), ~ id_to_use, all_of(cols_to_rename)), envir=.GlobalEnv)
})

#Add prefix to objects
# names(Account) <- paste0("Account_", names(Account))
# names(Assay__c) <- paste0("Assay_", names(Assay__c))
# names(Instrument__c) <- paste0("Instrument_", names(Instrument__c))
# names(Software__c) <- paste0("Software_", names(Software__c))
# names(Performance_Detail_Submission__c) <- paste0("Performance_", names(Performance_Detail_Submission__c))
# names(Test_Directory_Package__c) <- paste0("Package_", names(Test_Directory_Package__c))

#Join account objects
Assay__c <- Assay__c %>%
  left_join(Account, by=c("manufacturer_id"="id"))


  #   left_join(Account, by=c("Instrument_manufacturer"="Account_id"))
# Instrument__c_company <- Instrument__c %>%
#   left_join(Account, by=c("Instrument_manufacturer"="Account_id"))
# Software__c_company <- Software__c %>%
#   left_join(Account, by=c("Software_manufacturer"="Account_id"))

#Add additional prefix to objects coming from account
# names(Assay__c_company) <- ifelse(str_starts(names(Assay__c_company), "Account_"), paste0("Assay_", names(Assay__c_company)), names(Assay__c_company))
# names(Instrument__c_company) <- ifelse(str_starts(names(Instrument__c_company), "Account_"), paste0("Instrument_", names(Instrument__c_company)), names(Instrument__c_company))
# names(Software__c_company) <- ifelse(str_starts(names(Software__c_company), "Account_"), paste0("Software_", names(Software__c_company)), names(Software__c_company))


df_all <- Test_Directory_Package__c %>%
  #left_join(Assay__c_company, by=c("Package_assay_id"="Assay_id")) %>%
  left_join(Assay__c, by=c("assay_id"="id")) %>%
  left_join(Instrument__c, by=c("instrument_id"="id")) %>%
  left_join(Software__c, by=c("software_id"="id"))

df_all <- df_all %>%
  left_join(Performance_Detail_Submission__c %>% rename(performance_id=id), by=c("id"="package_id")) %>%
  rename(package_id=id)


extract_link <- function(x) {
  x <- gsub("^.+title=\"", "", x)
  x <- gsub(" \\(New Window\\).+", "", x)
  x
}


data <-
  df_all %>%
  mutate(across(everything(), ~as.character(.))) %>%
  mutate(across(everything(), ~na_if(., "-"))) %>%
  mutate(permalink = extract_link(permalink)) %>%
  mutate(permalink = if_else(startsWith(permalink, "http"), permalink, paste0("https://", permalink))) %>%
  # mutate_at(vars(ends_with("permalink")), extract_link) %>%
  # mutate_at(vars(ends_with("permalink")),  ~if_else(startsWith(., "http"), ., paste0("https://", .))) %>%
  relocate(assay_id, .before = assay_name)


data <- data %>%
  filter(test_to_be_listed_on_finds_web_page=="Yes")
  # filter_at(vars(ends_with("test_to_be_listed_on_finds_web_page")), all_vars(. == "Yes" | is.na(.)))

d <-
  data %>%
  mutate(country_tmp = gsub("Korea, Republic of", "South Korea", country)) %>%
  mutate(city = gsub("Unknown", NA, city)) %>%
  #mutate(city2 = coalesce(city, country)) %>%
  mutate(city2 = if_else(is.na(city), country_tmp, paste0(city, ", ", country_tmp)))

geo_data <-
  d %>%
  tidygeocoder::geocode(city2, method = 'bing', lat = lat , long = lng)

geo_data <-
  geo_data %>%
  mutate(city2 = gsub("South Korea", "Korea, Republic of", city2))

raw <-
  geo_data %>%
  distinct() %>%
  filter(!is.na(manufacturer)) #%>%
  # mutate(target_analyte = replace_na(target_analyte, "Unknown")) %>%
  # mutate(validated_sample_types = stringr::str_replace_all(validated_sample_types, c("Feces" = "Faeces")))

write_csv(raw, "data/testdir_explorer/data_all_testdir.csv")

