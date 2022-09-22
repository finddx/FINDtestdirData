
# fs::file_copy("../shinytestdir/testdir/data/meta_cols.csv", "data/meta_cols.csv")


library(readr)
library(tidyr)
library(dplyr)
library(salesforcer)
library(fuzzyjoin)
library(stringr)
library(reshape2)


sf_auth(
  username = Sys.getenv("FIND_SALESFORCE_USERNAME"),
  password = Sys.getenv("FIND_SALESFORCE_PASSWORD"),
  security_token = Sys.getenv("FIND_SALESFORCE_SECURITY_TOKEN")
)

covid_report_id <- '00O6900000CRYhTEAX'
sf_data_raw <- sf_run_report(covid_report_id)



sf_data <- sf_data_raw



colnames(sf_data) <- str_replace(string = colnames(sf_data), replacement = '', 'Parent Submission: ')

cols_toKeep <- c("Performance Detail Submission: ID", "Company/Institution Name",
                "Submission Title", "Country of Manufacturer",
                "Regulatory Approval", "Clinical Sensitivity (%)",
                "95% CI for Clinical Sensitivity (Lower)", "95% CI for Clinical Sensitivity (Upper)",
                "Clinical Specificity (%)","95% CI for Clinical Specificity (Lower)",
                "95% CI for Clinical Specificity (Upper)", "Assay Target",
                "Target Analyte(s)", "Self-testing/Self-Collection",
                "Validated Sample Types", "Time to results excl sample prep (mins)")

sf_data <- sf_data[,colnames(sf_data) %in% cols_toKeep]
sf_data$submission_id <- sf_data$`Performance Detail Submission: ID`

# Split regulatory status -------------------------------------------------

reg_status <- as.data.frame(str_split_fixed(sf_data$`Regulatory Approval`, pattern = "; ", n = 9))
reg_status <- cbind(submission_id = sf_data$submission_id, reg_status)

tmp <- reshape2::melt(reg_status, id = 'submission_id')

tmp <- dplyr::arrange(tmp, value)
tmp_cast <- reshape2::dcast(tmp, submission_id ~ value, fun = function(x) paste(x, collapse = ", "),
                            value.var = c("value"))
cols_toKeep <- c("submission_id", "Australia TGA", "Brazil ANVISA", "CE-IVD", "China NMPA", "Germany BfArM",
                 "Health Canada", "India CDSCO", "Japan MHLW", "Korea MFDS", "MHRA UK", "Philippines FDA",
                 "Singapore HSA", "South Africa SAHPRA", "Taiwan FDA", "Ukraine SMDC", "US FDA EUA", "WHO EUL",
                 "WHO EUL Under Assessment", "Other", "Unknown")
tmp_cast <- tmp_cast[,colnames(tmp_cast) %in% cols_toKeep]
tmp_cast[tmp_cast == ""] <- NA

reg_status <- tmp_cast
reg_status <- dplyr::left_join(sf_data[,c("Company/Institution Name", "Submission Title", "Country of Manufacturer", "Assay Target",
                                          "Target Analyte(s)", "Self-testing/Self-Collection", "Validated Sample Types", "Time to results excl sample prep (mins)",
                                          "submission_id" )],
                               reg_status, by = 'submission_id')

# Create the table_data df used from the app ------------------------------



authorities <- c("Australia TGA", "Brazil ANVISA", "CE-IVD",
                 "China NMPA", "Germany BfArM", "Health Canada", "India CDSCO",
                 "Japan MHLW", "Korea MFDS", "MHRA UK", "Other", "Philippines FDA",
                 "Singapore HSA", "South Africa SAHPRA", "Taiwan FDA", "Ukraine SMDC",
                 "Unknown", "US FDA EUA", "WHO EUL", "WHO EUL Under Assessment")


data <- reg_status %>%
  dplyr::mutate(across(all_of(authorities),  ~ replace(., !is.na(.), '1'))) %>%
  dplyr::mutate(across(all_of(authorities),  ~replace_na(., '0'))) %>%
  dplyr::mutate(across(all_of(authorities), as.numeric)) %>%
  # dplyr::mutate(australia = recode(australia,  `Australia TGA` = '1'),
  #               canada = recode(canada, `Health Canada` = '1'),
  #               fda = recode(fda,  `US FDA EUA` = '1'),
  #               eu = recode(eu, `CE-IVD` = '1'),
  #               japan = recode(japan, `Japan MHLW` = '1'),
  #               who = recode(who, `WHO EUL` = '1')) %>%
  # dplyr::mutate_at(vars(australia, canada, fda, eu, japan, who), ~replace_na(., '0'))%>%
  # dplyr::mutate(across(c(australia, canada, fda, eu, japan, who), as.numeric)) %>%
  dplyr::rename(
    company = `Company/Institution Name`,
    test_name = `Submission Title`,
    country = `Country of Manufacturer`,
    sample = `Validated Sample Types`,
    prof_self = `Self-testing/Self-Collection`,
    target = `Assay Target`,
    time = `Time to results excl sample prep (mins)`,
    australia = `Australia TGA`,
    canada = `Health Canada`,
    fda = `US FDA EUA`,
    eu = `CE-IVD`,
    japan = `Japan MHLW`,
    who = `WHO EUL`,
    brazil = `Brazil ANVISA`,
    china = `China NMPA`,
    germany = `Germany BfArM`,
    india = `India CDSCO`,
    korea = `Korea MFDS`,
    uk = `MHRA UK`,
    philippines = `Philippines FDA`,
    singapore = `Singapore HSA`,
    south_africa = `South Africa SAHPRA`,
    taiwan = `Taiwan FDA`,
    ukraine = `Ukraine SMDC`,
    who_eul = `WHO EUL Under Assessment`,
    other = `Other`,
    unknown = `Unknown`
  ) %>%
  dplyr::mutate(sum = rowSums(across(c(australia, canada, fda, japan, who)))) %>%
  dplyr::arrange(-sum) %>%
  dplyr::filter(sum > 0)





select(
  company = `Company/Institution Name`,
  test_name = `Submission Title`,
  country = `Country of Manufacturer`,
  sample = `Validated Sample Types`,
  prof_self = `Self-testing/Self-Collection`,
  target = `Assay Target`,
  time = `Time to results excl sample prep (mins)`,
  australia = `Australia TGA`,
  canada = `Health Canada`,
  fda = `US FDA EUA`,
  eu = `CE-IVD`,
  japan = `Japan MHLW`,
  who = `WHO EUL`
)


write_csv(data, "data/selftests.csv")



