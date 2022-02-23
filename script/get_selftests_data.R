library(readr)
library(tidyr)
library(dplyr)
library(salesforcer)
library(fuzzyjoin)
library(stringr)

# # Authenticate using username, password, and security token ...
# sf_auth(username = Sys.getenv("FIND_SALESFORCE_USERNAME"),
#         password = Sys.getenv("FIND_SALESFORCE_PASSWORD"),
#         security_token = Sys.getenv("FIND_SALESFORCE_SECURITY_TOKEN"))

# # # find a report in your org and run it
# all_reports <- sf_query("SELECT Id, Name FROM Report")
# # this is the Self Test report
# covid_report_id <- '00O6900000CRYhTEAX'
# sf_data <- sf_run_report(covid_report_id)

# write_csv(sf_data, "data/sf_data.csv")

sf_data <- read_csv("data/sf_data.csv")



# colnames(sf_data) <- str_replace(string = colnames(sf_data), replacement = '', 'Parent Submission: ')

# cols_toKeep <- c("Performance Detail Submission: ID", "Company/Institution Name",
#                 "Submission Title", "Country of Manufacturer",
#                 "Regulatory Approval", "Clinical Sensitivity (%)",
#                 "95% CI for Clinical Sensitivity (Lower)", "95% CI for Clinical Sensitivity (Upper)",
#                 "Clinical Specificity (%)","95% CI for Clinical Specificity (Lower)",
#                 "95% CI for Clinical Specificity (Upper)", "Assay Target",
#                 "Target Analyte(s)", "Self-testing/Self-Collection",
#                 "Validated Sample Types", "Time to results excl sample prep (mins)")

# sf_data <- sf_data[,colnames(sf_data) %in% cols_toKeep]
# sf_data$submission_id <- sf_data$`Performance Detail Submission: ID`

# # Split regulatory status -------------------------------------------------

# reg_status <- as.data.frame(str_split_fixed(sf_data$`Regulatory Approval`, pattern = "; ", n = 9))
# reg_status <- cbind(submission_id = sf_data$submission_id, reg_status)

# tmp <- reshape2::melt(reg_status, id = 'submission_id')

# tmp <- dplyr::arrange(tmp, value)
# tmp_cast <- reshape2::dcast(tmp, submission_id ~ value, fun = function(x) paste(x, collapse = ", "),
#                             value.var = c("value"))
# cols_toKeep <- c("submission_id", "Australia TGA", "Brazil ANVISA", "CE-IVD", "China NMPA", "Germany BfArM",
#                  "Health Canada", "India CDSCO", "Japan MHLW", "Korea MFDS", "MHRA UK", "Philippines FDA",
#                  "Singapore HSA", "South Africa SAHPRA", "Taiwan FDA", "Ukraine SMDC", "US FDA EUA", "WHO EUL",
#                  "WHO EUL Under Assessment", "Other", "Unknown")
# tmp_cast <- tmp_cast[,colnames(tmp_cast) %in% cols_toKeep]
# tmp_cast[tmp_cast == ""] <- NA

# reg_status <- tmp_cast
# reg_status <- dplyr::left_join(sf_data[,c("Company/Institution Name", "Submission Title", "Country of Manufacturer", "Assay Target",
#                                           "Target Analyte(s)", "Self-testing/Self-Collection", "Validated Sample Types", "Time to results excl sample prep (mins)",
#                                           "submission_id" )],
#                                reg_status, by = 'submission_id')

# # Create the table_data df used from the app ------------------------------

# table_data <- reg_status %>%
#   select(
#     company = `Company/Institution Name`,
#     test_name = `Submission Title`,
#     country = `Country of Manufacturer`,
#     sample = `Validated Sample Types`,
#     prof_self = `Self-testing/Self-Collection`,
#     target = `Assay Target`,
#     time = `Time to results excl sample prep (mins)`,
#     australia = `Australia TGA`,
#     canada = `Health Canada`,
#     fda = `US FDA EUA`,
#     eu = `CE-IVD`,
#     japan = `Japan MHLW`,
#     who = `WHO EUL`
#   ) %>%
#   dplyr::mutate(australia = recode(australia,  `Australia TGA` = '1'),
#                 canada = recode(canada, `Health Canada` = '1'),
#                 fda = recode(fda,  `US FDA EUA` = '1'),
#                 eu = recode(eu, `CE-IVD` = '1'),
#                 japan = recode(japan, `Japan MHLW` = '1'),
#                 who = recode(who, `WHO EUL` = '1')) %>%
#   dplyr::mutate_at(vars(australia, canada, fda, eu, japan, who), ~replace_na(., '0'))%>%
#   dplyr::mutate(across(c(australia, canada, fda, eu, japan, who), as.numeric)) %>%
#   dplyr::mutate(sum = rowSums(across(c(australia, canada, fda, japan, who)))) %>%
#   dplyr::arrange(-sum) %>%
#   dplyr::filter(sum > 0)


# # fda <- subset(subset(table_data, fda == 1))
# # australia <- subset(table_data, australia == 1)
# # canada <- subset(table_data, canada == 1)
# # japan <- subset(table_data, japan == 1)
# # who <- subset(table_data, who == 1)
# #
# # table(fda$prof_self)
# # table(australia$prof_self)
# # table(canada$prof_self)
# # table(japan$prof_self)
# # table(who$prof_self)



# table_data_summ <- table_data %>%
#   pivot_longer(cols = c(australia, canada, fda, japan, who)) %>%
#   filter(value == 1) %>%
#   group_by(prof_self,name) %>%
#   count(name, value)





# # Heatmap table for self tests --------------------------------------------
# heatmap_table_self <- table_data %>%
#   select(company,
#          test_name,
#          prof_self,
#          australia,
#          canada,
#          fda,
#          japan,
#          who) %>%
#   dplyr::filter(prof_self == 'Intended for self-testing (version available)') %>%
#   count(australia, canada, fda) %>%
#   dplyr::mutate(sum = rowSums(across(c(australia, canada, fda)))) %>%
#   mutate(id = paste0('id', seq(1:nrow(.)), "_", n)) %>%
#   pivot_longer(cols = c(australia, canada, fda))

# heatmap_table_self$n <- factor(heatmap_table_self$n, levels = sort(unique(heatmap_table_self$n), decreasing = T))
# heatmap_table_self$sum <- factor(heatmap_table_self$sum, levels = sort(unique(heatmap_table_self$n), decreasing = T))
# heatmap_table_self <- arrange(heatmap_table_self, sum) %>%
#   dplyr::mutate(value = recode(value,  `1` = 'Authorized', `0` = 'Not authorized'),
#                 name = recode(name, `australia` = 'Australia TGA', `canada` = 'Canada Health', `fda` = 'US FDA EUA'))


# # Heatmap table for professional antigen tests --------------------------------------------
# heatmap_table_prof <- table_data %>%
#   select(company,
#          test_name,
#          prof_self,
#          australia,
#          canada,
#          fda,
#          japan,
#          who) %>%
#   dplyr::filter(prof_self == 'Intended for professional use only') %>%
#   count(australia, canada, fda, japan, who) %>%
#   dplyr::mutate(sum = rowSums(across(c(australia, canada, fda, japan, who)))) %>%
#   mutate(id = paste0('id', seq(1:nrow(.)), "_", n)) %>%
#   pivot_longer(cols = c(australia, canada, fda, japan, who))

# heatmap_table_prof$n <- factor(heatmap_table_prof$n, levels = sort(unique(heatmap_table_prof$n), decreasing = T))
# heatmap_table_prof$sum <- factor(heatmap_table_prof$sum, levels = sort(unique(heatmap_table_prof$n), decreasing = T))
# heatmap_table_prof <- arrange(heatmap_table_prof, sum) %>%
#   dplyr::mutate(value = recode(value,  `1` = 'Authorized', `0` = 'Not authorized'),
#                 name = recode(name, `australia` = 'Australia TGA', `canada` = 'Canada Health',
#                               `fda` = 'US FDA EUA', `japan` = 'Japan MHLW', `who` = 'WHO EUL'))

#
#
# readr::write_csv(heatmap_table_self, paste0("self_tests/heatmap_table_self_", Sys.Date(), '.csv'))
# readr::write_csv(heatmap_table_prof, paste0("self_tests/heatmap_table_prof_", Sys.Date(), '.csv'))
#
#
#
#
#
# # Make long table for hierarchical graph ----------------------------------
#
# table_data_long <- table_data %>%
#   filter(sum >0) %>%
#   group_by(company, test_name, country, sample, prof_self, target, time, sum) %>%
#   pivot_longer(cols = c(australia, canada, fda, japan, who)) %>%
#   dplyr::mutate(Decision = recode(value,  `1` = 'Authorized', `0` = 'Not authorized'),
#                 `Regulatory authority` = recode(name, `australia` = 'Australia TGA', `canada` = 'Canada Health',
#                               `fda` = 'US FDA EUA', `japan` = 'Japan MHLW', `who` = 'WHO EUL'),
#                 `Number of authorizations` = paste0('Tests with ', sum, ' authorization(s)'))
#
#
#
# readr::write_csv(table_data_long, paste0("self_tests/self_prof_tests_report_long_", Sys.Date(), '.csv'))
#
#
#
#
# readr::write_csv(table_data, paste0("self_tests/self_prof_tests_report_", Sys.Date(), '.csv'))
# readr::write_csv(table_data_summ, paste0("self_tests/self_tests_report_summary_", Sys.Date(), '.csv'))
#
#


#table_data <- readr::read_csv(paste0("self_tests/self_prof_tests_report_", Sys.Date(), '.csv')) %>%
table_data <- readr::read_csv(paste0("self_tests/self_prof_tests_report_", "2022-02-18", '.csv')) %>%
  filter(prof_self == "Intended for self-testing (version available)") %>%
  dplyr::mutate(australia = recode(australia,  `1` = 'Authorized', `0` = 'Not listed'),
                canada = recode(canada, `1` = 'Authorized', `0` = 'Not listed'),
                fda = recode(fda,  `1` = 'Authorized', `0` = 'Not listed'),
                eu = recode(eu, `1` = 'Authorized', `0` = 'Not listed'),
                japan = recode(japan, `1` = 'Authorized', `0` = 'Not listed'),
                who = recode(who, `1` = 'Authorized', `0` = 'Not listed'))
