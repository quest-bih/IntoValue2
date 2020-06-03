library(tidyverse)


CTgov_sample <- read_delim("data/IntoValue2_CTgov_sample_no_abbrev.csv", delim = ";")
DRKS_sample <- read_delim("data/DRKS_sample.csv", delim = ";")

CTgov_sample_clean <- CTgov_sample %>%
  select(nct_id, cities_lead, brief_title, official_title,
         study_first_submitted_date, start_date, start_date_type, 
         completion_date, completion_month_year,
         completion_date_type, results_first_submitted_date,
         overall_status, phase, enrollment, enrollment_type, 
         name, affiliation, intervention_names_comb,
         were_results_reported) %>%
  rename(PI_name = name,
         PI_affiliation = affiliation,
         intervention = intervention_names_comb)

write_delim(CTgov_sample_clean, "data/IntoValue2_CTgov_sample_clean.csv", delim = ";", na = "")






CTgov_results <- CTgov_results %>%
  rename(registration_date = first_received_date) %>%
  rename(main_sponsor = sponsors.agency_class_1) %>%
  rename(recruitmentStatus = overall_status) %>%
  rename(intervention_type = interventions.intervention_type_1) %>%
  rename(results_recieved_date = first_received_results_date) %>%
  mutate(primary_completion_year = substring(primary_completion_date, 7)) %>%
  mutate(is_multicentric = ifelse(number_of_facilities.y > 1, TRUE, FALSE))



#align CTgov and DRKS colnames and contents
DRKS_results <- DRKS_results %>%
  rename(registration_date = firstDrksPublishDate) %>%
  rename(start_date = startDate) %>%
  rename(enrollment = targetSize) %>%
  mutate(recruitmentStatus = trimws(recruitmentStatus)) %>%
  mutate(main_sponsor = ifelse(investorInitiated == "yes", "Other", "Industry")) %>%
  mutate(primary_completion_year = substring(studyEnd, 7)) %>%
  mutate(is_multicentric = ifelse(monoMultiCentric == "Multicenter trial", TRUE, FALSE)) %>%
  mutate(completion_date = studyEnd) %>%
  mutate(primary_completion_date = studyEnd) %>%
  mutate(results_recieved_date = NA) %>%
  mutate(were_results_reported = FALSE) %>%
  mutate(intervention_type = NA)


#join CTgov and DRKS datasets using only the relevant columns
analysis_cols <- c("id", "publication_PMID", "publication_DOI",
                   "publication_URL", "publication_identified", "is_CTgov",
                   "registration_date", "start_date", "primary_completion_date",
                   "completion_date", "results_recieved_date", "publication_date",
                   "were_results_reported", "recruitmentStatus", "cities_primary",
                   "cities_facilities", "corrected_cities", "phase", "enrollment", "main_sponsor",
                   "primary_completion_year", "is_multicentric", "masking", "allocation", "intervention_type")

CTgov_results_join <- CTgov_results %>%
  select(analysis_cols)

DRKS_results_join <- DRKS_results %>%
  select(analysis_cols)

combined_results <- rbind(CTgov_results_join, DRKS_results_join)
