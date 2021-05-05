library(tidyverse)
library(lubridate)

#----------------------------------------------------------------------------------------------------------------------
# Loading of IV1 AACT dataset
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
AACT_folder <- "C:/Datenablage/AACT/AACT dataset 20170416/" #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values")

AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".txt")
AACT_datasets_IV1 <- AACT_dataset_files %>%
  map(read_delim, delim = "|", guess_max = 20000)
names(AACT_datasets_IV1) <- AACT_dataset_names


#----------------------------------------------------------------------------------------------------------------------
# Loading of IV2 AACT dataset
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
AACT_folder <- "C:/Datenablage/AACT/AACT dataset 20200603/" #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values")

AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".txt")
AACT_datasets_IV2 <- AACT_dataset_files %>%
  map(read_delim, delim = "|", guess_max = 20000)
names(AACT_datasets_IV2) <- AACT_dataset_names


#----------------------------------------------------------------------------------------------------------------------
# Loading of IV1 & 2 dataset
#----------------------------------------------------------------------------------------------------------------------

#read in dataset
IntoValue2_dataset <- read_csv("data/IntoValue2_Dataset.csv", na = "NA")
IntoValue1_dataset <- read_csv("data/IntoValue1_Dataset.csv", na = "NA")


#----------------------------------------------------------------------------------------------------------------------
# add the missing registration and summary results date to the IV1 dataset
#----------------------------------------------------------------------------------------------------------------------

IV1_dates_join <- AACT_datasets_IV1$studies %>% 
  select(nct_id, first_received_results_date, first_received_date) %>% 
  rename(id = nct_id)

IntoValue1_dataset <- IntoValue1_dataset %>% 
  left_join(IV1_dates_join) %>%
  rename(summary_res_date = first_received_results_date,
         study_registration_date = first_received_date) %>%
  mutate(study_registration_date = study_registration_date %>% ymd())


#----------------------------------------------------------------------------------------------------------------------
# add the missing primary completion date to the IV2 dataset
#----------------------------------------------------------------------------------------------------------------------

PCD_join <- AACT_datasets_IV2$studies %>% 
  select(nct_id, primary_completion_date, study_first_submitted_date) %>% 
  rename(id = nct_id,
         study_registration_date = study_first_submitted_date)

IntoValue2_dataset <- IntoValue2_dataset %>% 
  left_join(PCD_join) %>%
  mutate(primary_completion_year = primary_completion_date %>% str_sub(1,4),
         primary_completion_date = primary_completion_date %>% ymd(),
         study_registration_date = study_registration_date %>% ymd(),
         days_PCD_to_summary = summary_res_date - primary_completion_date,
         days_PCD_to_publication = publication_date - primary_completion_date,
         days_reg_to_PCD = primary_completion_date - study_registration_date)


#----------------------------------------------------------------------------------------------------------------------
# rename and modify IV1 variables to match the IV2 dataset format
#----------------------------------------------------------------------------------------------------------------------

IntoValue1_dataset$is_multicentric[IntoValue1_dataset$is_multicentric == "Not given"] <- NA

IntoValue1_dataset <- IntoValue1_dataset %>%
  rename(identification_step = publication_type,
         has_publication = publications,
         has_summary_results = summary_results,
         recruitment_status = recruitmentStatus,
         days_PCD_to_publication = days_to_publication_PCD,
         days_CD_to_publication = days_to_publication_CD,
         days_PCD_to_summary = days_to_summary_PCD,
         days_CD_to_summary = days_to_summary_CD) %>%
  #to change seperator
  mutate(lead_cities = lead_cities %>% str_replace_all("TU München", "TU_München"),
         lead_cities = lead_cities %>% str_replace_all("LMU München", "LMU_München"),
         lead_cities = lead_cities %>% str_replace(fixed("Kiel"), "Schleswig-Holstein"),
         lead_cities = lead_cities %>% str_replace(fixed("Lübeck"), "Schleswig-Holstein"),
         is_CTgov = ifelse(is_CTgov == "yes", TRUE, FALSE),
         is_multicentric = ifelse(is_multicentric == "yes", TRUE, FALSE),
         allocation  = allocation  %>% replace_na("Not given"),
         has_german_umc_lead = if_else(lead_or_facility == "lead", TRUE, FALSE)
         ) %>%
  select(-lead_or_facility)


#----------------------------------------------------------------------------------------------------------------------
# rename and modify IV2 variables
#----------------------------------------------------------------------------------------------------------------------

IntoValue2_dataset <- IntoValue2_dataset %>% 
  rename(days_CD_to_publication = days_to_publication,
         days_CD_to_summary = days_to_summary,
         days_reg_to_CD = days_reg_to_compl) %>%
  mutate(has_german_umc_lead = TRUE,
         publication_PMID = NA,
         facility_cities = NA)



#check for missing rows
IV1_notin_IV2 <- colnames(IntoValue1_dataset)[!(colnames(IntoValue1_dataset) %in% colnames(IntoValue2_dataset))]
IV2_notin_IV1 <- colnames(IntoValue2_dataset)[!(colnames(IntoValue2_dataset) %in% colnames(IntoValue1_dataset))]
IV1_notin_IV2
IV2_notin_IV1


#----------------------------------------------------------------------------------------------------------------------
# bring IV2 dataset in the same shape as the IV1 dataset used for the shiny app
# with one row for each combination of trial & UMC for easy filtering
#----------------------------------------------------------------------------------------------------------------------


#combine datasets
IntoValue1_dataset <- IntoValue1_dataset %>% 
  mutate(IV_version = "IV1")
IntoValue2_dataset <- IntoValue2_dataset %>% 
  mutate(IV_version = "IV2")

#clearly label the duplicate trials but do not remove them
dupl_trials_IV1 <- IntoValue1_dataset[(IntoValue1_dataset$id %in% IntoValue2_dataset$id),]$id
dupl_trials_IV2 <- IntoValue2_dataset[(IntoValue2_dataset$id %in% IntoValue1_dataset$id),]$id

IntoValue1_dataset[IntoValue1_dataset$id %in% dupl_trials_IV1,]$IV_version <- "IV1_dupl"
IntoValue2_dataset[IntoValue2_dataset$id %in% dupl_trials_IV2,]$IV_version <- "IV2_dupl"


IntoValue_datasets_comb <- rbind(IntoValue2_dataset, IntoValue1_dataset)  %>%
  mutate(completion_year = str_sub(completion_date, 1, 4))


#manually re-sort columns by topic
IntoValue_datasets_comb <- IntoValue_datasets_comb %>%
  select(id, lead_cities, has_publication, publication_DOI, publication_PMID,
         publication_URL, publication_date, identification_step,
         has_summary_results, summary_res_date,
         study_registration_date, start_date, completion_date, 
         completion_year, primary_completion_date, primary_completion_year, 
         days_CD_to_publication, days_PCD_to_publication,
         days_CD_to_summary, days_PCD_to_summary,
         days_reg_to_start, days_reg_to_CD, days_reg_to_PCD, days_reg_to_publ,
         recruitment_status, phase, enrollment, is_multicentric,
         main_sponsor, allocation, masking, intervention_type,
         center_size, is_CTgov,
         has_german_umc_lead, facility_cities, IV_version)

write_csv(IntoValue_datasets_comb, "data/IV1_IV2_combined_dataset.csv")
