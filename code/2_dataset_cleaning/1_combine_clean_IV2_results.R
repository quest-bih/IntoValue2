library(tidyverse)
library(lubridate)
library(assertthat)
library(assertr)

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset for additional variables
#----------------------------------------------------------------------------------------------------------------------

# Get registry data if not already downloaded/unzipped
source(here::here("code", "0_get_registry_data.R"))

AACT_folder <- here::here("data", "raw-registries", "2020-06-03_ctgov")

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values", "designs")

AACT_dataset_files <- paste0(AACT_folder, "/", AACT_dataset_names, ".txt")
AACT_datasets <- AACT_dataset_files %>%
  map(read_delim, delim = "|", guess_max = 50000)
names(AACT_datasets) <- AACT_dataset_names


#define subsets that needed to be joined into CTgov table
AACT_interventions <- AACT_datasets$interventions %>% select(nct_id, intervention_type) %>% 
  distinct(nct_id, .keep_all = TRUE)
AACT_calc_values <- AACT_datasets$calculated_values %>% select(nct_id, has_single_facility)
AACT_sponsors <- AACT_datasets$sponsors %>% filter(lead_or_collaborator == "lead") %>%
  select(nct_id, agency_class)
AACT_designs <- AACT_datasets$designs %>% select(nct_id, allocation, masking)
AACT_studies <- AACT_datasets$studies %>% select(nct_id, results_first_submitted_date)


#----------------------------------------------------------------------------------------------------------------------
# Loading of full DRKS dataset for additional variables 
#----------------------------------------------------------------------------------------------------------------------

DRKS_add_columns <- read_csv2(here::here("data", "raw-registries", "2020-06-03_drks.csv")) %>%
  select(drksId, allocation, masking, phase)


#----------------------------------------------------------------------------------------------------------------------
# Loading and preprocessing of results datasets
#----------------------------------------------------------------------------------------------------------------------

#add missing AACT variables, change variable names and standardize values
standardize_CTgov <- function(df) {
  df_std <- df %>%
    add_column("is_CTgov" = TRUE)  %>%
    left_join(AACT_interventions, by = "nct_id") %>%
    left_join(AACT_calc_values, by = "nct_id") %>%
    left_join(AACT_sponsors, by = "nct_id") %>%
    left_join(AACT_designs, by = "nct_id") %>%
    left_join(AACT_studies, by = "nct_id") %>%
    rename("id" = "nct_id",
           "indentification_step" = "Publication identified in which step",
           main_sponsor = agency_class,
           summary_res_date = results_first_submitted_date) %>%
    mutate(is_multicentric = !has_single_facility)
  return(df_std)
}

standardize_DRKS <- function(df) {
  df_std <- df %>%
    left_join(DRKS_add_columns, by = "drksId") %>%
    rename(id = "drksId",
           indentification_step = "Publication identified in which step",
           start_date = "startDate",
           study_first_submitted_date = "firstDrksPublishDate",
           completion_date = "studyEnd",
           overall_status = "recruitmentStatus",
           enrollment = "targetSize") %>%
    add_column("is_CTgov" = FALSE,
               "were_results_reported" = FALSE,
               intervention_type = NA,
               summary_res_date = NA) %>%
    mutate(is_multicentric = (monoMultiCentric == "Multicenter trial"),
           main_sponsor = ifelse(investorInitiated == "yes", "Other", "Industry"))
  return(df_std)
}


#loading of manual results and standardize column names for joining
CTgov_results <- read_csv("data/2_dataset_cleaning/manual_check_results/IntoValue2_CTgov_results_main_check_including_DC.csv") %>%
  standardize_CTgov()
DRKS_results <- read_csv("data/2_dataset_cleaning/manual_check_results/IntoValue2_DRKS_results_main_check_including_DC.csv") %>%
  standardize_DRKS()

CTgov_DC_changes <- read_csv("data/2_dataset_cleaning/manual_check_results/IntoValue2_extended_DC_CTgov_changes.csv") %>%
  standardize_CTgov()
DRKS_DC_changes <- read_csv("data/2_dataset_cleaning/manual_check_results/IntoValue2_extended_DC_DRKS_changes.csv") %>%
  standardize_DRKS()


#restrict dataset to standardized set of columns
analysis_cols <- c("id", "cities_lead_corrected", 
                   "indentification_step", "publication_DOI",
                   "publication_URL", "publication_date",
                   "study_first_submitted_date", "start_date", 
                   "completion_date", "summary_res_date",
                   "were_results_reported", "overall_status", 
                   "phase", "enrollment", "is_CTgov",
                   "is_multicentric", "main_sponsor",
                   "allocation", "masking",
                   "intervention_type")

CTgov_results_join <- CTgov_results %>% select(all_of(analysis_cols))
DRKS_results_join <- DRKS_results %>% select(all_of(analysis_cols))
CTgov_DC_changes_join <- CTgov_DC_changes %>% select(all_of(analysis_cols))
DRKS_DC_changes_join <- DRKS_DC_changes %>% select(all_of(analysis_cols))


#implement double check changes to dataset - keep only the results from the DC changes
#the distinct function only keeps the first occurence of the duplicate IDs
CTgov_results_combined <- rbind(CTgov_DC_changes_join, CTgov_results_join) %>% 
  distinct(id, .keep_all = TRUE) 
DRKS_results_combined <- rbind(DRKS_DC_changes_join, DRKS_results_join) %>% 
  distinct(id, .keep_all = TRUE)


#----------------------------------------------------------------------------------------------------------------------
# joining of datasets and further cleaning
#----------------------------------------------------------------------------------------------------------------------

#unify variables 
id_step_name <- function(id_step_num)
{
  step_name = NA
  if(id_step_num == 0) {
    step_name = "No publ"
  } else if(id_step_num == 1) {
    step_name = "Registry linked"
  } else if(id_step_num == 2) {
    step_name = "Publ found in Google ID search"
  } else if(id_step_num == 3) {
    step_name = "Publ found in Google search (no ID)"
  } else if(id_step_num == 8) {
    step_name = "No publ"
  }
  
  return(step_name)
}

#combine all results and rename and add missing columns to get same structure as the IntoValue1 dataset
intovalue2_results <- rbind(CTgov_results_combined, DRKS_results_combined) 

#remove publication date & URL for all cases that are not counted as publication
intovalue2_results[intovalue2_results$indentification_step %in% c(0, 8),]$publication_date <- NA
intovalue2_results[intovalue2_results$indentification_step %in% c(0, 8),]$publication_DOI <- NA
intovalue2_results[intovalue2_results$indentification_step %in% c(0, 8),]$publication_URL <- NA

#rename and reformat variables
intovalue2_results <- intovalue2_results %>%
  rename(has_summary_results = were_results_reported,
         lead_cities = cities_lead_corrected,
         recruitment_status = overall_status,
         identification_step = indentification_step,
         study_registration_date = study_first_submitted_date) %>%
  mutate(has_publication = ifelse(identification_step %in% c(1,2,3), TRUE, FALSE),
         identification_step =  identification_step %>% map_chr(function(x) id_step_name(x)),
         intervention_type = intervention_type %>% replace_na("Not given"),
         allocation  = allocation  %>% replace_na("Not given"),
         allocation  = allocation  %>% str_replace("N/A", "Not given"),
         phase  = phase  %>% str_replace("N/A", "Not given"),
         phase  = phase  %>% str_replace(fixed("[---]*"), "Not given"),
         lead_cities = lead_cities %>% str_replace(fixed("TU"), "TU_München"),
         lead_cities = lead_cities %>% str_replace(fixed("LMU"), "LMU_München"),
         lead_cities = lead_cities %>% str_replace(fixed("Charite"), "Berlin"))

#reformat dates using lubridate and calculate time differences
intovalue2_results <- intovalue2_results %>%
  mutate(publication_date = dmy(publication_date),
         completion_date = ymd(completion_date),
         study_registration_date = ymd(study_registration_date),
         start_date = ymd(start_date),
         summary_res_date = ymd(summary_res_date)) %>%
  mutate(days_to_publication = publication_date - completion_date,
         days_to_summary = summary_res_date - completion_date,
         days_reg_to_start = start_date - study_registration_date,
         days_reg_to_compl = completion_date - study_registration_date,
         days_reg_to_publ = publication_date - study_registration_date)


#filter out studies that are not part of the trial, as no UMC was affiliated with them
intovalue2_results <- intovalue2_results %>% #filter(!is.na(identification_step))
  filter(identification_step != 9)


#calculate and add center size = small/large
#threshold defined as median of the total number of registered trials per institution
cities_trial_num <- intovalue2_results$lead_cities %>% 
  str_split(" ") %>% 
  unlist() %>%
  table()
median_trial_num <- cities_trial_num %>% median()
large_centers <- which(cities_trial_num > median_trial_num) %>% names()

intovalue2_results <- intovalue2_results %>%
  mutate(center_size = lead_cities %>% map_chr(function(x) 
    ifelse(x %>% str_detect(large_centers) %>% any(), "large", "small")))


#additional cleaning steps
intovalue2_results <- intovalue2_results %>%
  mutate(
  publication_DOI = str_trim(publication_DOI),
  publication_URL = str_trim(publication_URL),
  publication_DOI = str_remove(publication_DOI, "\\.$"),
  publication_DOI = str_remove(publication_DOI, "https?://(dx\\.)?doi.org/"),
  publication_DOI = str_remove(publication_DOI, "(?i)^doi\\s*"),
  publication_DOI = tolower(publication_DOI)
  )


#manually re-sort columns by topic
intovalue2_results <- intovalue2_results %>%
  select(id, lead_cities, has_publication, publication_DOI, 
         publication_URL, publication_date, identification_step,
         has_summary_results, summary_res_date,
         study_registration_date, start_date, completion_date,
         days_to_publication, days_to_summary,
         days_reg_to_start, days_reg_to_compl, days_reg_to_publ,
         recruitment_status, phase, enrollment, is_multicentric,
         main_sponsor, allocation, masking, intervention_type,
         center_size, is_CTgov)
         
    
#----------------------------------------------------------------------------------------------------------------------
# assertion checks
#----------------------------------------------------------------------------------------------------------------------

intovalue2_results %>%
  assert(function(doi) str_detect(doi, "^10\\.\\d{4,9}/[-.;()/:\\w\\d]+$")|is.na(doi), publication_DOI) %>%
  assert(function(url) str_detect(url, "^http")|is.na(url), publication_URL) %>%
  verify(nrow(filter(., has_publication & is.na(publication_URL) & is.na(publication_DOI))) == 0)

#additional checks
assert_that(any(intovalue2_results$has_publication & 
                  (is.na(intovalue2_results$publication_DOI) & 
                     is.na(intovalue2_results$publication_URL))) == FALSE)
missing_DOI_URL <- intovalue2_results[intovalue2_results$has_publication & 
  (is.na(intovalue2_results$publication_DOI) & 
  is.na(intovalue2_results$publication_URL)),]

assert_that(!any(intovalue2_results$has_publication & 
                   is.na(intovalue2_results$publication_date)))
missing_publ_date <- intovalue2_results[intovalue2_results$has_publication & 
                                          is.na(intovalue2_results$publication_date),]


#----------------------------------------------------------------------------------------------------------------------
# save cleaned IV2 dataset
#----------------------------------------------------------------------------------------------------------------------

write_csv(intovalue2_results, "data/2_dataset_cleaning/intermediate_cleaning_steps/IntoValue2_Dataset.csv")
