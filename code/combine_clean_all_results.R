library(tidyverse)
library(lubridate)
library(assertthat)

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset for additional variables
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
AACT_folder <- "C:/Datenablage/AACT/AACT dataset 20200603/" #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values", "designs")

AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".txt")
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

DRKS_add_columns <- read_delim("generated_samples/DRKS/DRKS_combined.csv", delim = ";")  %>%
  select(drksId, allocation, masking, phase)

#----------------------------------------------------------------------------------------------------------------------
# Loading of results datasets
#----------------------------------------------------------------------------------------------------------------------


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

#standardize column names for joining
CTgov_results <- read_csv("manual_check/final_results/IntoValue2_CTgov_results_main_check_including_DC.csv") %>%
  standardize_CTgov()
DRKS_results <- read_csv("manual_check/final_results/IntoValue2_DRKS_results_main_check_including_DC.csv") %>%
  standardize_DRKS()

CTgov_DC_changes <- read_csv("manual_check/final_results/IntoValue2_extended_DC_CTgov_changes.csv") %>%
  standardize_CTgov()
DRKS_DC_changes <- read_csv("manual_check/final_results/IntoValue2_extended_DC_DRKS_changes.csv") %>%
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

CTgov_results_join <- CTgov_results %>% select(analysis_cols)
DRKS_results_join <- DRKS_results %>% select(analysis_cols)
CTgov_DC_changes_join <- CTgov_DC_changes %>% select(analysis_cols)
DRKS_DC_changes_join <- DRKS_DC_changes %>% select(analysis_cols)


#implement double check changes to dataset - keep only the results from the DC changes
CTgov_results_combined <- rbind(CTgov_DC_changes_join, CTgov_results_join) %>% 
  distinct(id, .keep_all = TRUE)
DRKS_results_combined <- rbind(DRKS_DC_changes_join, DRKS_results_join) %>% 
  distinct(id, .keep_all = TRUE)



#combine all results and rename and add missing columns to get same structure as the IntoValue1 dataset
intovalue2_results <- rbind(CTgov_results_combined, DRKS_results_combined) 

#remove publication date for all cases that are not counted as publication
intovalue2_results[intovalue2_results$indentification_step %in% c(0, 8),]$publication_date <- NA
intovalue2_results[intovalue2_results$indentification_step %in% c(0, 8),]$publication_DOI <- NA
intovalue2_results[intovalue2_results$indentification_step %in% c(0, 8),]$publication_URL <- NA


intovalue2_results <- intovalue2_results %>%
  rename("has_summary_results" = "were_results_reported",
         "lead_cities" = "cities_lead_corrected") %>%
  mutate(has_publication = ifelse(indentification_step %in% c(1,2,3), TRUE, FALSE),
         publication_date = dmy(publication_date),
         completion_date = ymd(completion_date),
         study_first_submitted_date = ymd(study_first_submitted_date),
         start_date = ymd(start_date),
         summary_res_date = ymd(summary_res_date)) %>%
  mutate(days_to_publication = publication_date - completion_date,
         days_to_summary = summary_res_date - completion_date,
         days_reg_to_start = start_date - study_first_submitted_date,
         days_reg_to_compl = completion_date - study_first_submitted_date,
         days_reg_to_publ = publication_date - study_first_submitted_date)

#filter out studies that are not part of the trial, as no UMC was affiliated with them
intovalue2_results <- intovalue2_results %>%
  filter(indentification_step != 9)



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


write_csv(intovalue2_results, "data/IntoValue2_Dataset.csv")
