library(tidyverse)
library(lubridate)

#----------------------------------------------------------------------------------------------------------------------
# Loading of IV1 AACT dataset
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
# AACT_folder <- "C:/Datenablage/AACT/AACT dataset 20170416/" #insert the AACT download folder here
AACT_folder <- "data/raw/AACT dataset 20170416/" #insert the AACT download folder here

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
# AACT_folder <- "C:/Datenablage/AACT/AACT dataset 20200603/" #insert the AACT download folder here
AACT_folder <- "data/raw/AACT dataset 20200603/" #insert the AACT download folder here

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
# extract completion year
#----------------------------------------------------------------------------------------------------------------------

IV1_dates_join <- AACT_datasets_IV1$studies %>% 
  select(nct_id, first_received_results_date, first_received_date) %>% 
  rename(id = nct_id)

IntoValue1_dataset <- IntoValue1_dataset %>% 
  left_join(IV1_dates_join) %>%
  rename(summary_results_date = first_received_results_date,
         registration_date = first_received_date) %>%
  mutate(registration_date = ymd(registration_date),
         completion_year = year(completion_date))


#----------------------------------------------------------------------------------------------------------------------
# updates dates and days calculations for IV2 dataset
# add the missing primary completion date to the IV2 dataset
# aact changed the default for start and (primary) completion dates between IV1 and IV2
# for IV1, dates default to first of month
# for IV2, dates default to last of month
# for consistency, recalculate IV2 dates from raw text (month year)
#----------------------------------------------------------------------------------------------------------------------

PCD_join <- AACT_datasets_IV2$studies %>% 
  select(nct_id, study_first_submitted_date, 
         start_month_year, completion_month_year, primary_completion_month_year) %>% 
  rename(id = nct_id,
         study_registration_date = study_first_submitted_date)

IntoValue2_dataset <- IntoValue2_dataset %>% 
  left_join(PCD_join) %>%
  rename(summary_results_date = summary_res_date,
         days_reg_to_publication = days_reg_to_publ) %>% 
  
  # Get new ctgov date and merge with drks
  # First make ctgov dates NA so no incorrect left over
  mutate(
    completion_date = if_else(is_CTgov, as_date(NA), completion_date),
    completion_date_ctgov_updated = as_date(parse_date_time(completion_month_year, c("my", "mdY"))),
    completion_date = coalesce(completion_date_ctgov_updated, completion_date),
    
    start_date = if_else(is_CTgov, as_date(NA), start_date),
    start_date_ctgov_updated = as_date(parse_date_time(start_month_year, c("my", "mdY"))),
    start_date = coalesce(start_date_ctgov_updated, start_date)
  ) %>%
  select(-completion_date_ctgov_updated, -start_date_ctgov_updated) %>% 
  
  mutate(completion_year = year(completion_date),
         primary_completion_date = as_date(parse_date_time(primary_completion_month_year, c("my", "mdY"))),
         primary_completion_year = year(primary_completion_date),
         registration_date = ymd(study_registration_date),
         days_cd_to_summary = summary_results_date - completion_date,
         days_cd_to_publication = publication_date - completion_date,
         days_reg_to_cd = completion_date - registration_date,
         days_pcd_to_summary = summary_results_date - primary_completion_date,
         days_pcd_to_publication = publication_date - primary_completion_date,
         days_reg_to_pcd = primary_completion_date - registration_date,
         days_reg_to_start = start_date - registration_date) %>% 
  select(-ends_with("month_year"), 
         -c(study_registration_date, days_to_publication, days_to_summary, days_reg_to_compl)
  )
         



#----------------------------------------------------------------------------------------------------------------------
# rename and modify IV1 variables to match the IV2 dataset format
#----------------------------------------------------------------------------------------------------------------------

IntoValue1_dataset$is_multicentric[IntoValue1_dataset$is_multicentric == "Not given"] <- NA

#DRKS only has a completion date - remove PCD entries from DRKS entries in IV1 dataset
IntoValue1_dataset$primary_completion_date[IntoValue1_dataset$is_CTgov == "no"] <- NA
IntoValue1_dataset$primary_completion_year[IntoValue1_dataset$is_CTgov == "no"] <- NA
IntoValue1_dataset$days_reg_to_PCD[IntoValue1_dataset$is_CTgov == "no"] <- NA
IntoValue1_dataset$days_to_publication_PCD[IntoValue1_dataset$is_CTgov == "no"] <- NA


IntoValue1_dataset <- IntoValue1_dataset %>%
  rename(identification_step = publication_type,
         has_publication = publications,
         has_summary_results = summary_results,
         recruitment_status = recruitmentStatus,
         days_pcd_to_publication = days_to_publication_PCD,
         days_cd_to_publication = days_to_publication_CD,
         days_pcd_to_summary = days_to_summary_PCD,
         days_cd_to_summary = days_to_summary_CD,
         days_reg_to_cd = days_reg_to_CD,
         days_reg_to_pcd = days_reg_to_PCD,
         days_reg_to_publication = days_reg_to_publ) %>%
  #to change seperator
  mutate(lead_cities = lead_cities %>% str_replace_all("TU München", "TU_München"),
         lead_cities = lead_cities %>% str_replace_all("LMU München", "LMU_München"),
         lead_cities = lead_cities %>% str_replace(fixed("Kiel"), "Schleswig-Holstein"),
         lead_cities = lead_cities %>% str_replace(fixed("Lübeck"), "Schleswig-Holstein"),
         facility_cities = facility_cities %>% str_replace_all("TU München", "TU_München"),
         facility_cities = facility_cities %>% str_replace_all("LMU München", "LMU_München"),
         facility_cities = facility_cities %>% str_replace(fixed("Kiel"), "Schleswig-Holstein"),
         facility_cities = facility_cities %>% str_replace(fixed("Lübeck"), "Schleswig-Holstein"),
         is_CTgov = ifelse(is_CTgov == "yes", TRUE, FALSE),
         is_multicentric = ifelse(is_multicentric == "yes", TRUE, FALSE),
         allocation  = allocation  %>% replace_na("Not given"),
         has_german_umc_lead = if_else(lead_or_facility == "lead", TRUE, FALSE),
         facility_cities = na_if(facility_cities, "")) %>%
  select(-lead_or_facility)


#----------------------------------------------------------------------------------------------------------------------
# rename and modify IV2 variables
#----------------------------------------------------------------------------------------------------------------------

IntoValue2_dataset <- IntoValue2_dataset %>%
  mutate(has_german_umc_lead = TRUE,
         publication_PMID = NA,
         facility_cities = NA)


#check for missing rows
IV1_notin_IV2 <- colnames(IntoValue1_dataset)[!(colnames(IntoValue1_dataset) %in% colnames(IntoValue2_dataset))]
IV2_notin_IV1 <- colnames(IntoValue2_dataset)[!(colnames(IntoValue2_dataset) %in% colnames(IntoValue1_dataset))]
IV1_notin_IV2
IV2_notin_IV1


#----------------------------------------------------------------------------------------------------------------------
# combine iv datasets
#----------------------------------------------------------------------------------------------------------------------

#add iv version to datasets
IntoValue1_dataset <- IntoValue1_dataset %>% 
  mutate(iv_version = 1)
IntoValue2_dataset <- IntoValue2_dataset %>% 
  mutate(iv_version = 2)

IntoValue_datasets_comb <-
  
  #combine datasets
  rbind(IntoValue2_dataset, IntoValue1_dataset) %>%
  
  #label duplicate trials (both iv1 and iv2 version labeled as dupes)
  group_by(id) %>% 
  mutate(is_dupe = if_else(n() > 1, TRUE, FALSE)) %>% 
  ungroup() %>% 
  
  #calculate whether registration is prospective
  #round start and registration to month and see whether the same month
  mutate(
    is_prospective = 
      floor_date(registration_date, unit = "month") <=
      floor_date(start_date, unit = "month")
  ) %>%
  
  #make boolean is_randomized column
  mutate(
    is_randomized = case_when(
      allocation %in% c("Randomized", "Randomized controlled trial") ~ TRUE,
      allocation %in% c("Non-Randomized", "Non-randomized controlled trial",
                        "Other", "Single arm study") ~ FALSE,
      allocation == "Not given" ~ NA
    )
  ) %>%
  
  #convert is_ctgov to registry
  mutate(registry = if_else(is_CTgov, "ClinicalTrials.gov", "DRKS"), .keep = "unused")


#manually re-sort columns by topic
IntoValue_datasets_comb <- IntoValue_datasets_comb %>%
  select(id, registry, lead_cities, 
         has_publication, 
         publication_doi = publication_DOI, 
         publication_pmid = publication_PMID, 
         publication_url = publication_URL,
         publication_date, identification_step,
         is_prospective,
         has_summary_results, summary_results_date,
         registration_date, start_date, 
         completion_date, completion_year, 
         primary_completion_date, primary_completion_year, 
         days_cd_to_publication, days_pcd_to_publication,
         days_cd_to_summary, days_pcd_to_summary,
         days_reg_to_start, days_reg_to_cd, days_reg_to_pcd, days_reg_to_publication,
         recruitment_status, phase, enrollment, is_multicentric,
         main_sponsor, allocation, is_randomized, masking, intervention_type, center_size,
         has_german_umc_lead, facility_cities, iv_version, is_dupe)

write_csv(IntoValue_datasets_comb, "data/IV1_IV2_combined_dataset.csv")
