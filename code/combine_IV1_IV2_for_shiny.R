library(tidyverse)
library(lubridate)

#read in dataset
IntoValue_dataset <- read_csv("data/IntoValue2_Dataset.csv", na = "NA")
IntoValue1_dataset <- readRDS("shiny_app/data/CT_gov_2.rds")

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
AACT_folder <- "C:/Datenablage/AACT/AACT dataset 20200603/" #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values")

AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".txt")
AACT_datasets <- AACT_dataset_files %>%
  map(read_delim, delim = "|")
names(AACT_datasets) <- AACT_dataset_names


#----------------------------------------------------------------------------------------------------------------------
# add the missing primary completion date to the IV2 dataset
#----------------------------------------------------------------------------------------------------------------------

PCD_join <- AACT_datasets$studies %>% 
  select(nct_id, primary_completion_date, study_first_submitted_date) %>% 
  rename(id = nct_id)

IntoValue_dataset <- read_csv("data/IntoValue2_Dataset.csv", na = "NA")
IntoValue_dataset <- IntoValue_dataset %>% 
  left_join(PCD_join) %>%
  mutate(primary_completion_year = primary_completion_date %>% str_sub(1,4),
         primary_completion_date = primary_completion_date %>% ymd(),
         study_first_submitted_date = study_first_submitted_date %>% ymd(),
         `Days to summary PCD` = summary_res_date - primary_completion_date,
         `Days to publication PCD` = publication_date - primary_completion_date,
         days_PCD_to_reg = primary_completion_date - study_first_submitted_date)

#----------------------------------------------------------------------------------------------------------------------
# rename and modify IV2 variables to match the IV1 dataset format
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
    step_name = "Google ID search"
  } else if(id_step_num == 3) {
    step_name = "Hand search"
  } else if(id_step_num == 8) {
    step_name = "No publ"
  }
  
  return(step_name)
}

IntoValue_dataset <- IntoValue_dataset %>% 
  rename(`Days to publication CD` = days_to_publication,
         `Days to summary CD` = days_to_summary,
         `Summary results` = has_summary_results,
         Publications = has_publication,
         recruitmentStatus = overall_status,
         days_CD_to_reg = days_reg_to_compl,
         days_start_to_reg = days_reg_to_start,
         days_publ_to_reg = days_reg_to_publ) %>%
  mutate(is_CTgov = is_CTgov %>% map_chr(function(x) ifelse(x, "yes", "no")),
         lead_or_facility = "lead",
         publication_PMID = NA)

IntoValue_dataset$`Publication type` = IntoValue_dataset$indentification_step %>% map_chr(id_step_name)
IntoValue_dataset$intervention_type <- IntoValue_dataset$intervention_type %>% replace_na("Not given")


#remove additional IV2 rows for now  
IntoValue_dataset <- IntoValue_dataset %>% 
  select(-indentification_step, -study_first_submitted_date,
         -summary_res_date, -start_date)



#spread out IV2 dataset to one row for each trial + participating lead city
cities <- IntoValue_dataset$lead_cities %>% str_split(" ") %>% unlist() %>% unique() %>% sort()
#calculate list of logical vectors for each city name, assinging studies to city name
cities_in_studies <- lapply(cities, grepl, x = IntoValue_dataset$lead_cities)
names(cities_in_studies) <- cities
cities_in_studies <- do.call(cbind, cities_in_studies)
#insert NA for cases where we dont have a hit for later gathering into one column to make dataset tidy
cities_in_studies[cities_in_studies == FALSE] <- NA

#add the new colums to the dataset, allowing easy subsetting of the data
IntoValue_dataset_trials_per_city <- as_tibble(cbind(IntoValue_dataset, cities_in_studies))%>%
  gather(cities, key = "city", value = "City_NCT_Combination", na.rm = TRUE) %>%
  select(-City_NCT_Combination) %>% 
  arrange(id)

#add a row for each trial with the "All trials combined" tag
IntoValue_dataset_all_trials <- IntoValue_dataset %>%
  mutate(city = "All trials combined")
IntoValue_dataset_trials_per_city <- IntoValue_dataset_trials_per_city %>%
  rbind(IntoValue_dataset_all_trials) %>%
  select(-lead_cities)

#check for remaning differences in variable names
common_variables <- colnames(IntoValue_dataset_trials_per_city)[(colnames(IntoValue_dataset_trials_per_city) %in% colnames(IntoValue1_dataset))]
variables_only_IV2 <- colnames(IntoValue_dataset_trials_per_city)[!(colnames(IntoValue_dataset_trials_per_city) %in% colnames(IntoValue1_dataset))]
variables_only_IV1 <- colnames(IntoValue1_dataset)[!(colnames(IntoValue1_dataset) %in% colnames(IntoValue_dataset_trials_per_city))]
common_variables
variables_only_IV2
variables_only_IV1


#unify city names
IntoValue_dataset_trials_per_city$city[IntoValue_dataset_trials_per_city$city == "TU"] <- "TU München"
IntoValue_dataset_trials_per_city$city[IntoValue_dataset_trials_per_city$city == "LMU"] <- "LMU München"
IntoValue_dataset_trials_per_city$city[IntoValue_dataset_trials_per_city$city == "Charite"] <- "Berlin"

#also combine Kiel and Lübeck from IV1
IntoValue1_dataset$city[IntoValue1_dataset$city == "Kiel"] <- "Schleswig-Holstein"
IntoValue1_dataset$city[IntoValue1_dataset$city == "Lübeck"] <- "Schleswig-Holstein"

#need to remove duplicate cases that were counted for Kiel and Lübeck before
IntoValue1_dataset <- IntoValue1_dataset %>% distinct(id, city, .keep_all = TRUE)


#check for remaning differences in city names
IV1_cities <- IntoValue1_dataset$city %>% unique() %>% sort()
IV2_cities <- IntoValue_dataset_trials_per_city$city %>% unique() %>% sort()
cities_only_IV1 <- IV1_cities[!(IV1_cities %in% IV2_cities)]
cities_only_IV2 <- IV2_cities[!(IV2_cities %in% IV1_cities)]
cities_only_IV1
cities_only_IV2


#remove duplicate trials in IV1 dataset before joining! - for now we remove the results from IV1 in those cases
dupl_trials <- IntoValue1_dataset[(IntoValue1_dataset$id %in% IntoValue_dataset$id),]
IntoValue1_dataset_dedupl <- IntoValue1_dataset[!(IntoValue1_dataset$id %in% IntoValue_dataset$id),]


#combine datasets
IntoValue1_dataset_dedupl <- IntoValue1_dataset_dedupl %>% 
  mutate(IV_version = "IV1")
IntoValue_dataset_trials_per_city <- IntoValue_dataset_trials_per_city %>% 
  mutate(IV_version = "IV2")

IntoValue_dataset_comb <- rbind(IntoValue_dataset_trials_per_city, IntoValue1_dataset_dedupl)  %>%
  mutate(completion_year = str_sub(completion_date, 1, 4))


write_rds(IntoValue_dataset_comb, "shiny_app/data/IntoValue_Dataset_combined.rds")
