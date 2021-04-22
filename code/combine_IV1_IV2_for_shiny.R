library(tidyverse)

#read in dataset
IntoValue_dataset <- read_csv("data/IntoValue2_Dataset.csv", na = "NA")
IntoValue1_dataset <- readRDS("shiny_app/data/CT_gov_2.rds")


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
         publication_PMID = NA,
         `Days to publication PCD` = NA,
         `Days to summary PCD` = NA,
         days_PCD_to_reg = NA,
         primary_completion_date = NA,
         primary_completion_year = NA)

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

IntoValue_dataset_comb <- rbind(IntoValue_dataset_trials_per_city, IntoValue1_dataset_dedupl)


write_rds(IntoValue_dataset_comb, "shiny_app/data/IntoValue_Dataset_combined.rds")
