library(tidyverse)



#----------------------------------------------------------------------------------------------------------------------
# bring IV2 dataset in the same shape as the IV1 dataset used for the shiny app
# with one row for each combination of trial & UMC for easy filtering
#----------------------------------------------------------------------------------------------------------------------

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

#load combined dataset
#....

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


#combine datasets
IntoValue1_dataset <- IntoValue1_dataset %>% 
  mutate(IV_version = "IV1")
IntoValue_dataset_trials_per_city <- IntoValue_dataset_trials_per_city %>% 
  mutate(IV_version = "IV2")

#remove duplicate trials in IV1 dataset before joining! - for now we remove the results from IV1 in those cases
dupl_trials_IV1 <- IntoValue1_dataset[(IntoValue1_dataset$id %in% IntoValue_dataset$id),]$id
dupl_trials_IV2 <- IntoValue_dataset_trials_per_city[(IntoValue_dataset_trials_per_city$id %in% IntoValue1_dataset$id),]$id

IntoValue1_dataset[IntoValue1_dataset$id %in% dupl_trials_IV1,]$IV_version <- "IV1_dupl"
IntoValue_dataset_trials_per_city[IntoValue_dataset_trials_per_city$id %in% dupl_trials_IV2,]$IV_version <- "IV2_dupl"



IntoValue_dataset_comb <- rbind(IntoValue_dataset_trials_per_city, IntoValue1_dataset)  %>%
  mutate(completion_year = str_sub(completion_date, 1, 4))


write_rds(IntoValue_dataset_comb, "shiny_app/data/IntoValue_Dataset_combined.rds")
