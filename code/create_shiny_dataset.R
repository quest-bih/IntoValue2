library(tidyverse)
library(assertthat)

#load combined dataset
iv_dataset <- read_csv("data/iv_main_dataset.csv")


#----------------------------------------------------------------------------------------------------------------------
# make one row per trial & UMC combination for easy subsetting
#----------------------------------------------------------------------------------------------------------------------

#all used city names
cities <- iv_dataset$lead_cities %>% str_split(" ") %>% unlist() %>% unique() %>% sort()

#function to spread table to one row per trial & UMC combination
#has to be called separately for lead and facility
make_trial_UMC_table <- function(iv_data, cities, city_col, category)
{
  #calculate list of logical vectors for each city name, assigning studies to city name
  cities_in_studies <- lapply(cities, grepl, x = iv_data[[city_col]])
  names(cities_in_studies) <- cities
  cities_in_studies <- do.call(cbind, cities_in_studies)
  #insert NA for cases where we dont have a hit for later gathering into one column to make dataset tidy
  cities_in_studies[cities_in_studies == FALSE] <- NA
  
  #add the new colums to the dataset, allowing easy subsetting of the data
  iv_data_trials_per_city <- as_tibble(cbind(iv_data, cities_in_studies)) %>%
    gather(all_of(cities), key = "city", value = "City_NCT_Combination", na.rm = TRUE) %>%
    mutate(lead_or_facility = category) %>%
    select(-City_NCT_Combination)  %>% 
    arrange(id)
  
  return(iv_data_trials_per_city)
}

#create one table for all lead trial & for all facility trials (IV1 only), then combine
iv_trials_per_lead_city <- make_trial_UMC_table(iv_dataset, cities, "lead_cities", "lead")
iv_trials_per_facility_city <- make_trial_UMC_table(iv_dataset, cities, "facility_cities", "facility")
iv_trials_per_city <- rbind(iv_trials_per_lead_city, iv_trials_per_facility_city)

#check for trials missing in the modified dataset
missing_trials <- iv_dataset[!(iv_dataset$id %in% iv_trials_per_city$id ),]
assert_that(dim(missing_trials)[1] == 0)


#check that all cities have the right number of entries in the modified dataset
iv_lead_city_table <- iv_dataset$lead_cities %>% str_split(" ") %>% unlist() %>% table()
iv_lead_trial_per_city_table <- iv_trials_per_lead_city$city %>% table()
assert_that(all(iv_lead_city_table == iv_lead_trial_per_city_table))


#add a row for each trial with the "All trials combined" tag
iv_dataset_all_trials <- iv_dataset %>%
  mutate(city = "All trials combined",
         lead_or_facility = ifelse(is.na(lead_cities), "facility", "lead"))
iv_trials_per_city <- iv_trials_per_city %>%
  rbind(iv_dataset_all_trials)


#unify city names
iv_trials_per_city$city[iv_trials_per_city$city == "TU_München"] <- "TU München"
iv_trials_per_city$city[iv_trials_per_city$city == "LMU_München"] <- "LMU München"



write_rds(iv_trials_per_city, "shiny_app/data/IntoValue_Dataset_combined.rds")
write_csv(iv_trials_per_city, "shiny_app/data/IntoValue_Dataset_combined.csv")
