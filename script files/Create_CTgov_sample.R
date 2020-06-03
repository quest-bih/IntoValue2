#----------------------------------------------------------------------------------------------------------------------
#
# The following script creates the sample of trials with contributing German university medical centers (UMC)
# from the clinicaltrials.gov database. Here, we rely on the AACT dataset, which brings the CT.gov database
# content in an easily reusable format. As the AACT dataset is very big (several GB), this has to be
# downloaded from https://aact.ctti-clinicaltrials.org/pipe_files by the user and the AACT folder
# has to be inserted by the user in the first code line of the script.
#
# The script searches the AACT dataset for affiliations of the sponsor/PI/responsible party/facilities
# associated with the different UMCs (keywords are loaded from city_search_terms.csv). It also filters
# the relevant completion years and study status (Completed, Terminated, Suspended, Unknown status).
#
# The script saves a filtered version of the dataset, only containing the relevant trials. Please be
# aware that the filtered dataset still contains false positives (i.e. trials that were found with the
# keywords but that were not associated with the UMCs - e.g. when a communal hospital in Berlin was found
# by the keyword "Berlin"). All trial affiliations were checked during the manual publication search to
# remove false positives.
#
#----------------------------------------------------------------------------------------------------------------------


library(tidyverse)

#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset
#----------------------------------------------------------------------------------------------------------------------

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
AACT_folder <- "data/AACT_20200603/" #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values")

AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".txt")
AACT_datasets <- AACT_dataset_files %>%
  map(read_delim, delim = "|")
names(AACT_datasets) <- AACT_dataset_names


#----------------------------------------------------------------------------------------------------------------------
# Load search terms for the affiliations/cities
#----------------------------------------------------------------------------------------------------------------------

#different seach terms for each university medical center are stored loaded from this csv
city_search_terms <- readLines("data/city_search_terms_no_abbrev.csv", encoding = "UTF-8") %>%
  str_split(";")
cities <- city_search_terms %>% map_chr(1)
city_search_terms <- city_search_terms %>%
  map(function(x) paste0("\\b", x, "\\b", collapse = "|"))
names(city_search_terms) <- cities


#----------------------------------------------------------------------------------------------------------------------
#  search for studies affiliated with a german medical faculty and get NCTs of those studies
#----------------------------------------------------------------------------------------------------------------------

#we want to find the names of the different universities in the
#PI/sponsor/responsible_party/facilities columns
grep_fast <- function(pattern, x)
{
  return(which(str_detect(x, pattern)))
}

get_nct <- function(affil_indices, dataset)
{
  ncts <- dataset %>%
    slice(affil_indices) %>%
    select(nct_id)
  return(ncts[[1]])
}

city_grep <- function(dataset, colname, grep_terms)
{
  indices <- map(grep_terms, grep_fast, x=dataset[[colname]])
  city_ncts <- map(indices, get_nct, dataset=dataset)
  return(city_ncts)
}

city_grep_indices <- function(dataset, colname, grep_terms)
{
  indices <- map(grep_terms, grep_fast, x=dataset[[colname]])
  return(indices)
}

#search the different affilation datasets for the city search terms
grep_PI <- city_grep(AACT_datasets$overall_officials, "affiliation", city_search_terms)
grep_sponsor <- city_grep(AACT_datasets$sponsors %>% filter(lead_or_collaborator == "lead"), "name", city_search_terms)
grep_resp_party_org <- city_grep(AACT_datasets$responsible_parties, "organization", city_search_terms)
grep_resp_party_affil <- city_grep(AACT_datasets$responsible_parties, "affiliation", city_search_terms)


#joining of the different grep results
affil_join <- function(affil_nct_list)
{
  affil_indices_joined <- affil_nct_list %>%
    pmap(c) %>%
    map(unique) %>%
    map(sort)
}

#combine the results for the different columns to get the studies with a match for
#a lead (PI/sponsor/responsible_party) or facility affiliation
grep_results_lead <- list(grep_PI, grep_sponsor, grep_resp_party_org, grep_resp_party_affil)

#for each study we want to know which city has
#a lead (PI/sponsor/responsible_party) or facility affiliation or any affil
affil_ncts_lead <- affil_join(grep_results_lead)

#get the unique study IDs
unique_ncts_lead <- unique(unlist(affil_ncts_lead))


#----------------------------------------------------------------------------------------------------------------------
# reduce the CTgov dataset to those studies that are indeed affiliated
# and filter for lead completion years & study status
#----------------------------------------------------------------------------------------------------------------------

CTgov_sample <- AACT_datasets$studies

completion_years <- c("2014", "2015", "2016", "2017") %>%
  paste(collapse="|")
study_status <- c("Completed" , "Terminated" , "Suspended", "Unknown status") %>%
  paste(collapse="|")

#filter cases for affiliation, years, study status, and study type
CTgov_sample <- CTgov_sample %>%
  filter(nct_id %in% unique_ncts_lead) %>%
  filter(grepl(completion_years, completion_date)) %>%
  filter(grepl(study_status, overall_status)) %>%
  filter(study_type == "Interventional")


#----------------------------------------------------------------------------------------------------------------------
# create for each study a list of affiliated cities and add to main table
#----------------------------------------------------------------------------------------------------------------------

get_city_per_NCT <- function(cities_nct_list, unique_ncts)
{
  cities_col <- vector("list", length(unique_ncts))
  names(cities_col) <- unique_ncts
  for (city in names(cities_nct_list)) {
    cities_col[cities_nct_list[[city]]] <-
      paste(cities_col[cities_nct_list[[city]]], city, sep = " ")
  }
  cities_col <- substring(cities_col, first = 6)
  names(cities_col) <- unique_ncts
  return(cities_col)
}

#create columns that list which cities are affiliated with the studies
nct_cities_lead <- get_city_per_NCT(affil_ncts_lead, unique_ncts_lead)

#prepare for joining with main table
nct_cities_lead_tbl <- as_tibble(cbind(unique_ncts_lead, nct_cities_lead))
names(nct_cities_lead_tbl) <- c("nct_id", "cities_lead")

#add columns to main table
CTgov_sample <- CTgov_sample %>%
  left_join(nct_cities_lead_tbl, by = "nct_id")



#add PI affil info to main table
#first get only affils of relevant PIs from full table
grep_PI_indices <- city_grep_indices(AACT_datasets$overall_officials, "affiliation", city_search_terms) %>% 
  unlist() %>% unique() %>% sort() 
PI_affils_table_filtered <- AACT_datasets$overall_officials[grep_PI_indices,] %>%
  distinct(nct_id, .keep_all = TRUE) #only take first relevant PI for each study to allow a clean join

CTgov_sample <- CTgov_sample %>%
  left_join(PI_affils_table_filtered, by = "nct_id")

#add intervantion name
interventions_combined <- AACT_datasets$interventions %>%
  group_by(nct_id) %>%
  summarise(intervention_names_comb = paste(name, collapse=" | "))

CTgov_sample <- CTgov_sample %>%
  left_join(interventions_combined, by = "nct_id")


#add calculated values
CTgov_sample <- CTgov_sample %>%
  left_join(AACT_datasets$calculated_values, by = "nct_id")


#save CT.gov trial sample
#please be aware that not all associations of the trials to the cites are correct (there are still false positives)
#such that the city associations had to be checked manually during publication search
write_delim(CTgov_sample, "data/IntoValue2_CTgov_sample_no_abbrev.csv", delim = ";", na = "")

