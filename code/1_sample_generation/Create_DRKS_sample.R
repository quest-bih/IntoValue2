#----------------------------------------------------------------------------------------------------------------------
#
# The following script creates the sample of trials with contributing German university medical centers (UMC)
# from the DRKS database. Here, a pre-filtered version of the dataset was downloaded from the DRKS database
# (found in DRKS_search_results_2009-2014.csv), filtering for completion years and study status as well as
# Germany as 'Country of recruitment'.
#
# The script searches the DRKS dataset for affiliations of the sponsor/PI/responsible party/recruitment locations
# associated with the different UMCs (keywords are loaded from city_search_terms.csv).
#
# The script saves a filtered version of the dataset, only containing the relevant trials. Please be
# aware that the filtered dataset still contains false positives (i.e. trials that were found with the
# keywords but that were not associated with the UMCs - e.g. when a communal hospital in Berlin was found
# by the keyword "Berlin"). All trial affiliations were checked during the manual publication search to
# remove false positives.
#
#----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)


#----------------------------------------------------------------------------------------------------------------------
# Data loading and transformation
#----------------------------------------------------------------------------------------------------------------------

completion_years <- c("2014", "2015", "2016", "2017") %>%
  paste(collapse="|")

#data downloaded from DRKS with specified search query
DRKS_sample <- read_delim("data/1_sample_generation/DRKS_downloaded.csv", delim = ";")
DRKS_sample <- DRKS_sample %>%
  arrange(drksId) %>%
  filter(grepl(completion_years, studyEnd))  #select only years with study end between 2009 and 2013


#----------------------------------------------------------------------------------------------------------------------
# Affiliation search
#----------------------------------------------------------------------------------------------------------------------

get_drks_id <- function(affil_indices, dataset)
{
  drks_id <- dataset %>%
    slice(affil_indices) %>%
    select(drksId)
  return(drks_id[[1]])
}

#affiliation columns used for the search
affil_columns <- paste0("address.affiliation",0:4)

#affiliations of different columns pasted together to simplify search
affiliations <- apply(DRKS_sample[affil_columns], 1, paste, collapse = " ")

# Load search terms for the affiliations/cities
city_search_terms <- readLines("data/1_sample_generation/city_search_terms.csv", encoding = "UTF-8") %>%
  str_split(";")
cities <- city_search_terms %>% map_chr(1)
city_search_terms <- city_search_terms %>%
  map(function(x) paste0("\\b", x, "\\b", collapse = "|"))
names(city_search_terms) <- cities


#actual search
affil_grep_idx <- map(city_search_terms, grep, x=affiliations)
affil_grep <- map(affil_grep_idx, get_drks_id, dataset=DRKS_sample)


#for each study we want to know which city has
#a lead (PI/sponsor/responsible_party) or facility affiliation or any affil
affil_drks_ids_lead <- affil_grep

unique_drks_ids_lead <- unique(unlist(affil_drks_ids_lead))

#filter cases for affiliation
DRKS_sample <- DRKS_sample %>%
  filter(drksId %in% unique_drks_ids_lead)


#----------------------------------------------------------------------------------------------------------------------
# create for each study a list of affiliated cities and add to main table
#----------------------------------------------------------------------------------------------------------------------

get_city_per_drks_id <- function(cities_drks_id_list, unique_drks_ids)
{
  cities_col <- vector("list", length(unique_drks_ids))
  names(cities_col) <- unique_drks_ids
  for (city in names(cities_drks_id_list)) {
    cities_col[cities_drks_id_list[[city]]] <-
      paste(cities_col[cities_drks_id_list[[city]]], city, sep = " ")
  }
  cities_col <- substring(cities_col, first = 6)
  names(cities_col) <- unique_drks_ids
  return(cities_col)
}

#create columns that list which cities are affiliated with the studies
drks_id_cities_lead <- get_city_per_drks_id(affil_drks_ids_lead, unique_drks_ids_lead)

#prepare for joining with main table
drks_id_cities_lead_tbl <- as_tibble(cbind(unique_drks_ids_lead, drks_id_cities_lead))
names(drks_id_cities_lead_tbl) <- c("drksId", "cities_lead")

#add columns to main table
DRKS_sample <- DRKS_sample %>%
  left_join(drks_id_cities_lead_tbl, by = "drksId")


#----------------------------------------------------------------------------------------------------------------------
# Comparison with CT.gov dataset to get double entries
#----------------------------------------------------------------------------------------------------------------------

#check the columns for the secondary IDs for NCT ids
id_columns <- DRKS_sample[,grep("secId.id",colnames(DRKS_sample))]
nct_entries_columns <- apply(id_columns, 2, str_detect, pattern = "NCT")
nct_entries <- apply(nct_entries_columns, 1, any, na.rm = TRUE)

#extract the NCTs
nct_pos <- apply(nct_entries_columns, 1, which)
nct_pos[sapply(nct_pos, length) ==  0] <- NA
nct_pos <- unlist(nct_pos)

entry_num <- dim(DRKS_sample)[1]
ncts <- rep("", entry_num)
for(i in 1:entry_num)
{
  if(is.na(nct_pos[i])) {
    ncts[i] <- NA
  } else {
    ncts[i] <- id_columns[[i, nct_pos[i]]]
  }
}

#add information on NCT-ids and filter studies with NCT (and thus are already registered on CT.gov)
DRKS_sample <- DRKS_sample %>%
  add_column(has_nct_id = nct_entries) %>%
  add_column(nct_id = ncts) %>%
  filter(has_nct_id == FALSE)

DRKS_sample_save <- DRKS_sample %>%
  rename(startDate_plannedActual = plannedActual) %>% 
  select(drksId, cities_lead, title,
         firstDrksPublishDate, startDate,
         startDate_plannedActual, studyEnd,
         intervention.category0, intervention.value0,
         intervention.category1, intervention.value1,
         intervention.category2, intervention.value2,
         address.type0, address.affiliation0, 
         address.firstname0, address.lastname0,
         address.type1, address.affiliation1, 
         address.firstname1, address.lastname1,
         targetSize, recruitmentStatus,
         investorInitiated, monoMultiCentric,
         publication.category0, publication.type0, publication.value0,
         publication.category1, publication.type1, publication.value1,
         publication.category2, publication.type2, publication.value2,
         publication.category3, publication.type3, publication.value3,
         publication.category4, publication.type4, publication.value4)


#save DRKS trial sample
write_delim(DRKS_sample_save, "data/1_sample_generation/IntoValue2_DRKS_sample.csv", delim = ";", na = "")
