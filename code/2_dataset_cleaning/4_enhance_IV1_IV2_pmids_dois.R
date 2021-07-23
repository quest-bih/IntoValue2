library(tidyverse)
library(lubridate)

source("code/2_dataset_cleaning/functions/convert_id_single_type.R")

# Add pmids and dois

intovalue <- 
  read_csv(
    "data/2_dataset_cleaning/intermediate_cleaning_steps/iv_clean_dataset.csv",
    col_types = cols(
      publication_pmid = col_number(),
      facility_cities = col_character()
    )
  )

# Convert dois to pmids
pmids_from_dois <-
  intovalue %>%
  filter(is.na(publication_pmid) & !is.na(publication_doi)) %>%
  select(publication_doi) %>%
  rowwise() %>%
  mutate(
    pmid_converted =
      convert_id_single_type(publication_doi, from = "doi", to = "pmid", quiet = FALSE)
  ) %>%
  ungroup() %>%
  filter(!is.na(pmid_converted)) %>%
  distinct()

write_csv(pmids_from_dois, "data/2_dataset_cleaning/intermediate_cleaning_steps/pmids_from_dois.csv")
pmids_from_dois <- read_csv("data/2_dataset_cleaning/intermediate_cleaning_steps/pmids_from_dois.csv")

# All pmids_from_dois should join back into intovalue
if (nrow(anti_join(pmids_from_dois, intovalue, by = "publication_doi")) != 0){
  rlang::abort("There are pmids from dois that do not match intovalue dataset dois!")
}

# Convert pmids to dois
dois_from_pmids <-
  intovalue %>%
  filter(!is.na(publication_pmid) & is.na(publication_doi)) %>%
  select(publication_pmid) %>%
  rowwise() %>%
  mutate(
    doi_converted =
      convert_id_single_type(publication_pmid, from = "pmid", to = "doi", quiet = FALSE)
  ) %>%
  ungroup() %>%
  filter(!is.na(doi_converted)) %>%
  distinct()

write_csv(dois_from_pmids, "data/2_dataset_cleaning/intermediate_cleaning_steps/dois_from_pmids.csv")
dois_from_pmids <- read_csv("data/2_dataset_cleaning/intermediate_cleaning_steps/dois_from_pmids.csv")

# All dois_from_pmids should join back into intovalue
if (nrow(anti_join(dois_from_pmids, intovalue, by = "publication_pmid")) != 0){
  rlang::abort("There are dois from pmids that do not match intovalue dataset pmids!")
}

# Add pmids back to intovalue
intovalue_pmids_dois <-
  intovalue %>% 
  left_join(pmids_from_dois, by = "publication_doi") %>%
  left_join(dois_from_pmids, by = "publication_pmid") %>%
  mutate(
    publication_pmid = coalesce(publication_pmid, pmid_converted),
    publication_doi = coalesce(publication_doi, doi_converted),
    publication_doi = tolower(publication_doi)
  ) %>%
  select(-pmid_converted, -doi_converted)

# Report on enhancements
n_missing_pmids_before <-
  intovalue %>% filter(!is.na(publication_doi) & is.na(publication_pmid)) %>% nrow()

n_missing_pmids_after <-
  intovalue_pmids_dois %>% filter(!is.na(publication_doi) & is.na(publication_pmid)) %>% nrow()

n_pmids_found <-
  n_missing_pmids_before - n_missing_pmids_after

rlang::inform(glue::glue(
  "PMIDs found for {n_pmids_found} of {n_missing_pmids_before} trials with DOIs (and no PMID).
  PMIDs still missing for {n_missing_pmids_after} trials with DOIs."
))

n_missing_dois_before <-
  intovalue %>% filter(!is.na(publication_pmid) & is.na(publication_doi)) %>% nrow()

n_missing_dois_after <-
  intovalue_pmids_dois %>% filter(!is.na(publication_pmid) & is.na(publication_doi)) %>% nrow()

n_dois_found <-
  n_missing_dois_before - n_missing_dois_after

rlang::inform(glue::glue(
  "DOIs found for {n_dois_found} of {n_missing_dois_before} trials with PMIDs (and no DOI).
  DOIs still missing for {n_missing_dois_after} trials with PMIDs."
))

write_csv(intovalue_pmids_dois, "data/2_dataset_cleaning/final_dataset/iv_main_dataset.csv")
