library(tidyverse)
library(assertr)
library(assertive)
library(pointblank)

intovalue <- 
  read_csv(
    "data/iv_enhanced_pmids_dois_dataset.csv",
    col_types = cols(
      publication_pmid = col_number(),
      facility_cities = col_character()
    )
  )

intovalue %>% 
  
  # Validate publication dois
  pointblank::col_vals_regex(
    columns = vars(publication_doi),
    regex = "^10\\.\\d{4,9}/[-.;()/:_[:alnum:]]+$",
    na_pass = TRUE
  ) %>% 
  
  # Validate publication pmids
  pointblank::col_vals_regex(
    columns = vars(publication_pmid),
    regex = "^[0-9]{8}$",
    na_pass = TRUE
  ) %>% 
  
  # Validate publication urls
  pointblank::col_vals_regex(
    columns = vars(publication_url),
    regex = "^http",
    na_pass = TRUE
  ) %>% 
  
  # Publication urls should not go to registries
  assertr::assert(
    function(publication_url) !str_detect(publication_url, "clinicaltrials.gov|drks.de")|is.na(publication_url), 
    publication_url
  ) %>% 
  
  # Check that all trials with publication (including abstract) has pmid, doi, OR url
  assertr::verify(nrow(filter(., identification_step != "No publ" & is.na(publication_url) & is.na(publication_doi) & is.na(publication_pmid))) == 0) %>% 
  
  # Check that if identification_step is no publication or abstract only, not has_publication
  pointblank::col_vals_equal(
    vars(has_publication),
    value = FALSE,
    preconditions = ~ . %>% filter(identification_step %in% c("No publ", "Abstract only"))
  ) %>% 
  
  # Check that if not has_publication, no publication date and identification_step is no publication or abstract only
  pointblank::conjointly(
    ~ col_vals_in_set(.,
                      columns = vars(identification_step),
                      set = c("No publ", "Abstract only")
    ),
    ~ col_vals_null(., vars(publication_date)),
    preconditions = ~ . %>% filter(!has_publication)
  ) %>% 
  
  # Check that if no publication, no publication ids
  pointblank::col_vals_null(
    vars(publication_url, publication_doi, publication_pmid),
    preconditions = ~ . %>% filter(identification_step == "No publ")
  ) %>% 
  
  # Check that if has_publication, has publication_date
  pointblank::col_vals_not_null(
    vars(publication_date),
    preconditions = ~ . %>% filter(has_publication)
  )