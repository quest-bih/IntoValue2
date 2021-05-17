library(tidyverse)
library(lubridate)


intovalue <- 
  read_csv(
    "data/IV1_IV2_combined_dataset.csv",
    col_types = cols(
      publication_pmid = col_character(),
      facility_cities = col_character()
    )
  )

intovalue_clean <-
  intovalue %>% 

  # Fix trial-specific issues
  mutate(
    
    # Correct publications listed as "no publication"
    identification_step = case_when(
      id == "DRKS00003240" ~ "Registry linked",
      id == "NCT00742495" ~ "Dissertation",
      TRUE ~ identification_step
    ),
    has_publication =
      if_else(id %in% c("DRKS00003240","NCT00742495"), TRUE, has_publication),
    
    # Correct iv2 trials with registry results incorrectly indicated as having publications
    has_publication = if_else(
      id %in% c("NCT01532453", "NCT02847260", "NCT02403986", "NCT02021942"),
      FALSE, has_publication
    ),
    identification_step = if_else(
      id %in% c("NCT01532453", "NCT02847260", "NCT02403986", "NCT02021942"),
      "No publ", identification_step
    ),
    
    # Correct iv2 trial with incorrect publication url
    publication_url = if_else(
      id == "NCT03789448", 
      "https://elifesciences.org/articles/58487", publication_url
    ),
    
    # Correct iv1 trial with registry result in cross-reg, incorrectly indicated as having publications
    # Note: DRKS00004156 and NCT00215683 are duplicated in the iv1 dataset, as the cross-registration was missed. Nico is looking into deduping.
    has_publication = if_else(id == "DRKS00004156", FALSE, has_publication),
    identification_step = if_else(id == "DRKS00004156", "No publ", identification_step),
    has_summary_results = if_else(id == "DRKS00004156", TRUE, has_summary_results),
    
    # Some drks trials with uploaded summary results miscounted as publications
    # Change to has_summary_results and !has_publication
    # Note: all iv2 except "DRKS00004744"
    has_publication = if_else(
      id %in% c("DRKS00000635", "DRKS00000156", "DRKS00006734", "DRKS00006766", "DRKS00007163", "DRKS00004744"),
      FALSE, has_publication
    ),
    identification_step = if_else(
      id %in% c("DRKS00000635", "DRKS00000156", "DRKS00006734", "DRKS00006766", "DRKS00007163", "DRKS00004744"),
      "No publ", identification_step
    ),
    has_summary_results = if_else(
      id %in% c("DRKS00000635", "DRKS00000156", "DRKS00006734", "DRKS00006766", "DRKS00007163", "DRKS00004744"),
      TRUE, has_summary_results
    ),
    
    # Change drks trial to abstract only, since noticed not full publication
    has_publication = if_else(id == "DRKS00004862", FALSE, has_publication),
    identification_step = if_else(id == "DRKS00004862", "Abstract only", identification_step),
    
    # Change drks trial from publication to abstract only and correct url
    # DRKS00003210 has an abstract-only linked in registry (no hyperlink)
    # Unclear whether `identification_step` should be "Abstract only" or "Registry link"
    # However, since "Abstract only" counted as !had_publication, probably this
    has_publication = if_else(id == "DRKS00003210", FALSE, has_publication),
    identification_step = if_else(id == "DRKS00003210", "Abstract only", identification_step),
    publication_url = if_else(
      id == "DRKS00003210", 
      "https://www.asu-arbeitsmedizin.com/sites/default/files/2019-08/asu-2010-06-265-404_nde4mdu5.pdf", 
      publication_url
    ),
    
    # Correct trials with publications miscoded as abstracts
    has_publication = if_else(id == "NCT01168791", TRUE, has_publication),
    identification_step = if_else(id == "NCT01168791", "Registry linked", identification_step),
    publication_date = if_else(id == "NCT01168791", as_date("2016-11-10"), publication_date),
    
    has_publication = if_else(id == "NCT00806663", TRUE, has_publication),
    identification_step = if_else(id == "NCT00806663", "Hand search", identification_step),
    publication_date = if_else(id == "NCT00806663", as_date("2014-05-07"), publication_date),
    
    # Remove registry links from publications URL
    publication_url = if_else(
      str_detect(publication_url, "clinicaltrials.gov|drks.de"),
      NA_character_, publication_url
    ),
    
    # Change trial marked as having abstract only but no publication ids
    identification_step = 
      if_else(id == "NCT00854802", "No publ", identification_step),
    
    # Change trials marked as having publications but no publication ids
    has_publication = if_else(
      id %in% c("NCT00854802", "NCT00977132", "NCT02118896"), 
      FALSE, has_publication
    ),
    identification_step = if_else(
      id %in% c("NCT00854802", "NCT00977132", "NCT02118896"), 
      "No publ", identification_step
    ),
    
    # Remove publication date if no publication
    publication_date = if_else(!has_publication, as_date(NA), publication_date),
    
    # Add dates to trials with publications but no dates
    publication_date = case_when(
      id == "NCT00742495" ~ as_date("2016-07-25"),
      id == "NCT01448785" ~ as_date("2016-09-16"),
      id == "NCT01713426" ~ as_date("2015-11-19"),
      id == "DRKS00003240" ~ as_date("2015-03-13"),
      id == "DRKS00000486" ~ as_date("2013-08-01"), # no day available
      TRUE ~ publication_date
    ),
    
    # Fix iv1 url
    publication_url = if_else(id == "DRKS00000486", "https://elibrary.klett-cotta.de/article/99.120110/aep-8-3-175", publication_url),
    
    # Fix iv2 data entry error
    publication_url = 
      if_else(id == "NCT02517775", "https://www.mdpi.com/2072-6643/9/3/268", publication_url),
    publication_doi = 
      if_else(id == "NCT02517775", "10.3390/nu9030268", publication_doi)
  ) %>%
  
  # Clean publication ids
  mutate(
    # Trim whitespace
    publication_pmid = str_trim(publication_pmid),
    publication_doi = str_trim(publication_doi),
    publication_url = str_trim(publication_url),
    
    # Remove trailing periods from dois
    publication_doi = str_remove(publication_doi, "\\.$"),
    
    # Change incorrect "jourl.pone" to correct "journal.pone" in dois
    publication_doi = str_replace(publication_doi, "jourl.pone", "journal.pone"),
    
    # Recode "none" as NA
    publication_pmid = na_if(publication_pmid, "none"),
    
    # Change incorrect publication url ("0") to NA
    publication_url = na_if(publication_url, "0"),
    
    # Fix pmid typo
    publication_pmid = 
      if_else(publication_pmid == "2140065", "21400652", publication_pmid),
    
    # Split combined pmid/doi and move doi portion to doi column
    publication_doi = 
      if_else(str_detect(publication_pmid, ".+10\\.\\d{4,9}/[-.;()/:\\w\\d]+$"),
              str_extract(publication_pmid, "10\\.\\d{4,9}/[-.;()/:\\w\\d]+$"),
              publication_doi, missing = publication_doi),
    publication_pmid = 
      if_else(str_detect(publication_pmid, 
                         ".+10\\.\\d{4,9}/[-.;()/:\\w\\d]+$"),
              str_remove(publication_pmid, "10\\.\\d{4,9}/[-.;()/:\\w\\d]+$"),
              publication_pmid),
    
    # Move pmcid from doi to pmid, to be replaced with pmid
    publication_pmid = 
      if_else(str_detect(publication_doi, 
                         "^https://www.ncbi.nlm.nih.gov/pmc/articles/"),
              str_extract(publication_doi, "PMC[0-9]{7}"),
              publication_pmid, missing = publication_pmid),
    publication_doi = 
      if_else(str_detect(publication_doi, 
                         "^https://www.ncbi.nlm.nih.gov/pmc/articles/"),
              NA_character_,
              publication_doi, missing = publication_doi),
    
    # Replace pmcid's with pmid's
    # Coverted using https://www.ncbi.nlm.nih.gov/pmc/pmctopmid/
    publication_pmid = case_when(
      publication_pmid == "PMC3436225" ~ "22952334",
      publication_pmid == "PMC3724560" ~ "23901266",
      publication_pmid == "PMC4746013" ~ "26866015",
      publication_pmid == "PMC5442103" ~ "28536463",
      publication_pmid == "PMC3950855" ~ "24434430",
      publication_pmid == "PMC4627210" ~ "26479485",
      publication_pmid == "PMC3581819" ~ "23447455",
      publication_pmid == "PMC4353868" ~ "25780869",
      publication_pmid == "PMC5510076" ~ "28213957",
      publication_pmid == "PMC6977734" ~ "31971950",
      TRUE ~ publication_pmid
    ),
    
    # Move pmid from doi (either as pmid url, or pmid only) and remove incorrect doi
    publication_pmid = 
      if_else(str_detect(publication_doi, 
                         "^https://pubmed.ncbi.nlm.nih.gov/|^[0-9]{8}$"),
              str_extract(publication_doi, "[0-9]{8}"),
              publication_pmid, missing = publication_pmid),
    publication_doi = 
      if_else(str_detect(publication_doi, 
                         "^https://pubmed.ncbi.nlm.nih.gov/|^[0-9]{8}$"),
              NA_character_,
              publication_doi, missing = publication_doi),
    
    # Remove url prefix from doi
    publication_doi = str_remove(publication_doi, "https?://(dx\\.)?doi.org/"),
    
    # Remove doi prefix from doi
    publication_doi = str_remove(publication_doi, "(?i)^doi\\s*"),
    
    # Remove leading forward slash from doi
    publication_doi = str_remove(publication_doi, "^/\\s*"),
    
    # Remove leading colon from dois
    publication_doi = str_remove(publication_doi, "^: "),
    
    # Replace miscoded NA doi
    publication_doi = na_if(publication_doi, "link broken"),
    
    # Move url from doi to url column
    publication_url = 
      if_else(str_detect(publication_doi, "^https?://"),
              publication_doi,
              publication_url, missing = publication_url),
    publication_doi = 
      if_else(str_detect(publication_doi, "^https?://"),
              NA_character_,
              publication_doi),
    
    # Split combined doi/url and move url portion to url column
    publication_url = 
      if_else(str_detect(publication_doi, "^10\\.\\d.+https?://"),
              str_extract(publication_doi, "https?://.+$"),
              publication_url, missing = publication_url),
    publication_doi = 
      if_else(str_detect(publication_doi, "^10\\.\\d.+https?://"),
              str_remove(publication_doi, "https?://.+$"),
              publication_doi),
    
    # Extract pmid from pubmed urls
    publication_pmid = if_else(str_detect(publication_url, 
                              "^https://pubmed.ncbi.nlm.nih.gov/[0-9]{8}"),
                   str_extract(publication_url, "[0-9]{8}"),
                   publication_pmid, missing = publication_pmid),
    
    # Correct doi typos
    publication_doi = case_when(
      publication_doi == "10.1002/cncr.2515"           ~ "10.1002/cncr.25156",
      publication_doi == "0.1371/journal.pone.0199776" ~ "10.1371/journal.pone.0199776",
      publication_doi == "0.1183/13993003.congress-2016.PA2216" ~ "10.1183/13993003.congress-2016.PA2216",
      publication_doi == "10.7717/peerj.6037. eCollection 2018" ~ "10.7717/peerj.6037",
      publication_doi == "0.4172/2167-0846.1000343" ~ NA_character_, # no valid doi for article
      str_detect(publication_doi, "^10.1200/JCO.2018.36.5_suppl.61") ~ "10.1200/JCO.2018.36.5_suppl.61",
      TRUE ~ publication_doi
    )
  ) %>% 
  
  # Extract doi from url and move to doi
  # Note: Currently in a few steps, plus visually inspect any discrepancies to make sure "doi" is correct if also "new_doi" exists and different
  mutate(new_doi = str_extract(publication_url, "10\\.\\d{4,9}/[-.;()/:\\w\\d]+"), .before = publication_url) %>%
  mutate(publication_doi = coalesce(publication_doi, new_doi)) %>%
  select(-new_doi, -.before) %>% 
  
  # Fix urls
  mutate(
    publication_url = case_when(
      # If url is doi only, now that doi transferred, make url
      str_detect(publication_url, "^10\\.\\d{4,9}/[-.;()/:\\w\\d]+$") ~ str_c("https://doi.org/", publication_url),
      
      # Add https to url without scheme, and one pdf
      str_detect(publication_url, "^www.") ~ str_c("https://", publication_url),
      publication_url == "file.scirp.org/pdf/OJTS20120300005_14362903.pdf" ~ str_c("https://", publication_url),
      
      TRUE ~ publication_url
    )
  ) %>% 
  
  # Convert formats
  mutate(
    publication_pmid = as.numeric(publication_pmid),
  ) %>% 
  
  # Recalculate days to publication since date may have changes
  mutate(
    days_cd_to_publication = as.numeric(publication_date - completion_date), 
    days_pcd_to_publication = as.numeric(publication_date - primary_completion_date), 
    days_reg_to_publication = as.numeric(publication_date - registration_date),
  )

write_csv(intovalue_clean, "data/iv_clean_dataset.csv")
