# @Nico: In general, I added as much detail as I could about source of registry variables, but some detail is still missing to make the dataset reproducible, so please add in any missing details on which fields from ctgov/aact and drks (also csv vs. webscraping) each variable comes from

library(tidyverse)

intovalue <- 
  read_csv(
    "data/iv_enhanced_pmids_dois_dataset.csv",
    col_types = cols(
      publication_pmid = col_number(),
      facility_cities = col_character()
    )
  )


# Get levels of variables as a string (colon-separated)
# Optionally, limit by registry
get_levels <- function(var, registry = NULL){
  
  # Limit to registry, if supplied
  if (!is_null(registry)) {
    intovalue <-
      intovalue %>% 
      filter(.data$registry == .env$registry)
  }
  
  intovalue %>%
    filter(!is.na({{var}})) %>%
    distinct({{var}}) %>%
    arrange({{var}}) %>%
    pull() %>%
    str_c(collapse = "; ")
}

# Get city levels
lead_cities_levels <-
  intovalue %>%
  distinct(lead_cities) %>%
  mutate(lead_cities = str_split(lead_cities, " ")) %>% 
  unnest_longer(lead_cities) %>% 
  distinct(lead_cities) %>%
  filter(!is.na(lead_cities)) %>% 
  arrange(lead_cities) %>% 
  pull() %>%
  str_c(collapse = "; ")

facility_cities_levels <-
  intovalue %>%
  distinct(facility_cities) %>%
  mutate(facility_cities = str_split(facility_cities, " ")) %>% 
  unnest_longer(facility_cities) %>% 
  distinct(facility_cities) %>%
  filter(!is.na(facility_cities)) %>% 
  arrange(facility_cities) %>% 
  pull() %>%
  str_c(collapse = "; ")

# Create codebook with levels
codebook <-
  tibble(
    name = colnames(intovalue),
    type = tolower(purrr::map_chr(intovalue, class))
  ) %>% 
  mutate(levels = case_when(
    name == "registry" ~ get_levels(registry),
    name == "identification_step" ~ get_levels(identification_step),
    name == "recruitment_status" ~ get_levels(recruitment_status),
    name == "phase" ~ get_levels(phase),
    name == "main_sponsor" ~ get_levels(main_sponsor),
    name == "allocation" ~ get_levels(allocation),
    name == "masking" ~ get_levels(masking),
    name == "intervention_type" ~ get_levels(intervention_type),
    name == "center_size" ~ get_levels(center_size),
    name == "iv_version" ~ get_levels(iv_version),
    name == "lead_cities" ~ lead_cities_levels,
    name == "facility_cities" ~ facility_cities_levels
  )) %>% 
  mutate(levels_ctgov = case_when(
    name == "registry" ~ "ClinicalTrials.gov",
    name == "recruitment_status" ~ get_levels(recruitment_status, "ClinicalTrials.gov"),
    name == "phase" ~ get_levels(phase, "ClinicalTrials.gov"),
    name == "main_sponsor" ~ get_levels(main_sponsor, "ClinicalTrials.gov"),
    name == "allocation" ~ get_levels(allocation, "ClinicalTrials.gov"),
    name == "masking" ~ get_levels(masking, "ClinicalTrials.gov"),
    name == "intervention_type" ~ get_levels(intervention_type, "ClinicalTrials.gov")
  )) %>% 
  mutate(levels_drks = case_when(
    name == "registry" ~ "DRKS",
    name == "recruitment_status" ~ get_levels(recruitment_status, "DRKS"),
    name == "phase" ~ get_levels(phase, "DRKS"),
    name == "main_sponsor" ~ get_levels(main_sponsor, "DRKS"),
    name == "allocation" ~ get_levels(allocation, "DRKS"),
    name == "masking" ~ get_levels(masking, "DRKS"),
    name == "intervention_type" ~ get_levels(intervention_type, "DRKS")
  ))

# Prepare variable descriptions
description <- tribble(
  ~name, ~description,
  
  "id", 
  "Trial registration number, either a ClinicalTrials.gov NCT id or DRKS id",
  
  "registry", 
  "Trial registry, either ClinicalTrials.gov or DRKS",
  
  "lead_cities", 
  "City or names of lead German university medical centers based on `affiliation` in registries. Derived from `sponsors`, `official`, and `responsible parties` from ClinicalTrials.gov, and any `addresses` from DRKS. Multiple UMCs (e.g., if study PI and sponsor organization have different affiliations) are separated by a single whitespace.", #@NICO: is this correct? or in IV1 some are lead? please edit to correct. i can't tell column names but i think it's in this: https://github.com/quest-bih/IntoValue2/blob/master/code/Create_DRKS_sample.R#L50
  
  
  "has_publication", 
  "Whether a publication was found. Abstracts only are NOT counted as publications, whereas dissertations are.",
  
  "publication_doi", 
  "Publication DOI. Manually entered during publication search or derived from PMID via FatCat.",
  
  "publication_pmid", 
  "Publication PMID. Manually entered during publication search or derived from DOI via FatCat.",
  
  "publication_url", 
  "Publication URL. Manually entered during publication search.",
  
  "publication_date", 
  "Publication date. Manually entered during publication search. Earliest date used, whether ePub or print pub date.",
  
  "identification_step", 
  "Manual publication search identification steps. Both versions include: No publ; Registry linked. Version 1 also includes: Abstract only; Dissertation; Hand search; Pubmed. Version 2 also includes: Publ found in Google ID search; Publ found in Google search (no ID). In version 2, abstracts only were not included, whereas dissertations were included.", #@NICO: any further clarification?
  
  "is_prospective", 
  "Whether trial was prospectively registered. Derived from `registration_date` and `start_date`. Trial is considered prospectively registered if registered in the same or previous months to start date.",
  
  "has_summary_results", 
  "Whether summary results found on registry during manual publication search. ClinicalTrials.gov includes a structured summary results field. DRKS includes summary results with other references, and summary results were determined based on manual inspection with names such as Ergebnisbericht or Abschlussbericht", #@NICO: any further clarification? is this correct that it's from manual search or was automated for ctgov?
  
  "summary_results_date", 
  "Date of summary results submission to registry. ClinicalTrials.gov only. Manually entered during publication search.", #@NICO: summary results SUBMISSION or POSTED date? is this correct that it's from manual search or was automated for ctgov?
  
  "registration_date", 
  "Date of study submission to registry, as given on registry.",
  
  "start_date", 
  "Date of the study start, as given on registry. ClinicalTrials.gov previously allowed start dates without day, in which case date is defaulted to first of the month.",
  
  "completion_date", 
  "Date of the study completion, as given on registry. ClinicalTrials.gov previously allowed completion dates without day, in which case date is defaulted to first of the month. Indicated as study end date on DRKS.",
  
  "completion_year", 
  "Year of the study completion. Derived from `completion_date`.",
  
  "primary_completion_date", 
  "Date of the study primary completion, as given on registry. ClinicalTrials.gov only [TODO]. ClinicalTrials.gov previously allowed primary completion dates without day, in which case date is defaulted to first of the month. In version 1, primary completion dates for DRKS are copied from completion dates.", #@NICO: DRKS doesn't have PCD right? i think we should remove.
  
  "primary_completion_year", 
  "Year of the study primary completion. Derived from `primary_completion_date`. ClinicalTrials.gov only [TODO].", #@NICO: as above
  
  "days_cd_to_publication", 
  "Number of days from `completion_date` to `publication_date`. Derived.",
  
  "days_pcd_to_publication", 
  "Number of days from `primary_completion_date` to `publication_date`. Derived. ClinicalTrials.gov only.", #@NICO currently has drks but if we remove pcd from drks then ctgov only
  
  "days_cd_to_summary", 
  "Number of days from `completion_date` to `summary_results_date`. Derived. ClinicalTrials.gov only.",
  
  "days_pcd_to_summary", 
  "Number of days from `primary_completion_date` to `summary_results_date`. Derived. ClinicalTrials.gov only.",
  
  "days_reg_to_start", 
  "Number of days from `registration_date` to `start_date`. Derived.",
  
  "days_reg_to_cd", 
  "Number of days from `registration_date` to `completion_date`. Derived.",
  
  "days_reg_to_pcd", 
  "Number of days from `registration_date` to `primary_completion_date`. ClinicalTrials.gov only.", #@NICO currently has drks but if we remove pcd from drks then ctgov only
  
  "days_reg_to_publication", 
  "Number of days from `registration_date` to `publication_date`. Derived.",
  
  "recruitment_status", 
  "Recruitment status, as given on registry. Different levels for ClinicalTrials.gov and DRKS.",
  
  "phase", 
  "Trial phase, as given on registry. Different levels for ClinicalTrials.gov and DRKS.",
  
  "enrollment", 
  "Number of trial participants, as given on registry. May be anticipated or actual number.",
  
  "is_multicentric", 
  "Whether multiple study centers are involved, as given on registry. Derived based on registrsties.", #@NICO: any more info on how derived?
  
  "main_sponsor", 
  "Whether main sponsor is industry or other, as given on registry. For ClinicalTrials.gov, other includes NIH.", #@NICO: any more info on how
  
  "allocation", 
  "Trial allocation and randomization, as given on registry. Different levels for ClinicalTrials.gov and DRKS.",
  
  "masking", 
  "Trial masking, as given on registry. Different levels for ClinicalTrials.gov and DRKS.", #@NICO: not sure which fields from ctgov and drks
  
  "intervention_type", 
  "Trial intervention, as given on registry. Different levels for ClinicalTrials.gov and DRKS.", #@NICO: not sure which fields from ctgov and drks. also, i see multiple intervention_types in ctgov and drks, how do you select 1 for iv?
  
  "center_size", 
  "Study center size classified as 'large' or 'small'. If no German UMC lead, 'No lead center'. Derived based on registrsties. Threshold defined as median of the total number of registered trials per institution.", #@NICO: I don't understand how this is derived. could you clarify?
  
  "has_german_umc_lead", 
  "Whether German UMC in trial `lead_cities` (sponsor, PI, or responsible party) or only in `facility_cities`. Version 2 includes only trials with German UMC lead.",
  
  "facility_cities", 
  "City or names of facility German university medical centers based on `affiliation` in registries. Derived from `sponsors`, `official`, and `responsible parties` from ClinicalTrials.gov, and any `addresses` from DRKS. Multiple UMCs (e.g., if study PI and sponsor organization have different affiliations) are separated by a single whitespace. Only used in version 1; in version 2, all NA.", #@NICO: what is the source for facility cities? aka how differentiated from lead cities? please change the "derived" sentences to reflect this.
  
  "iv_version", 
  "IntoValue version, either 1 or 2.",
  
  "is_dupe", 
  "Whether trial is duplicated in version 1 and 2. If so, both are marked as duplicates and can be filtered out in combination with `iv_version`."
)

# Check that all described variables are in the dataset and vice-versa
if (!all(codebook$name %in% description$name)| !all(description$name %in% codebook$name)){
  rlang::warn("There are missing or additional variables for codebook!")
}

codebook <-
  codebook %>% 
  left_join(description, by = "name") %>% 
  relocate(description, .after = "type")

write_csv(codebook, "data/iv_data_dictionary.csv")
