library(tidyverse)

intovalue <- read_csv("data/iv_main_dataset.csv")

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
    name == "publication_type" ~ get_levels(publication_type),
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
  "City or names of lead German university medical centers based on `affiliation` in registries. Derived from `sponsors`, `overall officials`, and `responsible parties` from ClinicalTrials.gov, and any `addresses` from DRKS. Multiple UMCs (e.g., if study PI and sponsor organization have different affiliations) are separated by a single whitespace.",
  
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
  "Manual publication search identification steps. Both versions include: No publ; Registry linked. Version 1 also includes: Pubmed; Hand search (= Publication found via Google search); Abstract only; Dissertation. Version 2 also includes: Publ found in Google ID search; Publ found in Google search (no ID). In version 2, abstracts only were not included, whereas dissertations were included.",
  
  "publication_type", 
  "Type of identified publication. Can be journal publication or dissertation.",
  
  "is_prospective", 
  "Whether trial was prospectively registered. Derived from `registration_date` and `start_date`. Trial is considered prospectively registered if registered in the same or previous months to start date.",
  
  "has_summary_results", 
  "Whether summary results were posted on registry. ClinicalTrials.gov includes a structured summary results field. DRKS includes summary results with other references, and summary results were determined based on manual inspection with names such as Ergebnisbericht or Abschlussbericht.",
  
  "summary_results_date", 
  "Date of summary results. For ClinicalTrials.gov only, date of submission to registry, derived from `results_first_submitted_date` (field previously called `first_received_results_date`). For DRKS, date included on posted summary result document.",
  
  "registration_date", 
  "Date of study submission to registry, as given on registry.",
  
  "start_date", 
  "Date of the study start, as given on registry. ClinicalTrials.gov previously allowed start dates without day, in which case date is defaulted to first of the month.",
  
  "completion_date", 
  "Date of the study completion, as given on registry. ClinicalTrials.gov previously allowed completion dates without day, in which case date is defaulted to first of the month. Indicated as `study end date` on DRKS.",
  
  "completion_year", 
  "Year of the study completion. Derived from `completion_date`.",
  
  "primary_completion_date", 
  "Date of the study primary completion, as given on registry. ClinicalTrials.gov only. ClinicalTrials.gov previously allowed primary completion dates without day, in which case date is defaulted to first of the month.",
  
  "primary_completion_year", 
  "Year of the study primary completion. Derived from `primary_completion_date`. ClinicalTrials.gov only.",
  
  "days_cd_to_publication", 
  "Number of days from `completion_date` to `publication_date`. Derived.",
  
  "days_pcd_to_publication", 
  "Number of days from `primary_completion_date` to `publication_date`. Derived. ClinicalTrials.gov only.",
  
  "days_cd_to_summary", 
  "Number of days from `completion_date` to `summary_results_date`. Derived. ClinicalTrials.gov only.",
  
  "days_pcd_to_summary", 
  "Number of days from `primary_completion_date` to `summary_results_date`. Derived. ClinicalTrials.gov only.",
  
  "days_reg_to_start", 
  "Number of days from `registration_date` to `start_date`. Derived.",
  
  "days_reg_to_cd", 
  "Number of days from `registration_date` to `completion_date`. Derived.",
  
  "days_reg_to_pcd", 
  "Number of days from `registration_date` to `primary_completion_date`. ClinicalTrials.gov only.",
  
  "days_reg_to_publication", 
  "Number of days from `registration_date` to `publication_date`. Derived.",
  
  "recruitment_status", 
  "Recruitment status, as given on registry. Different levels for ClinicalTrials.gov and DRKS.",
  
  "phase", 
  "Trial phase, as given on registry. Different levels for ClinicalTrials.gov and DRKS.",
  
  "enrollment", 
  "Number of trial participants, as given on registry. May be anticipated or actual number.",
  
  "is_multicentric", 
  "Whether multiple study centers are involved, as given on registry. Derived from `has_single_facility` (CT.gov) and `monoMultiCentric` (DRKS).",
  
  "main_sponsor", 
  "Whether main sponsor is industry or other, as given on registry. For ClinicalTrials.gov, sourced from `agency_class` in `sponsors` datatable , and `Other` includes NIH. For DRKS, sourced from `investigator initiated`.",
  
  "allocation", 
  "Trial allocation and randomization, as given on registry. Different levels for ClinicalTrials.gov and DRKS.",
  
  "is_randomized", 
  "Whether trial was randomized. Based on the 'allocation' variable. 'Randomized' or 'Randomized controlled trial' was counted as TRUE, while 'Non-Randomized, 'Non-randomized controlled trial', 'Other', and 'Single arm study' was counted as FALSE",
  
  "masking", 
  "Trial masking, as given on registry. Different levels for ClinicalTrials.gov (Version 1: No masking, Open Label, Single Blind, Double Blind, Participant, Care Provider, Investigator, Outcomes Assessor; Version 2: None (Open Label), Single, Double, Triple, Quadruple) and DRKS (Open (masking not used), Single blind, Blinded, Double or multiple blind).",
  
  "intervention_type", 
  "Trial intervention, as given on registry. ClinicalTrials.gov only.", 
  
  "center_size", 
  "UMC classified as 'large' or 'small' depending on the number of trials led per `lead_cities`. A UMC was classified as 'large' if it conducted more trials than the median trial number per UMC across all UMCs included in the IntoValue1 or Intovalue2 studies, respectively. If trial has no German UMC lead, 'No lead center'.",
  
  "has_german_umc_lead", 
  "Whether German UMC in trial `lead_cities` (sponsor, PI, or responsible party) or only in `facility_cities`. Version 2 includes only trials with German UMC lead.",
  
  "facility_cities", 
  "City or names of facility German university medical centers based on `affiliation` in registries. Derived from 'facilities' from ClinicalTrials.gov, and 'recruitmentLocation' from DRKS. Multiple UMCs (i.e., if one trial lists multiple facilities) are separated by a single whitespace. Only used in version 1; in version 2, all NA.",

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
