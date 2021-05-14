# Lookup table for coalescing levels between DRKS and ClinicalTrials.gov
# Currently includes `phase` and `recruitment_status`
# Could unify additional categorical registry variables: `allocation`, `masking`, `intervention_type`
# Could also add already unified categorical registry variables: `main_sponsor`, `is_multicentric`, `center_size` (?)

library(tidyverse)

intovalue <- 
  read_csv(
    "data/iv_enhanced_pmids_dois_dataset.csv",
    col_types = cols(
      publication_pmid = col_number(),
      facility_cities = col_character()
    )
  )

#TODO: @Nico Add DRKS IIa as either "I-II" or "II"
lookup_phase <-
  intovalue %>% 
  distinct(registry, level_registry = phase) %>% 
  mutate(name = "phase", .before = everything()) %>% 
  mutate(level_unified = case_when(
    level_registry %in% c("Early Phase 1", "I", "Phase 1") ~ "I",
    level_registry %in% c("I-II", "Phase 1/Phase 2") ~ "I-II",
    level_registry %in% c("II", "IIb", "Phase 2") ~ "II",
    level_registry %in% c("II-III", "Phase 2/Phase 3") ~ "II-III",
    level_registry %in% c("III", "IIIb", "Phase 3") ~ "III",
    level_registry %in% c("IV", "Phase 4") ~ "IV",
    TRUE ~ NA_character_
  ))

lookup_recruitment_status <-
  intovalue %>% 
  distinct(registry, level_registry = recruitment_status) %>% 
  mutate(name = "recruitment_status", .before = everything()) %>% 
  mutate(level_unified = if_else(
    level_registry %in% c("Completed", "Recruiting complete, follow-up complete"),
    "Completed", "Other"
  ))

lookup_registries <-
  bind_rows(lookup_phase, lookup_recruitment_status)

write_csv(lookup_registries, "data/iv_data_lookup_registries.csv")
