# Lookup table for coalescing levels between DRKS and ClinicalTrials.gov
# Currently includes `phase`, `recruitment_status`, `masking`
# Could also add already unified categorical registry variables: `center_size` (?)

library(tidyverse)

intovalue <- read_csv("data/iv_main_dataset.csv")

lookup_phase <-
  intovalue %>% 
  distinct(registry, level_registry = phase) %>% 
  mutate(name = "phase", .before = everything()) %>% 
  mutate(level_unified = case_when(
    level_registry %in% c("Early Phase 1", "I", "Phase 1") ~ "I",
    level_registry %in% c("I-II", "Phase 1/Phase 2") ~ "I-II",
    level_registry %in% c("II", "IIa", "IIb", "Phase 2") ~ "II",
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

lookup_masking <-
  intovalue %>% 
  distinct(registry, level_registry = masking) %>% 
  mutate(name = "masking", .before = everything()) %>%
  mutate(level_unified = case_when(
    level_registry %in% 
      c("Open Label", "No masking", "None (Open Label)", "Open (masking not used)") ~ 
      "Open label",
    
    level_registry %in% 
      c("Single", "Single blind", "Single Blind") ~ "Single blind",
    
    level_registry %in% 
      c("Double", "Double Blind", "Double-Blind", "Triple", "Quadruple", "Double or multiple blind") ~ 
      "Double or multiple blind",
    
    level_registry %in% c("Blinded") ~ "Blinded unspecified",
    is.na(level_registry) ~ NA_character_,
    TRUE ~ "Blinded other"
  ))

lookup_registries <-
  bind_rows(lookup_phase, lookup_recruitment_status, lookup_masking)

write_csv(lookup_registries, "data/iv_data_lookup_registries.csv")
