library(tidyverse)
library(lubridate)

#--------------------------------------------------------------------------------------------------------
# IRR 1
#--------------------------------------------------------------------------------------------------------

select_rows <- function(dataset)
{
  dataset_filtered <- dataset %>%
    rename(publ_identified = `Publication identified in which step`) %>%
    select(ID, publ_identified, publication_date)
  
  return(dataset_filtered)
}

CTgov_original <- read_delim("./manual_check/results/double_check_results/double_check_CTgov_original_results_IRR1.csv", delim = ";") %>%
  rename(publ_identified_orig = `Publication identified in which step`,
         publication_date_orig = publication_date) %>%
  select(ID, publ_identified_orig, publication_date_orig)
CTgov_DC <- read_delim("./manual_check/results/double_check_results/double_check_CTgov_results_IRR1.csv", delim = ";") %>%
  rename(publ_identified_DC = `Publication identified in which step`,
         publication_date_DC = publication_date) %>%
  select(ID, publ_identified_DC, publication_date_DC)

DRKS_original <- read_delim("./manual_check/results/double_check_results/double_check_DRKS_original_results_IRR1.csv", delim = ";") %>%
  rename(publ_identified_orig = `Publication identified in which step`,
         publication_date_orig = publication_date) %>%
  select(ID, publ_identified_orig, publication_date_orig)
DRKS_DC <- read_delim("./manual_check/results/double_check_results/double_check_DRKS_results_IRR1.csv", delim = ";") %>%
  rename(publ_identified_DC = `Publication identified in which step`,
         publication_date_DC = publication_date) %>%
  select(ID, publ_identified_DC, publication_date_DC)

trials_original <- rbind(CTgov_original, DRKS_original) %>%
  arrange(ID)
trials_DC <- rbind(CTgov_DC, DRKS_DC) %>%
  arrange(ID)

CTgov_compare <- CTgov_original %>% left_join(CTgov_DC)
trials_compare <- trials_original %>% left_join(trials_DC)

table(trials_compare$publ_identified_orig, trials_compare$publ_identified_DC, useNA = "ifany")

trial_count <- dim(trials_compare)[1]
affil_difference_count <- sum((trials_compare$publ_identified_orig == 9 & trials_compare$publ_identified_DC != 9) |
                          (trials_compare$publ_identified_orig != 9 & trials_compare$publ_identified_DC == 9))

IRR1 <- (trial_count - affil_difference_count)/trial_count
IRR1


#--------------------------------------------------------------------------------------------------------
# IRR 2
#--------------------------------------------------------------------------------------------------------

CTgov_original <- read_delim("./manual_check/results/double_check_results/double_check_CTgov_original_results_IRR2+3.csv", delim = ",") %>%
  rename(publ_identified_orig = `Publication identified in which step`,
         publication_date_orig = publication_date,
         article_yes_no_orig = article_yes_no,
         publication_DOI_orig = publication_DOI,
         publication_URL_orig = publication_URL,
         unsure_about_publ_yes_no_orig = unsure_about_publ_yes_no,
         reason_orig = reason,
         other_comments_orig = other_comments,
         rater_orig = rater) %>%
  select(ID, publ_identified_orig, publication_date_orig, article_yes_no_orig, 
         publication_DOI_orig, publication_URL_orig, unsure_about_publ_yes_no_orig,
         reason_orig, other_comments_orig, reason_orig, rater_orig)

CTgov_DC <- read_delim("./manual_check/results/double_check_results/double_check_CTgov_results_IRR2+3.csv", delim = ",") %>%
  rename(publ_identified_DC = `Publication identified in which step`,
         publication_date_DC = publication_date,
         article_yes_no_DC = article_yes_no,
         publication_DOI_DC = publication_DOI,
         publication_URL_DC = publication_URL,
         unsure_about_publ_yes_no_DC = unsure_about_publ_yes_no,
         reason_DC = reason,
         other_comments_DC = other_comments,
         rater_DC = rater) %>%
  select(ID, publ_identified_DC, publication_date_DC, article_yes_no_DC, 
         publication_DOI_DC, publication_URL_DC, unsure_about_publ_yes_no_DC,
         reason_DC, other_comments_DC, reason_DC, rater_DC)

DRKS_original <- read_delim("./manual_check/results/double_check_results/double_check_DRKS_original_results_IRR2+3.csv", delim = ",") %>%
  rename(publ_identified_orig = `Publication identified in which step`,
         publication_date_orig = publication_date,
         article_yes_no_orig = article_yes_no,
         publication_DOI_orig = publication_DOI,
         publication_URL_orig = publication_URL,
         unsure_about_publ_yes_no_orig = unsure_about_publ_yes_no,
         reason_orig = reason,
         other_comments_orig = other_comments,
         rater_orig = rater) %>%
  select(ID, publ_identified_orig, publication_date_orig, article_yes_no_orig, 
         publication_DOI_orig, publication_URL_orig, unsure_about_publ_yes_no_orig,
         reason_orig, other_comments_orig, reason_orig, rater_orig)

DRKS_DC <- read_delim("./manual_check/results/double_check_results/double_check_DRKS_results_IRR2+3.csv", delim = ",") %>%
  rename(publ_identified_DC = `Publication identified in which step`,
         publication_date_DC = publication_date,
         article_yes_no_DC = article_yes_no,
         publication_DOI_DC = publication_DOI,
         publication_URL_DC = publication_URL,
         unsure_about_publ_yes_no_DC = unsure_about_publ_yes_no,
         reason_DC = reason,
         other_comments_DC = other_comments,
         rater_DC = rater) %>%
  select(ID, publ_identified_DC, publication_date_DC, article_yes_no_DC, 
         publication_DOI_DC, publication_URL_DC, unsure_about_publ_yes_no_DC,
         reason_DC, other_comments_DC, reason_DC, rater_DC)

trials_original <- rbind(CTgov_original, DRKS_original) %>%
  arrange(ID)
trials_DC <- rbind(CTgov_DC, DRKS_DC) %>%
  arrange(ID)

CTgov_compare <- CTgov_original %>% left_join(CTgov_DC, by = "ID") %>%
  filter(publ_identified_orig != 9)
trials_compare <- trials_original %>% left_join(trials_DC, by = "ID") %>%
  filter(publ_identified_orig != 9)

table(trials_compare$publ_identified_orig, trials_compare$publ_identified_DC, useNA = "ifany")



#add summary results data

#the AACT dataset has to be downloaded first from https://aact.ctti-clinicaltrials.org/pipe_files
AACT_folder <- "data/AACT_20200603/" #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "calculated_values")

AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".txt")
AACT_datasets <- AACT_dataset_files %>%
  map(read_delim, delim = "|", guess_max = 50000)
names(AACT_datasets) <- AACT_dataset_names

#add results reported column to our dataset
AACT_summary_results <- AACT_datasets$calculated_values %>% 
  select(nct_id, were_results_reported) %>%
  rename(ID = nct_id)

AACT_summary_results_date <- AACT_datasets$studies %>% 
  select(nct_id, results_first_submitted_date) %>%
  mutate(results_first_submitted_date = ymd(results_first_submitted_date)) %>%
  rename(ID = nct_id)

trials_compare <- trials_compare %>% 
  left_join(AACT_summary_results, by = "ID") %>%
  left_join(AACT_summary_results_date, by = "ID")
trials_compare$were_results_reported <- replace_na(trials_compare$were_results_reported, FALSE)

#overwrite manual publication detection status for cases with summary results 
#as they will always be found independent of manual search and will go into the 
#final result of the publication search like this
trials_compare$publ_identified_orig[trials_compare$were_results_reported] <- 4
trials_compare$publ_identified_DC[trials_compare$were_results_reported] <- 4


table(trials_compare$publ_identified_orig, trials_compare$publ_identified_DC, useNA = "ifany")


trial_count <- dim(trials_compare)[1]
publ_difference_idx <- (trials_compare$publ_identified_orig %in% c(0,8) & !(trials_compare$publ_identified_DC %in% c(0,8))) |
  (!(trials_compare$publ_identified_orig %in% c(0,8)) & trials_compare$publ_identified_DC%in% c(0,8))
publ_difference_count <- sum(publ_difference_idx)

IRR2 <- (trial_count - publ_difference_count)/trial_count
IRR2


#trials with differences
trials_diff <- trials_compare[publ_difference_idx,]

write_csv(trials_diff, "./manual_check/results/double_check_results/DC_trials_with_discrepancy.csv")

#--------------------------------------------------------------------------------------------------------
# IRR 3
#--------------------------------------------------------------------------------------------------------

smaller_date <- function(date_1, date_2) {
  date_return <- NA
  if(is.na(date_1) & !is.na(date_2)) {
    date_return <- date_2
  } else if(!is.na(date_1) & is.na(date_2)) {
    date_return <- date_1
  } else if(!is.na(date_1) & !is.na(date_2)) {
    if(date_1 < date_2) {
      date_return <- date_1
    } else {
      date_return <- date_2
    }
  }
  return(date_return)
}

#filter out any publication with no identified result for at least one rater
trials_compare_with_results <- trials_compare %>% 
  mutate(publication_date_orig = dmy(publication_date_orig),
         publication_date_DC = dmy(publication_date_DC)) %>%
  filter(publ_identified_orig != 0 & publ_identified_orig != 8) %>% 
  filter(publ_identified_DC != 0 & publ_identified_DC != 8)

#correct publication date to smallest of manual check date or summary results date
smaller_summary_date_orig <- ((trials_compare_with_results$results_first_submitted_date < 
                        trials_compare_with_results$publication_date_orig) |
                          !is.na(trials_compare_with_results$results_first_submitted_date) & 
                          is.na(trials_compare_with_results$publication_date_orig)) %>% 
                        replace_na(FALSE)
trials_compare_with_results$publication_date_orig[smaller_summary_date_orig] <- 
  trials_compare_with_results$results_first_submitted_date[smaller_summary_date_orig]

smaller_summary_date_DC <- ((trials_compare_with_results$results_first_submitted_date < 
                            trials_compare_with_results$publication_date_DC) |
                              !is.na(trials_compare_with_results$results_first_submitted_date) & 
                              is.na(trials_compare_with_results$publication_date_DC)) %>% 
                            replace_na(FALSE)
trials_compare_with_results$publication_date_DC[smaller_summary_date_DC] <- 
  trials_compare_with_results$results_first_submitted_date[smaller_summary_date_DC]


#now compare the publication dates
trials_compare_with_results <- trials_compare_with_results %>%
  mutate(publ_data_within_1_year = (publication_date_orig - publication_date_DC) <= 365)


trial_count <- dim(trials_compare_with_results)[1]
matching_dates_count <- sum(trials_compare_with_results$publ_data_within_1_year, na.rm = TRUE)

IRR3 <- matching_dates_count/trial_count
IRR3



IRR1
IRR2
IRR3