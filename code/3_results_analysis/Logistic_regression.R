#----------------------------------------------------------------------------------------------------------------------
#
# The following script performs the logistic regression model described in the supplement.
# The model is built stepwise, starting with univariate regression models and adding the variable
# with the highest increase in log-likelihood in each step.
# The area under the curve is calculated for the final model. Additionally the logistic regression model
# is used on the journal article publications only (not considering summary result postings).
#
#----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lmtest)
library(pROC)
library(lubridate)


#----------------------------------------------------------------------------------------------------------------------
#  data cleaning - determine center size (large/small) & unify trial phases
#----------------------------------------------------------------------------------------------------------------------

#read in IntoValue dataset
IntoValue_studies <- read_csv("data/2_dataset_cleaning/final_dataset/iv_main_dataset.csv", na = "NA") %>%
  filter(iv_version == 2)

  
#read in registry lookup table to unify ctgov and drks levels
lookup_registries <- read_csv("data/2_dataset_cleaning/final_dataset/iv_data_lookup_registries.csv")

lookup_phase <- 
  lookup_registries %>% 
  filter(name == "phase") %>% 
  select(-name)

lookup_recruitment_status <- 
  lookup_registries %>% 
  filter(name == "recruitment_status") %>% 
  select(-name)

IntoValue_studies <-
  IntoValue_studies %>% 
  left_join(lookup_phase, by = c("registry", "phase" = "level_registry")) %>% 
  select(-phase, phase = level_unified) %>% 
  left_join(lookup_recruitment_status, by = c("registry", "recruitment_status" = "level_registry")) %>% 
  select(-recruitment_status, recruitment_status = level_unified)


IntoValue_studies <- IntoValue_studies %>%
  #filter(!is.na(enrollment)) %>% #filter out 3 cases where no participant number is given in order to produce the same model size for all cases
  mutate(days_to_publ_summary = pmin(days_cd_to_publication,   #get minimum of days to pub or to summary result
                                     days_cd_to_summary, na.rm = TRUE)) %>%
  mutate(timely_publication = days_to_publ_summary < 2*365)

IntoValue_studies <- IntoValue_studies %>%
  mutate(timely_publication = timely_publication %>% replace_na(FALSE)) %>%
  mutate(medication = intervention_type == "Drug") %>%
  mutate(phase = phase %>% replace_na("Not given"))


#define center size = small/large
#threshold defined as median of the total number of registered trials per institution
IntoValue_app_table <- readRDS("code/4_shiny_app/data/IntoValue_Dataset_combined.rds") %>%
  filter(city != "All trials combined")
cities_trial_num <- IntoValue_app_table$city %>%
  str_replace("Berlin", "Charite") %>%
  str_replace("TU München", "TU") %>%
  str_replace("LMU München", "LMU") %>% 
  table()
median_trial_num <- cities_trial_num %>% median()
large_centers <- which(cities_trial_num > median_trial_num) %>% names()
#center = large/small only for the trials with German leading institutions. Those without make up a third category
IntoValue_studies <- IntoValue_studies %>%
  mutate(center_size = lead_cities %>% map_chr(function(x) 
    ifelse(x %>% str_detect(large_centers) %>% any(), "large", "small")))


#----------------------------------------------------------------------------------------------------------------------
#  fit logistic regression models
#----------------------------------------------------------------------------------------------------------------------

#filter out 3 cases where no participant number is given in order to produce the same model size for all cases
#otherwise the models cannot be compared directly
IntoValue_studies <- IntoValue_studies %>%
  filter(!is.na(enrollment))
  
#first step: compare models to constant model
models_1 <- list(m0 = glm(timely_publication ~ 1, family = binomial(),data=IntoValue_studies),
                 m1 = glm(timely_publication ~ medication, family = binomial(),data=IntoValue_studies),
                 m2 = glm(timely_publication ~ center_size, family = binomial(),data=IntoValue_studies),
                 m3 = glm(timely_publication ~ is_multicentric, family = binomial(),data=IntoValue_studies),
                 m4 = glm(timely_publication ~ phase, family = binomial(),data=IntoValue_studies),
                 m5 = glm(timely_publication ~ ifelse(main_sponsor == "Industry", 1, 0), family = binomial(),data=IntoValue_studies),
                 m6 = glm(timely_publication ~ enrollment, family = binomial(),data=IntoValue_studies),
                 m7 = glm(timely_publication ~ as.factor(completion_year), family = binomial(),data=IntoValue_studies),
                 m8 = glm(timely_publication ~ recruitment_status, family = binomial(),data=IntoValue_studies))

comparison_1 <- map2(models_1[1], models_1[2:9], lrtest)
log_lik <- comparison_1 %>% map_dbl(function(x) x$LogLik[2])
chisq <- comparison_1 %>% map_dbl(function(x) x$Chisq[2])
p_val <- comparison_1 %>% map_dbl(function(x) x$`Pr(>Chisq)`[2])
aic <- models_1[2:9]  %>% map_dbl(function(x) x$aic)
#multiple comparison correction of p_values with Holm-Bonferroni
p_val <- p.adjust(p_val, method = "bonferroni")
comparison_1_summary <- as_tibble(cbind(log_lik, chisq, p_val, aic))


models_1_summary <- map(models_1, summary)
names(models_1_summary) <- names(models_1)


#calculate the odds ratios + confidence intervals for each variable value
calc_odds_ratio <- function(model)
{
  model_sum <- summary(model)
  para_num <- dim(model_sum$coefficients)[1]
  para <- model_sum$coefficients[2:para_num,1]
  para_sd <- model_sum$coefficients[2:para_num,2]

  odds_ratio_CI <- cbind(exp(para),
                         exp(para - 1.96 * para_sd),
                         exp(para + 1.96 * para_sd))
  colnames(odds_ratio_CI) <- c("odds_ratio", "odds_CI_left", "odds_CI_right")
  rownames(odds_ratio_CI) <- rownames(model_sum$coefficients)[2:para_num]

  return(odds_ratio_CI)
}

odds_ratios <- do.call(rbind, map(models_1[2:9], calc_odds_ratio))

#model with the highest likelihood: is_multicentric, closely followed by enrollment - both variables are correlated
#as multicenter trails (median enrollment: 137) have more participants as monocenter trials (median enrollment: 50)
min_model <- which.min(comparison_1_summary$aic)



#second step, adding the next explanatory variable
models_2 = list(m0 = glm(timely_publication ~ recruitment_status, family = binomial(),data=IntoValue_studies),
                m1 = glm(timely_publication ~ recruitment_status + medication, family = binomial(),data=IntoValue_studies),
                m2 = glm(timely_publication ~ recruitment_status + center_size, family = binomial(),data=IntoValue_studies),
                m3 = glm(timely_publication ~ recruitment_status + is_multicentric, family = binomial(),data=IntoValue_studies),
                m4 = glm(timely_publication ~ recruitment_status + phase, family = binomial(),data=IntoValue_studies),
                m5 = glm(timely_publication ~ recruitment_status + main_sponsor, family = binomial(),data=IntoValue_studies),
                m6 = glm(timely_publication ~ recruitment_status + enrollment, family = binomial(),data=IntoValue_studies),
                m7 = glm(timely_publication ~ recruitment_status + as.factor(completion_year), family = binomial(),data=IntoValue_studies))

comparison_2 <- map2(models_2[1], models_2[2:8], lrtest)
log_lik <- comparison_2 %>% map_dbl(function(x) x$LogLik[2])
chisq <- comparison_2 %>% map_dbl(function(x) x$Chisq[2])
p_val <- comparison_2 %>% map_dbl(function(x) x$`Pr(>Chisq)`[2])
aic <- models_2[2:8]  %>% map_dbl(function(x) x$aic)
#multiple comparison correction of p_values with Holm-Bonferroni
p_val <- p.adjust(p_val, method = "bonferroni")
comparison_2_summary <- as_tibble(cbind(log_lik,chisq,p_val,aic))


models_2_summary <- map(models_2, summary)
names(models_2_summary) <- names(models_2)

#model with the lowest likelihood in second step: main_sponsor (industry/academic)
min_model_2 <- which.min(comparison_2_summary$aic)




#third step, adding the next explanatory variable
models_3 = list(m0 = glm(timely_publication ~ recruitment_status + is_multicentric, family = binomial(),data=IntoValue_studies),
                m1 = glm(timely_publication ~ recruitment_status + is_multicentric + medication, family = binomial(),data=IntoValue_studies),
                m2 = glm(timely_publication ~ recruitment_status + is_multicentric + center_size, family = binomial(),data=IntoValue_studies),
                m3 = glm(timely_publication ~ recruitment_status + is_multicentric + phase, family = binomial(),data=IntoValue_studies),
                m4 = glm(timely_publication ~ recruitment_status + is_multicentric + main_sponsor, family = binomial(),data=IntoValue_studies),
                m5 = glm(timely_publication ~ recruitment_status + is_multicentric + enrollment, family = binomial(),data=IntoValue_studies),
                m6 = glm(timely_publication ~ recruitment_status + is_multicentric + as.factor(completion_year), family = binomial(),data=IntoValue_studies))

comparison_3 <- map2(models_3[1], models_3[2:7], lrtest)
log_lik <- comparison_3 %>% map_dbl(function(x) x$LogLik[2])
chisq <- comparison_3 %>% map_dbl(function(x) x$Chisq[2])
p_val <- comparison_3 %>% map_dbl(function(x) x$`Pr(>Chisq)`[2])
aic <- models_3[2:7]  %>% map_dbl(function(x) x$aic)
#multiple comparison correction of p_values with Holm-Bonferroni
p_val <- p.adjust(p_val, method = "bonferroni")
comparison_3_summary <- as_tibble(cbind(log_lik,chisq,p_val,aic))

#model with the lowest likelihood in second step: main_sponsor (industry/academic)
min_model_3 <- which.max(comparison_3_summary$log_lik)


#calculate the area under the curve for the final model
prob <- predict(models_3$m0,type=c("response"))
roc_curve <- roc(IntoValue_studies$timely_publication, prob, ci = TRUE)


#----------------------------------------------------------------------------------------------------------------------
#  different publication rates for the categories detected in the logistic regression model
#----------------------------------------------------------------------------------------------------------------------

recruitment_status_publ_rates <- IntoValue_studies %>% 
  group_by(recruitment_status, timely_publication) %>% 
  summarise(count = n())

completed_pub_rate <- recruitment_status_publ_rates$count[2]/sum(recruitment_status_publ_rates$count[1:2])
other_pub_rate <- recruitment_status_publ_rates$count[4]/sum(recruitment_status_publ_rates$count[3:4])
completed_pub_rate
other_pub_rate


multicentric_publ_rates <- IntoValue_studies %>% 
  group_by(is_multicentric, timely_publication) %>% 
  summarise(count = n())

monocentric_pub_rate <- multicentric_publ_rates$count[2]/sum(multicentric_publ_rates$count[1:2])
multicentric_pub_rate <- multicentric_publ_rates$count[4]/sum(multicentric_publ_rates$count[3:4])
monocentric_pub_rate
multicentric_pub_rate


compl_year_publ_rates <- IntoValue_studies %>% 
  group_by(completion_year, timely_publication) %>% 
  summarise(count = n())
compl_year_publ_num <- compl_year_publ_rates$count[seq(2, length(compl_year_publ_rates$count), 2)]
compl_year_unpubl_num <- compl_year_publ_rates$count[seq(1, length(compl_year_publ_rates$count), 2)]
compl_year_total_num <- compl_year_publ_num + compl_year_unpubl_num
compl_year_publ_rate <- compl_year_publ_num/compl_year_total_num
compl_year_publ_rate


