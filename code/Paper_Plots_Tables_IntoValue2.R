#----------------------------------------------------------------------------------------------------------------------
#
# The following script reproduces the tables and figures from the main paper.
#
# 1) Demographic table
# 2) University medical center results
# 3) Kaplan-Meier curve
#
#----------------------------------------------------------------------------------------------------------------------


library(tidyverse)
library(KMsurv) #for Kaplan-Meier curve
library(lubridate)
library(assertthat)

#read in dataset
IntoValue_dataset <- read_csv("manual_check/final_results/IntoValue2_Dataset.csv", na = "NA")
IntoValue1_dataset <- read_csv("code/IntoValue1_Dataset.csv", na = "NA")


#--------------------------------------------------------------------------------------------------
# demographics table
#--------------------------------------------------------------------------------------------------

demographics <- function(categories, trial_num)
{
  demographics_tab <- cbind(categories, 100*round(categories/trial_num, 3))
  colnames(demographics_tab) <- c("Trials", "Percentage")
  return(demographics_tab)
}


demographics_table <- list()
trial_num <- dim(IntoValue_dataset)[1]


#Number of included trials
demographics_table[["Total"]] <- demographics(trial_num, trial_num)


#Type of intervention
interventions <- table(IntoValue_dataset$intervention_type, useNA = "ifany")
demographics_table[["Intervention"]] <- demographics(interventions, trial_num)


#Lead sponsor
lead_sponsor <- table(IntoValue_dataset$main_sponsor, useNA = "ifany")
demographics_table[["lead_sponsor"]] <- demographics(lead_sponsor, trial_num)


#study phase
#different ways of writing and defining the phases for CTgov and DRKS have to be combined
phase <- c("I" = sum(IntoValue_dataset$phase %in% c("Early Phase 1", "I", "Phase 1")),
              "I-II" = sum(IntoValue_dataset$phase %in% c("I-II", "Phase 1/Phase 2")),
              "II" = sum(IntoValue_dataset$phase %in% c("II", "IIa", "IIb", "Phase 2")),
              "II-III" = sum(IntoValue_dataset$phase %in% c("II-III", "Phase 2/Phase 3")),
              "III" = sum(IntoValue_dataset$phase %in% c("III", "IIIb", "Phase 3")),
              "IV" = sum(IntoValue_dataset$phase %in% c("IV", "Phase 4")),
              "Not given" = sum(IntoValue_dataset$phase %in% c("Not given", "[---]*", "N/A")))

demographics_table[["phase"]] <- demographics(phase, trial_num)


#mono-/multicentric
mono_multicentric <- c("Multicentric" = sum(IntoValue_dataset$is_multicentric == TRUE),
                          "Monocentric" = sum(IntoValue_dataset$is_multicentric == FALSE))
demographics_table[["Mono_Multicentric"]] <- demographics(mono_multicentric, trial_num)


#number of participants per study
#DRKS: targetSize (but this is only planned and not actual)
sample_size <- c("1 - 100" = sum(IntoValue_dataset$enrollment > 0 & IntoValue_dataset$enrollment <= 100, na.rm = TRUE),
                    "100 - 500" = sum(IntoValue_dataset$enrollment > 100 & IntoValue_dataset$enrollment <= 500, na.rm = TRUE),
                    "> 500" = sum(IntoValue_dataset$enrollment > 500, na.rm = TRUE),
                    "Not given" = sum(is.na(IntoValue_dataset$enrollment)))

demographics_table[["Sample_size"]] <- demographics(sample_size, trial_num)


#time of registration
time_to_registration <- c("before trial start" = sum(IntoValue_dataset$days_reg_to_start >= 0, na.rm = TRUE),
                             "after trial start" = sum(IntoValue_dataset$days_reg_to_start < 0, na.rm = TRUE),
                             "21 days after trial start" = sum(IntoValue_dataset$days_reg_to_start < -21, na.rm = TRUE),
                             "60 days after trial start" = sum(IntoValue_dataset$days_reg_to_start < -60, na.rm = TRUE),
                             "after trial completion" = sum(IntoValue_dataset$days_reg_to_compl < 0, na.rm = TRUE),
                             "after publication" = sum(IntoValue_dataset$days_reg_to_publ < 0, na.rm = TRUE),
                             "start date not given" = sum(is.na(IntoValue_dataset$start_date)))

demographics_table[["time_to_registration"]] <- demographics(time_to_registration, trial_num)


#trial end (CD)
trial_end_CD <- table(year(IntoValue_dataset$completion_date))

demographics_table[["trial_end_CD"]] <- demographics(trial_end_CD, trial_num)


#recruitment status
recruitment_status <- c("Completed" = sum(IntoValue_dataset$overall_status %in% c("Completed", "Recruiting complete, follow-up complete"), na.rm = TRUE),
                           "Terminated" = sum(IntoValue_dataset$overall_status %in% c("Terminated", "Recruiting stopped after recruiting started "), na.rm = TRUE),
                           "Suspended" = sum(IntoValue_dataset$overall_status %in% c("Suspended", "Recruiting suspended on temporary hold "), na.rm = TRUE),
                           "Unknown status" = sum(IntoValue_dataset$overall_status == "Unknown status", na.rm = TRUE))

demographics_table[["recruitment_status"]] <- demographics(recruitment_status, trial_num)

print(demographics_table)

demo_table_save <- do.call(rbind, demographics_table)
demo_table_save <- tibble(category = rownames(demo_table_save),
       Trials = demo_table_save[,1],
       Percentage = demo_table_save[,2] %>% round(2)) 
write_csv(demo_table_save, "results_for_paper/demographics_table.csv")

#--------------------------------------------------------------------------------------------------
# cities results table
#--------------------------------------------------------------------------------------------------

IntoValue_dataset_cities <- IntoValue_dataset %>%
  mutate(days_to_publ = pmin(days_to_publication,   #get minimum of days to pub or to summary result
                             days_to_summary, na.rm = TRUE))

#still has to solve this issue (after updating the unclear cases)
assert_that(sum(!is.na(IntoValue_dataset_cities$days_to_publ)) ==
              sum(!is.na(IntoValue_dataset_cities$has_publication)))

IntoValue_dataset_cities$lead_cities <- IntoValue_dataset_cities$lead_cities %>% 
  str_replace_all("Charite", "Berlin")

IntoValue_dataset_cities$lead_cities <- paste("All", IntoValue_dataset_cities$lead_cities)


#list of city names
cities <- IntoValue_dataset_cities$lead_cities %>% 
  str_split(" ") %>% 
  unlist() %>% 
  unique() %>% 
  sort()

total_enrollment <- IntoValue_dataset_cities$enrollment %>% sum(na.rm = TRUE)

CTgov_trials <- IntoValue_dataset_cities %>% 
  filter(is_CTgov)


#for given city calculates number of trials, published trials within 24 month after CD,
#and publication percentage
get_city_statistics <- function(city, city_assignments, days_to_publ, years=2)
{
  city_tot <- sum(str_detect(city_assignments, city), na.rm = TRUE)
  city_publ_24m_CD <- sum(str_detect(city_assignments, city) &
                            days_to_publ < years*365, na.rm = TRUE)
  city_perc <- round(city_publ_24m_CD/city_tot, 3) * 100

  city_stat <- tibble("City" = city, "Trials" = city_tot,
                      !!(paste0("Published <", years * 12, "m after CD")) := city_publ_24m_CD,
                      "Percentage" = city_perc)

  return(city_stat)
}

#main table for 2 years
city_statistics_lead <- map(cities, get_city_statistics,
                            city_assignments = IntoValue_dataset_cities$lead_cities,
                            days_to_publ = IntoValue_dataset_cities$days_to_publ)
city_statistics_lead <- do.call(rbind, city_statistics_lead)

print(city_statistics_lead, n = Inf)
city_statistics_lead$Percentage <- city_statistics_lead$Percentage %>% round(2)
write_csv(city_statistics_lead, "results_for_paper/city_statistics.csv")



#additional table for 5 years

#we finished the publication search beginning of Dec. 2020
#and take this as cutoff date for the timeframe 
# THE DATE STILL HAS TO BE DISCUSSED IN THE GROUP!!!
cutoff_date <- dmy("01.12.2020") - months(60)
IntoValue_dataset_cities_5y <- IntoValue_dataset_cities %>% filter(completion_date < cutoff_date)

city_statistics_lead_5y <- map(cities, get_city_statistics,
                            city_assignments = IntoValue_dataset_cities_5y$lead_cities,
                            days_to_publ = IntoValue_dataset_cities_5y$days_to_publ,
                            years = 50) #for this subgroup we do not stop counting publications after 5 years, but allow any duration
city_statistics_lead_5y <- do.call(rbind, city_statistics_lead_5y)

print(city_statistics_lead_5y, n = Inf)
city_statistics_lead_5y$Percentage <- city_statistics_lead_5y$Percentage %>% round(2)
write_csv(city_statistics_lead_5y, "results_for_paper/city_statistics_5years.csv")



unpublished_trials_5y <- IntoValue_dataset_cities_5y %>% 
  filter(!has_publication)


#--------------------------------------------------------------------------------------------------
# cities results plot
#--------------------------------------------------------------------------------------------------


#use old IntoValue1 dataset
IntoValue1_dataset <- IntoValue1_dataset %>%
  mutate(days_to_publ = pmin(days_to_publication_CD,   #get minimum of days to pub or to summary result
                             days_to_summary_CD, na.rm = TRUE)) %>%
  filter(lead_or_facility == "lead")


#only count the studies with 24 month follow up time
cutoff_date <- dmy("01.12.2017") - months(24)
has_long_followup <- IntoValue1_dataset$completion_date < cutoff_date
IntoValue1_dataset <- IntoValue1_dataset[has_long_followup,]


#combine old and new dataset into one table
IntoValue1_dataset$lead_cities <- IntoValue1_dataset$lead_cities %>% 
  str_replace_all("Lübeck", "Schleswig-Holstein") %>%
  str_replace_all("Kiel", "Schleswig-Holstein")

IntoValue1_dataset$lead_cities <- paste("All", IntoValue1_dataset$lead_cities)


city_statistics_old <- map(cities, get_city_statistics,
    city_assignments = IntoValue1_dataset$lead_cities,
    days_to_publ = IntoValue1_dataset$days_to_publ)
city_statistics_old <- do.call(rbind, city_statistics_old) %>%
  rename(trials_IntoValue1 = Trials,
         timely_published_IntoValue1 = `Published <24m after CD`,
         percentage_IntoValue1 = Percentage)

city_statistics_comp <- city_statistics_lead %>% 
  left_join(city_statistics_old, by = "City") %>%
  mutate(percentage_diff = Percentage - percentage_IntoValue1) 
write_csv(city_statistics_comp, "results_for_paper/city_statistics_comp.csv")

#plot new results per UMC with changes compared to previous study
city_statistics_comp$percentage_diff_chr <- city_statistics_comp$percentage_diff %>% 
  round(2) %>% as.character() %>% 
  map_chr(function(x) ifelse(str_detect(x, "-"), x, paste0("+", x)))

city_statistics_comp <- city_statistics_comp %>%
  mutate(fill_col = "#006780")
city_statistics_comp[city_statistics_comp$City == "All",]$fill_col <- "#D78102"


ggplot(data=city_statistics_comp, 
       aes(x=reorder(City, -Percentage), y=Percentage)) + 
  geom_bar(position = 'dodge', stat='identity', fill = city_statistics_comp$fill_col,
           colour="#444444", size = 0.6) +
  geom_text(aes(label=percentage_diff_chr),  position=position_dodge(width=0), vjust=-1.25) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16)) +
  xlab("City") + ylab("Percentage published < 24 months") +
  ylim(c(0,100))
ggsave("results_for_paper/City_changes.svg", width = 35, height = 18, units = "cm", dpi = 600)





ggplot(data=city_statistics_comp_diffplot, 
       aes(x=City, y=perc, 
           fill = bar_type)) + 
  geom_bar(position = 'stack', stat='identity', fill = city_statistics_comp_diffplot$fill_col, colour="#444444", size = 0.6) +
  #scale_fill_manual(values = c("#bcbcc3", "#0085A6", "#006780")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.position = "none") +
  xlab("City") + ylab("Percentage published < 24 months")
ggsave("results_for_paper/City_changes_2.png", width = 35, height = 18, units = "cm", dpi = 300)




city_statistics_comp_diffplot <- city_statistics_comp %>% 
  select(City, Percentage, percentage_IntoValue1, percentage_diff, fill_col) %>%
  mutate(perc_main = ifelse(Percentage < percentage_IntoValue1, Percentage, percentage_IntoValue1)) %>%
  mutate(perc_plus = ifelse(percentage_diff > 0, percentage_diff, 0)) %>%
  mutate(perc_minus = ifelse(percentage_diff < 0, -percentage_diff, 0)) %>%
  select(-Percentage, -percentage_IntoValue1, -percentage_diff) %>%
  pivot_longer(!c(City, fill_col) , names_to = "bar_type", values_to = "perc")


city_statistics_comp_diffplot$bar_type <- factor(city_statistics_comp_diffplot$bar_type, 
                                                 levels=c("perc_minus","perc_plus", "perc_main"))
city_order <- (city_statistics_comp %>% arrange(-Percentage))$City
city_statistics_comp_diffplot$City <- factor(city_statistics_comp_diffplot$City, 
                                             levels=city_order)


city_statistics_comp_diffplot$fill_col[city_statistics_comp_diffplot$bar_type == "perc_main"] <- "#006780"
city_statistics_comp_diffplot$fill_col[city_statistics_comp_diffplot$bar_type == "perc_plus"] <- "#0085A6"
city_statistics_comp_diffplot$fill_col[city_statistics_comp_diffplot$bar_type == "perc_minus"] <- "#bcbcc3"
city_statistics_comp_diffplot[city_statistics_comp_diffplot$City == "All" & 
                              city_statistics_comp_diffplot$bar_type == "perc_main",]$fill_col <- "#D78102"
city_statistics_comp_diffplot[city_statistics_comp_diffplot$City == "All" & 
                              city_statistics_comp_diffplot$bar_type == "perc_plus",]$fill_col <- "#e9a602"

  
ggplot(data=city_statistics_comp_diffplot, 
       aes(x=City, y=perc, 
           fill = bar_type)) + 
  geom_bar(position = 'stack', stat='identity', fill = city_statistics_comp_diffplot$fill_col, colour="#444444", size = 0.6) +
  #scale_fill_manual(values = c("#bcbcc3", "#0085A6", "#006780")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.position = "none") +
  xlab("City") + ylab("Percentage published < 24 months")
ggsave("results_for_paper/City_changes_2.png", width = 35, height = 18, units = "cm", dpi = 300)



#--------------------------------------------------------------------------------------------------
# Kaplan-Meier plot
#--------------------------------------------------------------------------------------------------

#get minimum of days to pub or to summary result & time in days that one study could be tracked
#(end of publication search was on ???)
IntoValue_KM_data <- IntoValue_dataset %>%
  mutate(days_to_publ = pmin(days_to_publication,   #get minimum of days to pub or to summary result
                             days_to_summary, na.rm = TRUE)) %>%
  mutate(days_obs = dmy("01.08.2020") - completion_date) #still need to discuss end date of publ search

#sort the studies according to completion years for Kaplan-Meier curve
compl_years <- IntoValue_KM_data$completion_date %>% str_sub(1, 4)
subsets_KM_plot <- list("Total" = rep(TRUE, dim(IntoValue_KM_data)[1]) ,
                         "2014" = compl_years == "2014",
                         "2015" = compl_years == "2015",
                         "2016" = compl_years == "2016",
                         "2017" = compl_years == "2017")

#define the year range and create corresponding day vector
year_range <- 1:6
days <- 1:(365*last(year_range))


#function that summarizes the publication and censoring (= publications could not be tracked longer) events
#for one subgroup and uses the lifetab function from the KMsurv package to calculate the KM curve
get_KM_curve <- function(subset, days, days_to_publ, days_to_cens)
{
  #only take certain subset of data, e.g. different completion years
  days_to_publ <- days_to_publ[subset]
  days_to_cens <- days_to_cens[subset]

  #for the publications before study end set time to publ to 1 day
  days_to_publ[which(days_to_publ <= 0)] <- 1
  #if the publication takes longer than the time interval we look at,
  #set it to no publ
  days_to_publ[days_to_publ > last(days)] <- NA

  #if the census time is longer than the total time interval we look at,
  #set the last day of the time interval as cens day
  days_to_cens[days_to_cens > last(days)] <- last(days)

  #is study either censored (= no publ found) or published first
  event_or_cens <- ifelse(!is.na(days_to_publ), 1, 0)

  #count the publication events for each day
  pub_counts <- rep(0, length(days))
  interval_counts <- days_to_publ[days_to_publ %in% days]
  interval_counts <- table(interval_counts)
  pub_counts[as.integer(names(interval_counts))] <- interval_counts

  #count the censoring events for each day
  cens_counts <- rep(0, length(days))
  cens_interval_counts <- days_to_cens[days_to_cens %in% days & is.na(days_to_publ)]
  cens_interval_counts <- table(cens_interval_counts)
  cens_counts[as.integer(names(cens_interval_counts))] <- cens_interval_counts

  #use the lifetab function from the KMsurv package to calculate the Kaplan-Meier curve
  KL_curve <- lifetab(c(0,days), length(days_to_publ), cens_counts, pub_counts)

  return(KL_curve$surv)
}


#calculate cumulative distributions for subsets and make data tidy for plotting with ggplot2
cum_dist_years <- sapply(subsets_KM_plot, get_KM_curve, days, IntoValue_KM_data$days_to_publ, IntoValue_KM_data$days_obs) %>%
  as_tibble() %>%
  add_column(days) %>%
  mutate(months = time_length(days(days), unit="months")) %>%
  tail(-1) #delete first datapoint as percentage starts at 1 (which we don't want, as some publ. can be found before day 1)

#make data tidy for plotting with ggplot2
cum_dist_years <- cum_dist_years %>%
  gather(colnames(cum_dist_years) %>% head(-2) , key = "category", value = "fraction")

#delete last datapoints for the later years, as we couldn't follow up the trials
#cum_dist_years <- cum_dist_years %>%
#  filter(!(category == "2017" & months > 48)) %>%
#  filter(!(category == "2016" & months > 60))


ggplot(cum_dist_years, aes(x = months, y = fraction, color = category)) +
  geom_step(size = 1) + #, color = "#C02942") +
  theme_minimal() +
  xlab("Months") + ylab("Unpublished studies (%)") +
  scale_color_brewer(name = "Primary\nCompletion\nYear" , palette = 'Dark2') +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16)) +
  scale_x_continuous(name = "Months", breaks=c(0, year_range)*12,
                     labels = paste0(c(0, year_range)*12, "")) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  theme(axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))
ggsave("results_for_paper/Kaplan_Meier.png", width = 33, height = 20, units = "cm", dpi = 300)

