
#--------------------------------------------------------------------------------------------------
# functions for Kaplan-Meier curve calculation
#--------------------------------------------------------------------------------------------------

#calculates cumulative distribution of published studies given day-vector (x-axis) and the actual time interval data for the publications
get_cum_dist <- function(subset, days, day_intervals)
{
  #only take certain subset of data
  day_intervals <- day_intervals[subset]

  counts <- rep(0, length(days))

  day_intervals[which(day_intervals <= 0)] <- 1 #for the publications before study end set time to publ to 1 day
  interval_counts <- day_intervals[day_intervals %in% days]
  interval_counts <- table(interval_counts)

  counts[as.integer(names(interval_counts))] <- interval_counts
  cum_counts <- cumsum(counts)
  cum_frac <- 1 - (cum_counts/length(day_intervals))

  return(cum_frac)
}

get_KM_curve <- function(subset, days, days_to_publ, days_to_cens)
{
  #only take certain subset of data
  days_to_publ <- days_to_publ[subset]
  days_to_cens <- days_to_cens[subset]

  #for the publications before study end set time to publ to 1 day
  days_to_publ[which(days_to_publ <= 0)] <- 1
  #if the publication takes longer than the time interval we look at, set it to no publ
  days_to_publ[days_to_publ > last(days)] <- NA

  #if the census time is longer than the total time interval we look at, set the last day of the time interval as cens day
  days_to_cens[days_to_cens > last(days)] <- last(days)

  #is study either censored (= no publ found) or published first
  event_or_cens <- ifelse(!is.na(days_to_publ), 1, 0)


  pub_counts <- rep(0, length(days))
  interval_counts <- days_to_publ[days_to_publ %in% days]
  interval_counts <- table(interval_counts)
  pub_counts[as.integer(names(interval_counts))] <- interval_counts

  cens_counts <- rep(0, length(days))
  cens_interval_counts <- days_to_cens[days_to_cens %in% days & is.na(days_to_publ)]
  cens_interval_counts <- table(cens_interval_counts)
  cens_counts[as.integer(names(cens_interval_counts))] <- cens_interval_counts

  KL_curve <- lifetab(c(0,days), length(days_to_publ), cens_counts, pub_counts)

  return(KL_curve$surv)
}



#--------------------------------------------------------------------------------------------------
# functions to process the timely publication input options
#--------------------------------------------------------------------------------------------------

#if we only want to count publications and not summary results, we also have to take the corresponding
#version of the "days to publication" column, which does not include summary result publication dates
#that predate the real publication
set_days_to_publ <- function(summary_type_in, compl_date_in, input_table) {

  if(summary_type_in == "Publications and Summary results") {
    if(compl_date_in == "Primary completion date (CT.gov only)") {
      days_to_pub_col <- input_table$days_pcd_to_publication
      days_to_sum_col <- input_table$days_pcd_to_summary
    } else {
      days_to_pub_col <- input_table$days_cd_to_publication
      days_to_sum_col <- input_table$days_cd_to_summary
    }

    days_to_pub <- pmin(days_to_pub_col, days_to_sum_col, na.rm = TRUE)
    #days_to_pub <- map2_int(as.integer(days_to_pub_col), as.integer(days_to_sum_col),
    #                        function(x, y) ifelse( !all(is.na(c(x, y))), min(x, y, na.rm = TRUE), NA))
  } else if (summary_type_in == "Publications") {
    if(compl_date_in == "Primary completion date (CT.gov only)") {
      days_to_pub <- input_table$days_pcd_to_publication
    } else {
      days_to_pub <- input_table$days_cd_to_publication
    }
  } else {
    if(compl_date_in == "Primary completion date (CT.gov only)") {
      days_to_pub <- input_table$days_pcd_to_summary
    } else {
      days_to_pub <- input_table$days_cd_to_summary
    }
  }

  return(days_to_pub)
}


#how many days could we observe the different trials since CD/PCD 
#important for censoring for the Kaplan-Meier curve 
set_days_observed <- function(compl_date_in, input_table) { 
  
  #make vector with the cutoff dates for each trial
  cutoff_vec <- rep(dmy("01.12.2017"), dim(input_table)[1])
  cutoff_vec[input_table$iv_version %in% c("IV2", "IV2_dupl")] <- dmy("01.12.2020")
  
  
  if(compl_date_in == "Primary completion date (CT.gov only)") { 
    days_observed = cutoff_vec - input_table$primary_completion_date 
  } else { 
    days_observed = cutoff_vec - input_table$completion_date 
  } 
  
  return(days_observed) 
} 


#for years to publ need to consider 2 things:
#1) how many years since CD/PCD do we consider as timely publication
#2) how long could we at least follow up a study
#usually these two values should be the same, only for the special subset of 'any duration'
#we chose trails with at least a 6 year follow-up period
set_years_to_publ <- function(month_to_publ) {
  if(month_to_publ == "Any duration (trials with >6 years follow-up period)") {
    years_until_publ <- 100
    month_follow_up <- 6 * 12

  } else if(month_to_publ == "Any duration (any follow-up period)") {
    years_until_publ <- 100
    month_follow_up <- -10

  } else {
    years_until_publ <- as.integer(month_to_publ)/12
    month_follow_up <- as.integer(month_to_publ)
  }

  return(c(years_until_publ, month_follow_up))
}


filter_followup_time <- function(input_table, followup_time, compl_date_in) {

  # we finished the publication search beginning of Dec. 2017 for IV1
  # and at beginning of Dec. 2020 for IV2
  # and take this as cutoff date for the timeframe
  cutoff_date_IV1 <- dmy("01.12.2017") - months(followup_time)
  cutoff_date_IV2 <- dmy("01.09.2020") - months(followup_time)
  
  if(compl_date_in == "Primary completion date (CT.gov only)") {
    compl_col <- "primary_completion_date"
  } else {
    compl_col <- "completion_date"
  }

  has_long_followup_IV1 <- (input_table[[compl_col]] < cutoff_date_IV1)
  has_long_followup_IV2 <- (input_table[[compl_col]] < cutoff_date_IV2)
  is_IV1 <- input_table[["iv_version"]] %in% c("IV1", "IV1_dupl")
  is_IV2 <- input_table[["iv_version"]] %in% c("IV2", "IV2_dupl")
  
  has_long_followup <- (has_long_followup_IV1 & is_IV1) | (has_long_followup_IV2 & is_IV2)

  return(input_table[has_long_followup,])
}


set_publ_type <- function(publ_type_in) {
  #first select which publications are counted
  if(publ_type_in == "Full search (incl. Google Scholar)") {
    publ_type = c("Registry linked", "Pubmed", "Hand search", "Dissertation", 
                  "Publ found in Google ID search", "Publ found in Google search (no ID)")
  } else if(publ_type_in == "Registry + Pubmed") {
    publ_type = c("Registry linked", "Pubmed")
  } else {
    publ_type = c("Registry linked")
  }

  return(publ_type)
}


#--------------------------------------------------------------------------------------------------
# functions to process the subset input options
#--------------------------------------------------------------------------------------------------

set_registry <- function(registry_in) {
  if(registry_in == "CT.gov + DRKS") {
    registry <- c("ClinicalTrials.gov", "DRKS")
  } else if(registry_in == "CT.gov") {
    registry <- c("ClinicalTrials.gov")
  } else {
    registry <- c("DRKS")
  }

  return(registry)
}


set_compl_status <- function(compl_status_in) {
  if(compl_status_in == "Any status") {
    compl_status <- c("Completed", "Terminated", "Unknown status", "Suspended",
                      "Recruiting complete, follow-up complete",
                      "Recruiting stopped after recruiting started",
                      "Recruiting suspended on temporary hold")
  } else if(compl_status_in == "Completed") {
    compl_status <- c("Completed", "Recruiting complete, follow-up complete")
  } else if(compl_status_in == "Terminated") {
    compl_status <- c("Terminated", "Recruiting stopped after recruiting started")
  } else if(compl_status_in == "Suspended") {
    compl_status <- c("Suspended", "Recruiting suspended on temporary hold")
  } else {
    compl_status <- c("Unknown status")
  }

  return(compl_status)
}


set_sponsor_status <- function(sponsor_status_in) {
  if(sponsor_status_in == "Lead or Facility") {
    sponsor_status <- c("lead", "facility")
  } else if(sponsor_status_in == "Lead trials only") {
    sponsor_status <- c("lead")
  } else {
    sponsor_status <- c("facility")
  }

  return(sponsor_status)
}


set_multicentric_status <- function(multicentric_in) {
  if(multicentric_in == "Mono- or Multicentric") {
    multicentric <- c("TRUE", "FALSE", "Not given")
  } else if(multicentric_in == "Multicentric") {
    multicentric <- c("TRUE")
  } else {
    multicentric <- c("FALSE")
  }

  return(multicentric)
}


set_industry_status <- function(industry_in) {
  if(industry_in == "Any") {
    industry <- c("Industry", "Other")
  } else if(industry_in == "Industry") {
    industry <- c("Industry")
  } else {
    industry <- c("Other")
  }

  return(industry)
}


set_phase_status <- function(phase_sub_in) {
  if(phase_sub_in == "Any") {
    phase_sub <- c("Early Phase 1", "I", "Phase 1", "I-II", "Phase 1/Phase 2",
                   "II", "IIa", "IIb", "Phase 2", "II-III", "Phase 2/Phase 3",
                   "III", "IIIb", "Phase 3", "IV", "Phase 4", "N/A", "NA", "[---]*",
                   "Not given")
  } else if(phase_sub_in == "I") {
    phase_sub <- c("Early Phase 1", "I", "Phase 1")
  } else if(phase_sub_in == "I-II") {
    phase_sub <- c("I-II", "Phase 1/Phase 2")
  } else if(phase_sub_in == "II") {
    phase_sub <- c("II", "IIa", "IIb", "Phase 2")
  } else if(phase_sub_in == "II-III") {
    phase_sub <- c("II-III", "Phase 2/Phase 3")
  } else if(phase_sub_in == "III") {
    phase_sub <- c("III", "IIIb", "Phase 3")
  } else if(phase_sub_in == "IV") {
    phase_sub <- c("IV", "Phase 4")
  } else {
    phase_sub <- c("N/A", "NA", "Not given")
  }

  return(phase_sub)
}


set_reg_time <- function(reg_time_in, input_table) {
  switch(reg_time_in,
         "Any" = {reg_filter <- rep(TRUE, length(input_table$days_reg_to_start))},
         "In the month of the trial start or earlier" = {
           reg_filter <- floor_date(input_table$start_date, unit = "month") >= 
             floor_date(input_table$registration_date, unit = "month")},
         "After the month of the trial start" = {
           reg_filter <- floor_date(input_table$start_date, unit = "month") <
             floor_date(input_table$registration_date, unit = "month")},
         "After trial completion (CD)" = {reg_filter <- input_table$days_reg_to_cd < 0},
         "After trial completion (PCD)" = {reg_filter <- input_table$days_reg_to_pcd < 0},
         "After publication" = {reg_filter <- input_table$days_reg_to_publication < 0}
  )

  return(reg_filter)
}


set_intervention_status <- function(intervention_in) {
  if(intervention_in == "Any") {
    intervention <- c("Behavioral", "Biological", "Combination Product", "Diagnostic Test",
                      "Dietary Supplement", "Genetic", "Not given",
                      "Other", "Procedure", "Radiation", "Drug", "Device")
  } else if(intervention_in == "Drug") {
    intervention <- c("Drug")
  } else if(intervention_in == "Device") {
    intervention <- c("Device")
  } else {
    intervention <- c("Behavioral", "Biological", "Combination Product", "Diagnostic Test",
                      "Dietary Supplement", "Genetic", "Not given",
                      "Other", "Procedure", "Radiation")
  }

  return(intervention)
}


set_participants_status <- function(participants_in) {
  if(participants_in == "Any") {
    participants <- "Any"
  } else if(participants_in == "<100") {
    participants <- c(1, 99)
  } else if(participants_in == "100-500") {
    participants <- c(100, 500)
  } else if(participants_in == ">500") {
    participants <- c(501, 10000000)
  } else { #participants number not given
    participants <- NA
  }

  return(participants)
}

filter_participants <- function(input_table, interval) {
  if(is.na(interval[1])) {
    in_interval <- is.na(input_table$enrollment)
  } else if(interval == "Any") {
    in_interval <- rep(TRUE, length(input_table$enrollment))
  } else {
    in_interval <- input_table$enrollment >= interval[1] &
                   input_table$enrollment <= interval[2] &
                   !is.na(input_table$enrollment)
  }

  return(input_table[in_interval, ])
}


set_complyear_status <- function(complyear_in) {
  if(complyear_in == "Any") {
    complyear <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  } else {
    complyear <- complyear_in
  }

  return(complyear)
}

#when showing data for IV2 or IV1+2 show new results for duplicate trials
#when showing old IV1 data only, show old results for duplicate trials
set_study_version <- function(version_in) {
  if(version_in == "Any") {
    version <- c("IV1", "IV2", "IV2_dupl")
  } else if(version_in == "IntoValue1") {
    version <- c("IV1", "IV1_dupl")
  } else {
    version <- c("IV2", "IV2_dupl")
  }

  return(version)
}


#--------------------------------------------------------------------------------------------------
# Code for filtering data according to set input options & arranging for table or plot
#--------------------------------------------------------------------------------------------------

#set of "empty" entries for each city to get all cities displayed at any time - not counted in the end
make_basic_city_tab <- function(input_data, delayed = FALSE)
{

  cities <- c("All trials combined", "Aachen", "Berlin", "Bochum", "Bonn", "Dresden", "Düsseldorf",
                "Erlangen", "Duisburg-Essen", "Frankfurt", "Freiburg",
                "Giessen", "Göttingen", "Greifswald", "Halle", "Hamburg", "Hannover",
                "Heidelberg", "Jena", "Köln", "Leipzig", "Magdeburg",
                "Mainz", "Mannheim", "Marburg", "LMU München", "TU München", "Münster",
                "Regensburg", "Rostock", "Schleswig-Holstein", "Homburg", "Tübingen", 
                "Ulm", "Würzburg", "Witten-Herdecke")
  #make small tibble containing all the city names that is added just before the grouping
  #this makes sure that all cities are displayed at all times, even with no studies
  #this additional entry per city has to be removed after the grouping
  cities_basic <- input_data[rep(1,length(cities)),]
  if(delayed) {
    cities_basic[["delay"]] <- TRUE
  } else {
    cities_basic[["publ"]] <- TRUE
  }
  cities_basic$city <- cities

  return(cities_basic)
}

#uses all the currently choosen input options to filter the dataset accordingly
make_table_data <- function(input_table, summary_type_in, compl_status_in, sponsor_status_in, compl_date_in,
                            month_to_publ_in, reg_time_in, multicentric_in, industry_in, phase_sub_in,
                            publ_type_in, registry_in, intervention_in, participants_in, complyear_in, version_in, delayed = FALSE)
{

  #set variables for timely publication options
  days_to_pub <- set_days_to_publ(summary_type_in, compl_date_in, input_table)
  years_until_publ <- set_years_to_publ(month_to_publ_in)[1]
  month_follow_up <- set_years_to_publ(month_to_publ_in)[2]
  publ_type <- set_publ_type(publ_type_in)

  #set variables for subset options
  registry_sub <- set_registry(registry_in)
  compl_status <- set_compl_status(compl_status_in)
  sponsor_status <- set_sponsor_status(sponsor_status_in)
  multicentric <- set_multicentric_status(multicentric_in)
  industry <- set_industry_status(industry_in)
  phase_sub <- set_phase_status(phase_sub_in)
  reg_filter <- set_reg_time(reg_time_in, input_table)
  intervention <- set_intervention_status(intervention_in)
  participants <- set_participants_status(participants_in)
  complyear <- set_complyear_status(complyear_in)
  version <- set_study_version(version_in)


  #one has to be careful with the publication types - depending on which publications I want to measure
  #(summary results or publications) the publication type of some publications has to be altered
  if(summary_type_in == "Publications and Summary results") {
    #all entries with summary results have to be marked accordingly w.r.t. publication type
    input_table[input_table$has_summary_results,]$identification_step <- "Registry linked"
  } else if(summary_type_in == "Summary results (CT.gov only)") {
    #set all publication types to "No publ" exept for the cases that have a summary result
    input_table$identification_step <- "No publ"
    has_summary <- !is.na(input_table$days_cd_to_summary)
    input_table[input_table$has_summary_results,]$identification_step <- "Registry linked"
  } else {
    #do nothing if only publications are used
  }


  #finally add timeframe for publ.

  if(delayed == TRUE) {
    input_table[["publ"]] <- input_table$days_reg_to_start > -60
  } else {
    #add those publ to the publ count column
    input_table[["publ"]] <- input_table[["identification_step"]] %in% publ_type
    input_table[["publ"]] <- input_table[["publ"]] & (days_to_pub < (365 * years_until_publ))

    input_table[["publ"]][is.na(input_table[["publ"]])] <- FALSE
  }

  #filter for registration timepoint & completion status and group by cities
  input_table_filtered <- input_table[reg_filter,] %>%
    filter_participants(participants) %>%
    filter_followup_time(month_follow_up, compl_date_in) %>%
    filter(registry %in% registry_sub) %>%
    filter(recruitment_status %in% compl_status) %>%
    filter(lead_or_facility %in% sponsor_status) %>%
    filter(is_multicentric %in% multicentric) %>%
    filter(main_sponsor %in% industry) %>%
    filter(phase %in% phase_sub) %>%
    filter(intervention_type %in% intervention) %>%
    filter(completion_year %in% complyear) %>%
    filter(iv_version %in% version)

  table_cities <- input_table_filtered %>%
    rbind(make_basic_city_tab(input_table)) %>% #add the cities tibble to ensure that all cities are present in the results
    group_by(city, publ) %>%
    summarise(count = n(), enroll = sum(enrollment, na.rm = TRUE)) %>%
    mutate(percent = (count-1)/(sum(count)-1)) %>% #remove the extra count from the cities column when calculating the percentage
    mutate(total = sum(count) - 1)

  #get the total number of participants per city
  table_enroll_tot <- input_table_filtered %>%
    rbind(make_basic_city_tab(input_table)) %>%
    group_by(city) %>%
    summarise(enroll_tot = sum(enrollment, na.rm = TRUE))

  table_data <- table_cities %>%
    filter(publ == TRUE) %>%
    add_column(enroll_tot = table_enroll_tot$enroll_tot) %>%
    mutate(participants_unpubl = enroll_tot - enroll) %>%
    rename(participants = enroll_tot) %>%
    select(-enroll) %>%
    arrange(desc(percent)) %>%
    mutate(count = count - 1) #remove the extra count from the cities column

  return(table_data)
}



#arranges data for display as table
make_table <- function(input_table, summary_type_in, compl_status_in, sponsor_status_in,
                       compl_date_in, month_to_publ_in, reg_time_in, multicentric_in, industry_in, phase_sub_in,
                       publ_type_in, registry_in, intervention_in, participants_in, complyear_in, version_in, delayed = FALSE)
{
  #first filter data according to input options
  table_data <- make_table_data(input_table, summary_type_in, compl_status_in, sponsor_status_in,
                                compl_date_in, month_to_publ_in, reg_time_in, multicentric_in, industry_in, phase_sub_in,
                                publ_type_in, registry_in, intervention_in, participants_in, complyear_in, version_in, delayed)

  table_data <- table_data %>%
    ungroup() %>%
    select(-publ) %>%
    mutate(percent = round(percent, 2)) %>%
    select("city", "count", "total", "percent",
           "participants_unpubl", "participants") %>%
    rename("City" = city) %>%
    rename("Total studies" = total) %>%
    rename("Planned participants" = participants) %>%
    rename("Planned participants in unpublished studies" = participants_unpubl)


  if(delayed) {
    table_data <- table_data %>%
      rename("Percentage registered in time" = percent) %>%
      rename("Studies with timely registration" = count)
  } else {
    table_data <- table_data %>%
      rename("Percentage published" = percent) %>%
      rename("Studies with publications" = count)
  }


  return(table_data)
}


#arranges data for plotting
make_plot_data <- function(input_table, summary_type_in, compl_status_in, sponsor_status_in,
                           compl_date_in, month_to_publ_in, reg_time_in, multicentric_in, industry_in, phase_sub_in,
                           publ_type_in, registry_in, intervention_in, participants_in, complyear_in, version_in)
{
  #first filter data according to input options
  plot_data <- make_table_data(input_table, summary_type_in, compl_status_in, sponsor_status_in,
                               compl_date_in, month_to_publ_in, reg_time_in, multicentric_in, industry_in, phase_sub_in,
                               publ_type_in, registry_in, intervention_in, participants_in, complyear_in, version_in)

  #modifications specific for plotting
  plot_data$city <- factor(plot_data$city, levels = plot_data$city)


  plot_data$bar_col <- ifelse(plot_data$city == "All trials combined", "#e9a602", "#5799c7") # #E97F02

  plot_data$id = 1:dim(plot_data)[1]


  plot_data$tooltip <- as.character(paste0("<b>", plot_data$city, "</b><br>",
                                           plot_data$count, " published studies <br>",
                                           plot_data$count/plot_data$percent, " total studies"))

  return(plot_data)
}

