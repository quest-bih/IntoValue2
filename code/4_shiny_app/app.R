library(shiny)
library(DT)
library(tidyverse)
library(ggvis)
library("shinythemes")
library(RColorBrewer)
library(lubridate)
library(KMsurv) #for Kaplan-Meier curve

source("app_functions.R", encoding = "UTF-8")

#read in .rds output from Journal_Whitelist_script.R here
IntoValue_app_table <- read_csv("data/IntoValue_Dataset_combined.csv")

#rename Duisburg to Duisburg-Essen
IntoValue_app_table$city <- IntoValue_app_table$city %>% str_replace("Duisburg", "Duisburg-Essen")

#merge iv_version and is_dupe column for easier filtering
IntoValue_app_table <- IntoValue_app_table %>% 
  mutate(iv_version = case_when(
    iv_version == 1 & !is_dupe  ~ "IV1",
    iv_version == 1 & is_dupe  ~ "IV1_dupl",
    iv_version == 2 & !is_dupe  ~ "IV2",
    iv_version == 2 & is_dupe  ~ "IV2_dupl"
  ),
  is_multicentric = is_multicentric %>% replace_na("Not given"))


IntoValue_studies_only <- IntoValue_app_table %>%
  filter(city == "All trials combined")


ui <- navbarPage("IntoValue", theme = shinytheme("flatly"),
                 tabPanel("Timely reporting",
                          h1("IntoValue - Institutions’ contribution to increasing value and reducing waste", align = "center"),
                          h4("Clinical trial dissemination rates of all German university medical centers", align = "center"),
                          br(),
                          h5(HTML(paste0("The following Shiny app accompanies the two publications 
                                                    on the clinical trial dissemination rates of all German 
                                                    university medical centers and allows to further explore the results.
                                                    The publications are ",
                                         a(href = 'https://www.biorxiv.org/content/10.1101/467746v2',
                                           "'Result dissemination from clinical trials conducted at German university 
                                                      medical centers was delayed and incomplete'"), " and ",
                                         a(href = 'https://osf.io/98j7u/',
                                           "'Results dissemination from completed clinical trials conducted at German 
                                                      university medical centers remains delayed and incomplete. The 2014-2017 cohort'"), 
                                         ". You can interactively choose different subsets of the data and different
                                                        ways of defining timely publication. The results can be displayed both as diagram or in a table
                                                        (see navigation bar at the top). Additionally, a Kaplan-Meier curve for the percentage of
                                                        unpublished studies over time can be displayed for different subgroups of the data as well
                                                        as different stratifying variables. The dataset, analysis code, as well as detailed methods
                                                        can be found on the OSF project pages for the ", a(href = 'https://osf.io/fh426/', "first"),
                                         " and the ", a(href = 'https://osf.io/98j7u/', "second"), " study and ", 
                                         a(href = 'https://github.com/quest-bih/IntoValue2', "this"), " Github repository. The combined dataset
                                                        for both studies can be found ", a(href = 'https://doi.org/10.5281/zenodo.5141343', "here"), 
                                         "."))),
                          
                          br(),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("What counts as timely publication?"),
                                     selectInput('publ', 'Count summary results', c("Publications and Summary results", "Publications", "Summary results (CT.gov only)")),
                                     selectInput('complDate', 'Count from which completion date', c("Completion date", "Primary completion date (CT.gov only)")),
                                     selectInput('timeframe', 'Published within how many month', c("12", "24", "30", "36", "48", "60", "Any duration (trials with >6 years follow-up period)", "Any duration (any follow-up period)"), selected = "24"),
                                     selectInput('pubType', 'Publication identified in which stage', c("Registry only", "Full search (incl. Google Scholar)"), selected = "Full search (incl. Google Scholar)"),
                                     helpText('There are different ways to define what a timely publication is. First, study results can either be
                                                          posted as summary results on the registry or published in a peer-reviewed journal. Second,
                                                          there are different dates on which a study could be considered completed: either the primary
                                                          completion date (when all data for the primary endpoints were collected) or the completion
                                                          date (last patient last visit). Third, there are different recommendations on how long
                                                          it should take to make study results available. Note: when different periods are choosen in the
                                                          "Published within how many month"-box, only those trials are considered that we were able to track for
                                                          this period (e.g. 60 month since the completion date).
                                                          Only exception is the category "Any duration (any follow-up period)", where all trials are considered.')
                                   ),
                                   wellPanel(
                                     h4("Different subgroups"),
                                     selectInput('version', 'Study version*', c("Any", "IntoValue1", "IntoValue2")),
                                     selectInput('registry', 'Registry', c("CT.gov + DRKS", "CT.gov", "DRKS")),
                                     selectInput('status', 'Completion status', c("Any status", "Completed", "Terminated", "Unknown status", "Suspended")),
                                     selectInput('sponsor', 'Responsibility level', c("Lead or Facility", "Lead trials only", "Facility trials only"), selected = "Lead trials only"),
                                     selectInput('registered', 'Registration at which timepoint', c("Any", "In the month of the trial start or earlier", 
                                                                                                    "After the month of the trial start",
                                                                                                    "After trial completion (CD)", "After trial completion (PCD)",
                                                                                                    "After publication")),
                                     selectInput('multicentric', 'Number of centers', c("Mono- or Multicentric", "Multicentric", "Monocentric")),
                                     selectInput('industry', 'Sponsor from industry or not', c("Any", "Industry", "Academic")),
                                     selectInput('phase', 'Trial phase', c("Any", "I", "I-II", "II", "II-III", "III", "IV", "Not given")),
                                     selectInput('intervention', 'Type of intervention', c("Any", "Drug", "Device", "Other")),
                                     selectInput('participants', 'Number of participants', c("Any", "<100", "100-500", ">500", "Not given")),
                                     selectInput('complyear', 'Completion year', c("Any", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")),
                                     helpText('There are many choices on which group of clinical trials to consider. All of these choices
                                          alter the results for the final publication rates. Only the trials in our dataset that match the set filter criteria
                                                           are considered for the calculation of the timely reporting percentage.'),
                                     helpText('*There are 128 trials that are part of the samples used for both studies. 
                                              The reason for this is either a change in the completion date in the registry after the
                                              end of the IntoValue1 study or the change in the search criteria from primary completion date (IntoValue1)
                                              to completion date (IntoValue2). In those cases the results from the second study are shown
                                              when "Any" or "IntoValue2" is chosen. When "IntoValue1" is chosen, the results from the first study are shown.')
                                   ),
                                   wellPanel(
                                     h4(HTML(paste0("This Shiny app is hosted by"))),
                                     tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 122, width = 187),href="https://www.bihealth.org/en/quest-center")
                                   )
                            ),
                            column(9,
                                   ggvisOutput('plot1')
                            )
                            
                          )
                 ),
                 tabPanel("Results table",
                          h1("IntoValue - Institutions’ contribution to increasing value and reducing waste", align = "center"),
                          h4("Clinical trial dissemination rates of all German university medical centers", align = "center"),
                          br(),
                          h5(HTML(paste0("The following Shiny app accompanies the two publications 
                                                    on the clinical trial dissemination rates of all German 
                                                    university medical centers and allows to further explore the results.
                                                    The publications are ",
                                         a(href = 'https://www.biorxiv.org/content/10.1101/467746v2',
                                           "'Result dissemination from clinical trials conducted at German university 
                                                      medical centers was delayed and incomplete'"), " and ",
                                         a(href = 'https://osf.io/98j7u/',
                                           "'Results dissemination from completed clinical trials conducted at German 
                                                      university medical centers remains delayed and incomplete. The 2014-2017 cohort'"), 
                                         ". You can interactively choose different subsets of the data and different
                                                        ways of defining timely publication. The results can be displayed both as diagram or in a table
                                                        (see navigation bar at the top). Additionally, a Kaplan-Meier curve for the percentage of
                                                        unpublished studies over time can be displayed for different subgroups of the data as well
                                                        as different stratifying variables. The dataset, analysis code, as well as detailed methods
                                                        can be found on the OSF project pages for the ", a(href = 'https://osf.io/fh426/', "first"),
                                         " and the ", a(href = 'https://osf.io/98j7u/', "second"), " study and ", 
                                         a(href = 'https://github.com/quest-bih/IntoValue2', "this"), " Github repository. The combined dataset
                                                        for both studies can be found ", a(href = 'https://doi.org/10.5281/zenodo.5141343', "here"), 
                                         "."))),
                          br(),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("What counts as timely publication?"),
                                     selectInput('tab_publ', 'Count summary results', c("Publications and Summary results", "Publications", "Summary results (CT.gov only)")),
                                     selectInput('tab_complDate', 'Count from which completion date', c("Completion date", "Primary completion date (CT.gov only)")),
                                     selectInput('tab_timeframe', 'Published within how many month', c("12", "24", "30", "36", "48", "60", "Any duration (trials with >6 years follow-up period)", "Any duration (any follow-up period)"), selected = "24"),
                                     selectInput('tab_pubType', 'Publication identified in which stage', c("Registry only", "Full search (incl. Google Scholar)"), selected = "Full search (incl. Google Scholar)"),
                                     helpText('There are different ways to define what a timely publication is. First, study results can either be
                                        posted as summary results on the registry or published in a peer-reviewed journal. Second,
                                              there are different dates on which a study could be considered completed: either the primary
                                              completion date (when all data for the primary endpoints were collected) or the completion
                                              date (last patient last visit). Third, there are different recommendations on how long
                                              it should take to make study results available. Note: when different periods are choosen in the
                                              "Published within how many month"-box, only those trials are considered that we were able to track for
                                              this period (e.g. 60 month since the completion date).
                                              Only exception is the category "Any duration (any follow-up period)", where all trials are considered.')
                                   ),
                                   wellPanel(
                                     h4("Different subgroups"),
                                     selectInput('tab_version', 'Study version*', c("Any", "IntoValue1", "IntoValue2")),
                                     selectInput('tab_registry', 'Registry', c("CT.gov + DRKS", "CT.gov", "DRKS")),
                                     selectInput('tab_status', 'Completion status', c("Any status", "Completed", "Terminated", "Unknown status", "Suspended")),
                                     selectInput('tab_sponsor', 'Responsibility level', c("Lead or Facility", "Lead trials only", "Facility trials only"), selected = "Lead trials only"),
                                     selectInput('tab_registered', 'Registration at which timepoint', c("Any", "In the month of the trial start or earlier", 
                                                                                                        "After the month of the trial start",
                                                                                                        "After trial completion (CD)", "After trial completion (PCD)",
                                                                                                        "After publication")),
                                     selectInput('tab_multicentric', 'Number of centers', c("Mono- or Multicentric", "Multicentric", "Monocentric")),
                                     selectInput('tab_industry', 'Sponsor from industry or not', c("Any", "Industry", "Academic")),
                                     selectInput('tab_phase', 'Trial phase', c("Any", "I", "I-II", "II", "II-III", "III", "IV", "Not given")),
                                     selectInput('tab_intervention', 'Type of intervention', c("Any", "Drug", "Device", "Other")),
                                     selectInput('tab_participants', 'Number of participants', c("Any", "<100", "100-500", ">500", "Not given")),
                                     selectInput('tab_complyear', 'Completion year', c("Any", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")),
                                     helpText('There are many choices on which group of clinical trials to consider. All of these choices
                                        alter the results for the final publication rates. Only the trials in our dataset that match the set filter criteria
                                                           are considered for the calculation of the timely reporting percentage.'),
                                     helpText('*There are 128 trials that are part of the samples used for both studies. 
                                              The reason for this is either a change in the completion date in the registry after the
                                              end of the IntoValue1 study or the change in the search criteria from primary completion date (IntoValue1)
                                              to completion date (IntoValue2). In those cases the results from the second study are shown
                                              when "Any" or "IntoValue2" is chosen. When "IntoValue1" is chosen, the results from the first study are shown.')
                                   ),
                                   wellPanel(
                                     h4(HTML(paste0("This Shiny app is hosted by"))),
                                     tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 122, width = 187),href="https://www.bihealth.org/en/quest-center")
                                   )
                            ),
                            column(9,
                                   DT::dataTableOutput("table")
                            )
                            
                          )
                 ),
                 tabPanel(HTML("Kaplan-Meier curve</a></li><li><a href=\"https://s-quest.bihealth.org/intovalue_de/\">Deutsch"),
                          h1("IntoValue - Institutions’ contribution to increasing value and reducing waste", align = "center"),
                          h4("Clinical trial dissemination rates of all German university medical centers", align = "center"),
                          br(),
                          h5(HTML(paste0("The following Shiny app accompanies the two publications 
                                                    on the clinical trial dissemination rates of all German 
                                                    university medical centers and allows to further explore the results.
                                                    The publications are ",
                                         a(href = 'https://www.biorxiv.org/content/10.1101/467746v2',
                                           "'Result dissemination from clinical trials conducted at German university 
                                                      medical centers was delayed and incomplete'"), " and ",
                                         a(href = 'https://osf.io/98j7u/',
                                           "'Results dissemination from completed clinical trials conducted at German 
                                                      university medical centers remains delayed and incomplete. The 2014-2017 cohort'"), 
                                         ". You can interactively choose different subsets of the data and different
                                                        ways of defining timely publication. The results can be displayed both as diagram or in a table
                                                        (see navigation bar at the top). Additionally, a Kaplan-Meier curve for the percentage of
                                                        unpublished studies over time can be displayed for different subgroups of the data as well
                                                        as different stratifying variables. The dataset, analysis code, as well as detailed methods
                                                        can be found on the OSF project pages for the ", a(href = 'https://osf.io/fh426/', "first"),
                                         " and the ", a(href = 'https://osf.io/98j7u/', "second"), " study and ", 
                                         a(href = 'https://github.com/quest-bih/IntoValue2', "this"), " Github repository. The combined dataset
                                                        for both studies can be found ", a(href = 'https://doi.org/10.5281/zenodo.5141343', "here"), 
                                         "."))),
                          br(),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     h4("General settings"),
                                     selectInput('cumPubl', 'Count summary results', c("Publications and Summary results", "Publications", "Summary results (CT.gov only)")),
                                     selectInput('cumComplDate', 'Count from which completion date', c("Completion date", "Primary completion date (CT.gov only)")),
                                     selectInput('cumRegistry', 'Registry', c("CT.gov + DRKS", "CT.gov", "DRKS")),
                                     selectInput('cumStatus', 'Completion status', c("Any status", "Completed", "Terminated", "Unknown status", "Suspended")),
                                     selectInput('cumSponsor', 'Responsibility level', c("Lead or Facility", "Lead trials only", "Facility trials only"), selected = "Lead trials only"),
                                     helpText('These options control the subgroups of the dataset which are used to calculate the Kaplan-Meier curve.
                                              The Kaplan-Meier curve takes into account that some trials only have a limited follow-up period that differs
                                              between trials (right-censored events). Each study is only used to estimate the Kaplan-Meier curve until the
                                              end of its follow-up period. The remainder of the curve is then estimated with the remaining studies with longer
                                              follow-up periods.')
                                   ),
                                   wellPanel(
                                     h4("Stratifying variable"),
                                     selectInput('cumDistCat', 'Category', c("Completion year", "Lead sponsor Industry vs. Academic",
                                                                             "Mono- vs. Multicentric", "Intervention types (CT.gov only)",
                                                                             "Number of participants")),
                                     helpText('Changes the stratifying variable for the Kaplan-Meier plot.')
                                   ),
                                   wellPanel(
                                     h4(HTML(paste0("This Shiny app is hosted by"))),
                                     tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 122, width = 187),href="https://www.bihealth.org/en/quest-center")
                                   )
                            ),
                            column(9,
                                   ggvisOutput('plot_KM')
                            )
                            
                          )
                 )
)


server <- function(input, output) {
  
  
  #--------------------------------------------------------------------------------------------------
  # Code for plotting
  #--------------------------------------------------------------------------------------------------
  
  vis <- reactive({
    
    plot_data <-  make_plot_data(IntoValue_app_table, input$publ, input$status, input$sponsor,
                                 input$complDate, input$timeframe, input$registered,
                                 input$multicentric, input$industry, input$phase,
                                 input$pubType, input$registry, input$intervention,
                                 input$participants, input$complyear, input$version)
    
    
    plot_data %>%
      ggvis(x=~city, y=~percent) %>% #, fill := "#5799c7", stroke := "#34729d") %>%
      layer_bars(fill :=~bar_col, stroke := "#34729d", fillOpacity := 0.5, fillOpacity.hover := 0.8,
                 key := ~tooltip, width = 0.8) %>%
      hide_legend("fill") %>%
      add_axis("x", title = "",
               properties = axis_props(
                 labels = list(angle = -90, align = "right", baseline = "middle", fontSize = 15))) %>%
      add_axis("y", title = "Percentage of published results", title_offset = 60,
               format = "%",
               properties = axis_props(
                 labels = list(fontSize = 14),
                 title = list(fontSize = 16))) %>%
      add_tooltip(function(data){
        as.character(data$tooltip)
      }, "hover") %>%
      set_options(width = 1000, height = 650) %>%
      scale_numeric("y", domain = c(0, 1))
  })
  
  vis %>% bind_shiny("plot1")
  
  
  #--------------------------------------------------------------------------------------------------
  # Code for Table
  #--------------------------------------------------------------------------------------------------
  
  output$table <- DT::renderDataTable({
    DT::datatable(make_table(IntoValue_app_table, input$tab_publ, input$tab_status, input$tab_sponsor,
                             input$tab_complDate, input$tab_timeframe, input$tab_registered,
                             input$tab_multicentric, input$tab_industry, input$tab_phase,
                             input$tab_pubType, input$tab_registry, input$tab_intervention,
                             input$tab_participants, input$tab_complyear, input$tab_version),
                  options = list(
                    pageLength = 37,
                    lengthMenu = list(c(37),
                                      c(37)))
    )
  })
  
  
  #--------------------------------------------------------------------------------------------------
  # Code for Kaplan-Meier curve
  #--------------------------------------------------------------------------------------------------
  
  cum_dist_plot <- reactive({
    
    registry_sub <- set_registry(input$cumRegistry)
    compl_status <- set_compl_status(input$cumStatus)
    sponsor_status <- set_sponsor_status(input$cumSponsor)
    
    #filter for registration timepoint & completion status and group by cities
    IntoValue_KM_data <- IntoValue_studies_only %>%
      filter(registry %in% registry_sub) %>%
      filter(recruitment_status %in% compl_status) %>%
      filter(lead_or_facility %in% sponsor_status)
    
    days_to_pub <- set_days_to_publ(input$cumPubl, input$cumComplDate, IntoValue_KM_data)
    days_obs <- set_days_observed(input$cumComplDate, IntoValue_KM_data)
    
    cum_cat <- input$cumDistCat
    
    if(cum_cat == "Completion year") {
      compl_years <- IntoValue_KM_data$completion_date %>% str_sub(1, 4)
      subsets_cum_plot <- list("Total" = rep(TRUE, dim(IntoValue_KM_data)[1]),
                               "2009" = compl_years == "2009",
                               "2010" = compl_years == "2010",
                               "2011" = compl_years == "2011",
                               "2012" = compl_years == "2012",
                               "2013" = compl_years == "2013",
                               "2014" = compl_years == "2014",
                               "2015" = compl_years == "2015",
                               "2016" = compl_years == "2016",
                               "2017" = compl_years == "2017")
    } else if(cum_cat == "Lead sponsor Industry vs. Academic") {
      subsets_cum_plot <- list("Total" = rep(TRUE, dim(IntoValue_KM_data)[1]),
                               "Industry" = IntoValue_KM_data$main_sponsor == "Industry",
                               "Academic" = IntoValue_KM_data$main_sponsor != "Industry")
    } else if(cum_cat == "Mono- vs. Multicentric") {
      subsets_cum_plot <- list("Total" = rep(TRUE, dim(IntoValue_KM_data)[1]),
                               "Monocentric" = IntoValue_KM_data$is_multicentric == "FALSE",
                               "Multicentric" = IntoValue_KM_data$is_multicentric == "TRUE")
    } else if(cum_cat == "Intervention types (CT.gov only)"){
      subsets_cum_plot <- list("Total" = IntoValue_KM_data$intervention_type != "Not given",
                               "Intervention_drug" = IntoValue_KM_data$intervention_type == "Drug",
                               "Intervention_device" = IntoValue_KM_data$intervention_type == "Device",
                               "Intervention_other" = !(IntoValue_KM_data$intervention_type %in% c("Drug", "Device", "Not given")))
    } else { #"Number of participants"
      subsets_cum_plot <- list("Any" =  !is.na(IntoValue_KM_data$enrollment),
                               "<100" = IntoValue_KM_data$enrollment < 100 & !is.na(IntoValue_KM_data$enrollment),
                               "100-500" = IntoValue_KM_data$enrollment >= 100 & IntoValue_KM_data$enrollment <= 500 & !is.na(IntoValue_KM_data$enrollment),
                               ">500" = IntoValue_KM_data$enrollment > 500 & !is.na(IntoValue_KM_data$enrollment))
    }
    
    
    year_range <- 1:6
    days <- 1:(365*last(year_range))
    
    #calculate cumulative distributions for subsets and make data tidy for plotting with ggplot2
    cum_dist_years <- sapply(subsets_cum_plot, get_KM_curve, days = days, days_to_publ = days_to_pub, days_to_cens = days_obs) %>%
      as_tibble() %>%
      add_column(days) %>%
      mutate(months = time_length(days(days), unit="months")) %>%
      tail(-1) #delete first datapoint as percentage starts at 1 (which we don't want, as some publ. can be found before day 1)
    
    cum_dist_years <- cum_dist_years %>%
      gather(colnames(cum_dist_years) %>% head(-2) , key = "category", value = "fraction")
    
    #delete last datapoints for the later years, as we couldn't follow up the trials
    #for more than 48 month for 2013 and 60 month for 2012
    #or 36 - 60 month for 2017-2015, respectively
    if(cum_cat == "Completion year") {
      cum_dist_years <- cum_dist_years %>%
        filter(!(category == "2013" & months > 48)) %>%
        filter(!(category == "2012" & months > 60)) %>%
        filter(!(category == "2017" & months > 36)) %>%
        filter(!(category == "2016" & months > 48)) %>%
        filter(!(category == "2015" & months > 60))
    }
    
    #cum_dist_years$tooltip = as.character(paste0("<b>", cum_dist_years$category, "</b><br>",
    #                                             "Day ", cum_dist_years$days, "<br>",
    #                                             round(cum_dist_years$fraction *100, 1), "% unpublished"))
    
    cum_dist_years %>%
      ggvis(x=~months, y=~fraction, stroke = ~category) %>%
      layer_lines(strokeWidth := 2) %>%
      #layer_points(size = 1, key := ~tooltip, opacity := 0) %>%
      add_axis("x", title = "Months", title_offset = 50,
               values = c(0, 12, 24, 36, 48, 60, 72),
               properties = axis_props(
                 labels = list(fontSize = 15),
                 title = list(fontSize = 16))) %>%
      add_axis("y", title = "Unpublished studies (%)", title_offset = 60,
               format = "%",
               properties = axis_props(
                 labels = list(fontSize = 14),
                 title = list(fontSize = 16))) %>%
      set_options(width = 1000, height = 650) %>%
      scale_numeric("y", domain = c(0, 1)) #%>%
    #add_tooltip(function(data){
    #  as.character(data$tooltip)
    #}, "hover")
    
    
  })
  
  cum_dist_plot %>% bind_shiny("plot_KM")
  
  #write(paste0("App visit at: ", Sys.time()), "/var/log/shiny-server/visitors_intovalue.txt", append = TRUE)
}

shinyApp(ui, server)

