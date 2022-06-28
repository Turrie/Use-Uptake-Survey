#-------------------------------------------------------------------------------
# Project: Akilimo Survey Analysis Dashboard
# Last Updated : September 2021
# Author : Fridah Wanjala (wanjala.n.fridah@gmail.com)
# About: Dashboard showing Akilimo tools usage and uptake analysis
#-------------------------------------------------------------------------------

#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/UseUptake3")


# Drop all objects from memory
#rm(list = ls())

#-------------------------------------------------------------------------------
# 0. House keeping
#-------------------------------------------------------------------------------
# ## Install packages
# if(!'tidyverse' %in% installed.packages()[, 'Package']) {install.packages('tidyverse', repos = 'http://cran.us.r-project.org')}
# if(!'MASS' %in% installed.packages()[, 'Package']) {install.packages('MASS', repos = 'http://cran.us.r-project.org')}
# if(!'webr' %in% installed.packages()[, 'Package']) {install.packages('webr', repos = 'http://cran.us.r-project.org')}
# if(!'knitr' %in% installed.packages()[, 'Package']) {install.packages('knitr', repos = 'http://cran.us.r-project.org')}
# if(!'epitab' %in% installed.packages()[, 'Package']) {install.packages('epitab', repos = 'http://cran.us.r-project.org')}
# if(!'scales' %in% installed.packages()[, 'Package']) {install.packages('scales', repos = 'http://cran.us.r-project.org')}
# if(!'gridExtra' %in% installed.packages()[, 'Package']) {install.packages('gridExtra', repos = 'http://cran.us.r-project.org')}
# if(!'glmnet' %in% installed.packages()[, 'Package']) {install.packages('glmnet', repos = 'http://cran.us.r-project.org')}
# if(!'lubridate' %in% installed.packages()[, 'Package']) {install.packages('lubridate', repos = 'http://cran.us.r-project.org')}
# if(!'readxl' %in% installed.packages()[, 'Package']) {install.packages('readxl', repos = 'http://cran.us.r-project.org')}
# if(!'glue' %in% installed.packages()[, 'Package']) {install.packages('glue', repos = 'http://cran.us.r-project.org')}
# if(!'ggiraph' %in% installed.packages()[, 'Package']) {install.packages('ggiraph', repos = 'http://cran.us.r-project.org')}
# if(!'moonBook' %in% installed.packages()[, 'Package']) {install.packages('moonBook', repos = 'http://cran.us.r-project.org')}
# if(!'ggiraphExtra' %in% installed.packages()[, 'Package']) {install.packages('ggiraphExtra', repos = 'http://cran.us.r-project.org')}
# if(!'kableExtra' %in% installed.packages()[, 'Package']) {install.packages('kableExtra', repos = 'http://cran.us.r-project.org')}
# if(!'hrbrthemes' %in% installed.packages()[, 'Package']) {install.packages('hrbrthemes', repos = 'http://cran.us.r-project.org')}
# if(!'GGally' %in% installed.packages()[, 'Package']) {install.packages('GGally', repos = 'http://cran.us.r-project.org')}
# if(!'viridis' %in% installed.packages()[, 'Package']) {install.packages('viridis', repos = 'http://cran.us.r-project.org')}
# if(!'plotly' %in% installed.packages()[, 'Package']) {install.packages('plotly', repos = 'http://cran.us.r-project.org')}
# if(!'shiny' %in% installed.packages()[, 'Package']) {install.packages('shiny', repos = 'http://cran.us.r-project.org')}
# if(!'shinydashboard' %in% installed.packages()[, 'Package']) {install.packages('shinydashboard', repos = 'http://cran.us.r-project.org')}
# if(!'stringr' %in% installed.packages()[, 'Package']) {install.packages('stringr', repos = 'http://cran.us.r-project.org')}
# if(!'treemap' %in% installed.packages()[, 'Package']) {install.packages('treemap', repos = 'http://cran.us.r-project.org')}
# if(!'DT' %in% installed.packages()[, 'Package']) {install.packages('DT', repos = 'http://cran.us.r-project.org')}
# if(!'shinyBS' %in% installed.packages()[, 'Package']) {install.packages('shinyBS', repos = 'http://cran.us.r-project.org')}
# if(!'shinyjs' %in% installed.packages()[, 'Package']) {install.packages('shinyjs', repos = 'http://cran.us.r-project.org')}
# if(!'magrittr' %in% installed.packages()[, 'Package']) {install.packages('magrittr', repos = 'http://cran.us.r-project.org')}
# if(!'shinycssloaders' %in% installed.packages()[, 'Package']) {install.packages('shinycssloaders', repos = 'http://cran.us.r-project.org')}
# if(!'timevis' %in% installed.packages()[, 'Package']) {install.packages('timevis', repos = 'http://cran.us.r-project.org')}
# if(!'comtradr' %in% installed.packages()[, 'Package']) {install.packages('comtradr', repos = 'http://cran.us.r-project.org')}
# if(!'memoise' %in% installed.packages()[, 'Package']) {install.packages('memoise', repos = 'http://cran.us.r-project.org')}
# if(!'promises' %in% installed.packages()[, 'Package']) {install.packages('promises', repos = 'http://cran.us.r-project.org')}
# if(!'future' %in% installed.packages()[, 'Package']) {install.packages('future', repos = 'http://cran.us.r-project.org')}
# if(!'rlang' %in% installed.packages()[, 'Package']) {install.packages('rlang', repos = 'http://cran.us.r-project.org')}
# if(!'tools' %in% installed.packages()[, 'Package']) {install.packages('tools', repos = 'http://cran.us.r-project.org')}
# if(!'shinyalert' %in% installed.packages()[, 'Package']) {install.packages('shinyalert', repos = 'http://cran.us.r-project.org')}
# if(!'plyr' %in% installed.packages()[, 'Package']) {install.packages('plyr', repos = 'http://cran.us.r-project.org')}
# if(!'reshape2' %in% installed.packages()[, 'Package']) {install.packages('reshape2', repos = 'http://cran.us.r-project.org')}
# if(!'mycor' %in% installed.packages()[, 'Package']) {install.packages('mycor', repos = 'http://cran.us.r-project.org')}
# if(!'ppcor' %in% installed.packages()[, 'Package']) {install.packages('ppcor', repos = 'http://cran.us.r-project.org')}
# if(!'mgcv' %in% installed.packages()[, 'Package']) {install.packages('mgcv', repos = 'http://cran.us.r-project.org')}
# if(!'sjlabelled' %in% installed.packages()[, 'Package']) {install.packages('sjlabelled', repos = 'http://cran.us.r-project.org')}
# if(!'sjmisc' %in% installed.packages()[, 'Package']) {install.packages('sjmisc', repos = 'http://cran.us.r-project.org')}
# if(!'grid' %in% installed.packages()[, 'Package']) {install.packages('grid', repos = 'http://cran.us.r-project.org')}
# if(!'purrr' %in% installed.packages()[, 'Package']) {install.packages('purrr', repos = 'http://cran.us.r-project.org')}
# if(!'RColorBrewer' %in% installed.packages()[, 'Package']) {install.packages('RColorBrewer', repos = 'http://cran.us.r-project.org')}
# if(!'caret' %in% installed.packages()[, 'Package']) {install.packages('caret', repos = 'http://cran.us.r-project.org')}
# if(!'randomForest' %in% installed.packages()[, 'Package']) {install.packages('randomForest', repos = 'http://cran.us.r-project.org')}
# if(!'lsr' %in% installed.packages()[, 'Package']) {install.packages('lsr', repos = 'http://cran.us.r-project.org')}
# if(!'scales' %in% installed.packages()[, 'Package']) {install.packages('scales', repos = 'http://cran.us.r-project.org')}




## Load the packages
library(shinyWidgets) 
suppressMessages(suppressWarnings(library(randomForest)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(caret)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(MASS)))
suppressMessages(suppressWarnings(library(webr)))
suppressMessages(suppressWarnings(library(knitr)))
suppressMessages(suppressWarnings(library(epitab)))
suppressMessages(suppressWarnings(library(scales)))
suppressMessages(suppressWarnings(library(gridExtra)))
suppressMessages(suppressWarnings(library(glmnet)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(ggiraph)))
suppressMessages(suppressWarnings(library(moonBook)))
suppressMessages(suppressWarnings(library(ggiraphExtra)))
suppressMessages(suppressWarnings(library(kableExtra)))
suppressMessages(suppressWarnings(library(hrbrthemes)))
suppressMessages(suppressWarnings(library(GGally)))
suppressMessages(suppressWarnings(library(viridis)))
suppressMessages(suppressWarnings(library(plotly)))
suppressMessages(suppressWarnings(library(highcharter)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(shinydashboard)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(treemap)))
suppressMessages(suppressWarnings(library(DT)))
suppressMessages(suppressWarnings(library(shinyBS)))
suppressMessages(suppressWarnings(library(shinyjs)))
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(shinycssloaders)))
suppressMessages(suppressWarnings(library(timevis)))
suppressMessages(suppressWarnings(library(comtradr)))
suppressMessages(suppressWarnings(library(memoise)))
suppressMessages(suppressWarnings(library(promises)))
suppressMessages(suppressWarnings(library(future)))
suppressMessages(suppressWarnings(library(rlang)))
suppressMessages(suppressWarnings(library(shinyalert)))
suppressMessages(suppressWarnings(library(reshape2)))
suppressMessages(suppressWarnings(library(mycor)))
suppressMessages(suppressWarnings(library(ppcor)))
suppressMessages(suppressWarnings(library(mgcv)))
suppressMessages(suppressWarnings(library(sjlabelled)))
suppressMessages(suppressWarnings(library(sjmisc)))
suppressMessages(suppressWarnings(library(grid)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(RColorBrewer)))
suppressMessages(suppressWarnings(library(lsr)))
suppressMessages(suppressWarnings(library(sf)))

library(dplyr, warn.conflicts = FALSE)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
#-------------------------------------------------------------------------------
# 1. Load data, analysis and cunctions scripts
#-------------------------------------------------------------------------------
## a. Script (AKILIMO_use_uptake.R) does the following
# i. Imports and cleans the parent (AKILIMO_use_uptake.csv) csv datasets doanloaded from ODK
# ii. Imports and cleans the child (AKILIMO_use_uptake-perceptions_repeat.csv) csv dataset imported from ODK
# iii. Imports the choices sheet of the ODK xls form
# iv. Imports xlsx file with list of perception statements
# v. Generates analysis variables from both datasets
# vi. Merges the two datasets (parent and child)
# vii. Loads the plot functions script (AKILIMO_use_uptake_functions.R)
suppressMessages(suppressWarnings(source("www/scripts/AKILIMO_use_uptake.R")))

## b. Script (PieDonutFunction.R) does the following
# i. Contains function to draw pie and donut chart
suppressMessages(suppressWarnings(source("www/scripts/PieDonutFunction.R")))

## c. Import dataset with descriptions of the most important use cases
most_imp_use_cases <- read_excel("www/data/supplementary/uptake_use_cases.xlsx", sheet = "cleaned") %>%
  mutate(description_final = glue("{new_description} ({newUseCaseCategory})"))

## Drop objects we no longer need
rm(list = setdiff(ls(), c("ds1", "ds2", "makeSmallPies", "makeSmallPies2GroupingVars", "ggPieDonutMine", 
                          "makeCandlePlots", "makeCandlePlotsGroup", "most_imp_use_cases")))

#-------------------------------------------------------------------------------
# 2. Objects to be used on the server side
#-------------------------------------------------------------------------------
## Total number of respondents
# Overall
total_n <- ds1 %>%
  count()

# By country
n_country <- ds1 %>%
  count(country) %>%
  mutate(Percent = n/sum(n))

# By country and gender
ds1 <- ds1[!is.na(ds1$genderHH), ]

n_gender_country <- ds1 %>%
  count(country, genderHH) %>%
  group_by(country) %>%
  mutate(Percent = n/sum(n)) %>%
  ungroup() %>%
  filter(grepl("female", genderHH, ignore.case = T)) 

## List of filters
countries_lst <- c(levels(ds1$country))
partners_lst <- c("All partners", levels(ds1$partner))
ng_partners_lst <- c("All partners", unique(ds1 %>% filter(grepl("nigeria", country, ignore.case = T)) %>% pull(partner) %>% as.character(.)))
tz_partners_lst <- c("All partners", unique(ds1 %>% filter(grepl("tanzania", country, ignore.case = T)) %>% pull(partner) %>% as.character(.)))
use_uptake_lst <- c("Use", "Uptake")

## Ploting theme
my_theme <- theme(axis.text.x = element_text(size = rel(1.0)),
                  axis.text.y = element_text(size = rel(1.0), hjust = 0.5),
                  axis.ticks.y = element_blank(),
                  axis.title = element_text(size = rel(1.0)),
                  axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
                  legend.text = element_text(size = rel(1.0)),
                  legend.title = element_blank(),
                  axis.line = element_blank(),
                  legend.position = "none",
                  strip.text.x = element_text(size = rel(1.1)),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  strip.background = element_blank(),
                  panel.spacing = unit(1, "lines")) 


## First and last survey date
first_survey_date <- min(ds1$SurveyStartDate) %>% as.Date()
latest_survey_date <- max(ds1$SurveyStartDate) %>% as.Date() %>% format(., "%b %d, %Y") 

## List of facetting anf grouping variables
# Fcetting variables
area_wealth <- c("Farm typology" = "farm_typology",
                 "Total area (acres)" = "totalArea_fct",
                 "Income from agriculture" = "propAgriculture_fct",
                 "Total area on cassava (acres)" = "cassavaTotalArea_fct",
                 "Income from cassava" = "propIncomeCassava_fct",
                 "Type of assets" = "asset",
                 "Number of assets" = "number_assets_fct",
                 "Types of positive changes" = "poschange",
                 "Number of positive changes" = "number_changes_fct")

interface_tool <- c("Number of tool formats" = "number_tool_formats_fct",
                    "Supporting materials" = "supporting_material",
                    "Number of supporting materials" = "number_supporting_material_fct",
                    "Personal support" = "personal_support_fct",
                    "Digital channels" = "digital_channel",
                    "Number of digital channels" = "fct_digital_channel")

facet_default <- c("Gender" = "genderHH",
                   "Event" = "event")


facet_all <- c("None" = "None",
               "Education" = "educationHH",
               "Age" = "ageHH",
               "Event" = "event",
               "Partner" = "partner",
               "Gender" = "genderHH")

compare_countries_grps <- c("Education" = "educationHH",
                            "Age" = "ageHH",
                            "Event" = "event",
                            "Use Case" = "useCase",
                            "Gender" = "genderHH")

facetting_vars <- facet_all
names(facetting_vars) <- NULL
facetting_vars <- facetting_vars[facetting_vars != "None"]

drivers_facets <- c("None" = "None", "Education" = "educationHH","Gender" = "genderHH")

# Grouping variables
subgroups_default <- c("Education" = "educationHH",
                       "Age" = "ageHH",
                       "Gender" = "genderHH",
                       "Use Case" = "useCase",
                       area_wealth,
                       interface_tool)

subgroups_all <- c("Education" = "educationHH",
                   "Age" = "ageHH",
                   "Event" = "event",
                   "Partner" = "partner",
                   "Gender" = "genderHH",
                   "Use Case" = "useCase",
                   area_wealth,
                   interface_tool,
                   "None" = "None")
# Filter variables to be treated differently
filter_vars <- c("country", "partner")

# Perceptions - main categories
perception_main_categories <- sort(c(unique(ds2$MainCategory)))

# Types of plots
plot_types <- c("Proportion bar", "Frequency bar", "Pie")

# List of use cases
use_case_types <- c("Fertilizer Recommendation" = "FR",
                    "Intercropping" = "IC",
                    "Planting Practices/Weed Management" = "PP_WM",
                    "Scheduled Planting/High Starch content" = "SP_HS"
)

## Plot colours for country
country_cols <- c("#00BA38", "#619CFF") # for the two countries
color <- colorRampPalette(c("red","white","green3"))(18)[c(4,12,15)] # for use/uptake levels
plot_cols <- c("#A020F0", "#000080", "#9A8822", "#FF1493", "#7CFC00", "#F8766D", "#8B3A62") # Drivers of use and uptake 

#-------------------------------------------------------------------------------
# 3. Transform datasetss to be used on the server side
#-------------------------------------------------------------------------------
## Dataset with most important use cases
# Clean transform this dataset for plotting
uptake_vars <- c(grep("FR[0-9]$", names(ds1), value = T, ignore.case = T),
                 grep("CIM[0-9]$", names(ds1), value = T, ignore.case = T),
                 grep("CIS[0-9]$", names(ds1), value = T, ignore.case = T),
                 grep("PP_WM[0-9]$", names(ds1), value = T, ignore.case = T),
                 grep("SP_HS[0-9]$", names(ds1), value = T, ignore.case = T))

# If useCase = "FR" and either of FR1 to FR6 is missing, repace them with 0
data_uptake_cleaned <- ds1 %>%
  dplyr::select(facetting_vars, country, useCase, uptake_vars) %>%
  mutate_at(vars(matches("FR")), function(x) ifelse(.$useCase == "FR" & is.na(x), 0, x)) %>%
  mutate_at(vars(matches("PP_WM")), function(x) ifelse(.$useCase == "PP_WM" & is.na(x), 0, x)) %>%
  mutate_at(vars(matches("SP_HS")), function(x) ifelse(.$useCase == "SP_HS" & is.na(x), 0, x)) %>%
  mutate_at(vars(matches("CIS")), function(x) ifelse(.$useCase == "IC" & is.na(x) & .$country == "Tanzania", 0, x)) %>%
  mutate_at(vars(matches("CIM")), function(x) ifelse(.$useCase == "IC" & is.na(x) & .$country == "Nigeria", 0, x)) %>%
  dplyr::select(-useCase) %>%
  mutate(IC1 = ifelse(country == "Tanzania", CIS1, CIM1),
         IC2 = ifelse(country == "Tanzania", CIS2, CIM2),
         IC3 = ifelse(country == "Tanzania", CIS3, CIM3),
         IC4 = ifelse(country == "Tanzania", CIS4, CIM4),
         IC5 = ifelse(country == "Tanzania", CIS5, CIM5),
         IC6 = ifelse(country == "Tanzania", CIS6, CIM6)) %>%
  dplyr::select(-starts_with("CIS"), -starts_with("CIM"))

# Step lables
var_labels <- setdiff(sort(names(data_uptake_cleaned)), c(facetting_vars, "country"))
new_var_labels <- gsub("FR|IC|PP_WM|SP_HS", "Step ", var_labels, ignore.case = T)

most_imp_use_cases <- most_imp_use_cases %>%
  mutate(Steps = gsub("FR|IC|PP_WM|SP_HS", "Step ", newUseCaseCategory, ignore.case = T)) %>%
  rename("Description" = "description_final")

data_uptake_full <- data_uptake_cleaned %>%
  gather(variable0, value, FR1:IC6) %>%
  filter(!is.na(value)) %>%
  mutate(
    variable = factor(variable0, levels = var_labels, labels = new_var_labels),
    value = factor(value, levels = c(0, 1, 2), labels = c("No", "Yes, but partially or modified", "Yes, strictly as recommended"))
  ) 

## Create temp table with descriptin of use/uptake levels for presentation as a legend
temp_df <- data.frame(Groups = c("Low", "Medium", "High"), 
                      DescriptionUse = c("No use of tools since exposure during dissemination event",
                                         "Tools used once or twice since exposure during dissemination event", 
                                         "Tools used at least 3 times since dissemination event"),
                      DescriptionUptake = c("Up to 1 out of 6 steps fully applied, or up to 2 steps partially applied in the farm", 
                                            "2-4 steps fully applied, or 3-6 out of 6 steps partially applied in the farm", 
                                            "At least 5 out of 6 steps fully applied in the farm")) %>%
  mutate(Percent = 0.05,
         Groups = factor(Groups, levels = c("Low", "Medium", "High"), labels = c("Low", "Medium", "High")),
         DescriptionUse = factor(DescriptionUse, levels = c("No use of tools since exposure during dissemination event",
                                                            "Tools used once or twice since exposure during dissemination event", 
                                                            "Tools used at least 3 times since dissemination event")),
         DescriptionUptake = factor(DescriptionUptake, levels = c("Up to 1 out of 6 steps fully applied, or up to 2 steps partially applied in the farm", 
                                                                  "2-4 steps fully applied, or 3-6 out of 6 steps partially applied in the farm", 
                                                                  "At least 5 out of 6 steps fully applied in the farm"))
  )


#-------------------------------------------------------------------------------
# 4. Drivers of use and uptake tab - pre-processing of analysis data
#-------------------------------------------------------------------------------
## List of statements
main_categories <- sort(unique(ds2$MainCategory))
main_categories_letters <- LETTERS[1:length(main_categories)]
drivers_facetting_vars <- c(facetting_vars, "useCase")
statements <- ds2 %>%
  select(category, statement, MainCategory) %>%
  mutate(MainCategoryLetter = plyr::mapvalues(MainCategory, 
                                              from = main_categories, 
                                              to = main_categories_letters)) %>%
  distinct(., .keep_all = TRUE) %>%
  arrange(MainCategory) %>%
  mutate(row_id = row_number(),
         statement = as.character(statement)) %>%
  arrange(row_id) %>%
  group_by(MainCategory) %>%
  arrange(row_id) %>%
  mutate(ids = row_number()) %>%
  ungroup() %>%
  mutate(statement_letter = glue("{MainCategoryLetter}{ids}") %>% as.factor(.))
statements$colors <- plyr::mapvalues(statements$MainCategory, from = perception_main_categories, to = plot_cols)

# Order the statements
statement_levels <- statements$row_id
statement_labels <- statements$statement

## Prepare data for modellling
data_drivers <- ds2 %>%
  select(KEY1, statement_nr, statement, response, category, use, uptake,
         MainCategory, statement_fct, country, partner, one_of(drivers_facetting_vars)) %>%
  mutate(response = as.character(response),
         response = case_when(response %in% c('Strongly disagree', 'Disagree') ~ "Disagree",
                              response %in% c('Strongly agree', 'Agree') ~ "Agree",
                              TRUE ~ response) %>%
           factor(., levels = c("Disagree", "Neither agree nor disagree", "Agree")))

data_drivers_wide <- data_drivers %>%
  select(KEY1, category, response, country, partner, one_of(drivers_facetting_vars), use, uptake) %>%
  spread(category, response)

# Vector of predictor variables
predictors <- unique(data_drivers$category)

# Random forest params
mtry <- floor(sqrt(length(predictors)))
trees <- 400

# Since we cannot add a legend with the main categories, I will create a temporary table and do a plot instead to show the colour codes for the main categories
temp_statements <- statements %>%
  select(MainCategory, colors) %>%
  distinct(., .keep_all = TRUE) %>%
  mutate(Percent = 0.05,
         MainCategory = trimws(MainCategory))

#-------------------------------------------------------------------------------
# End of Fridah script
#-------------------------------------------------------------------------------

#Turry starts here

main_ds <- read.csv("www/data/form_data/dissemination_events.csv")
names(main_ds)
main_ds2 <- subset(main_ds, select = c("country", "subscriberid", "LGA", "region", "state", "phonenumber", "X_geopoint_latitude",                        
                             "X_geopoint_longitude", "event", "partner","participantDetails.1..participant", "participantDetails.1..participant_male", "participantDetails.1..participant_female",
                             "participantDetails.2..participant", "participantDetails.2..participant_male","participantDetails.2..participant_female", "participantDetails.3..participant",            
                             "participantDetails.3..participant_male", "participantDetails.3..participant_female", "participantDetails.4..participant", "participantDetails.4..participant_male",       
                             "participantDetails.4..participant_female", "participantDetails.5..participant", "participantDetails.5..participant_male", "participantDetails.5..participant_female", 
                             "participantDetails.6..participant","participantDetails.6..participant_male", "participantDetails.6..participant_female", "participantDetails.7..participant",            
                             "participantDetails.7..participant_male","participantDetails.7..participant_female", "participantDetails.8..participant", "participantDetails.8..participant_male",       
                             "participantDetails.8..participant_female","participantDetails.9..participant","participantDetails.9..participant_male", "participantDetails.9..participant_female",
                             "useCase.FR", "useCase.IC","useCase.WM_PP", "useCase.SP_HS" 
                   ))

      colnames(main_ds2) <- c("country", "subscriberid", "LGA", "region", "HASC_1", "phonenumber", "X_geopoint_latitude", "X_geopoint_longitude", "event", "partner", 
                               "participantDetails1", "male1", "female1","participantDetails2", "male2","female2","participantDetails3", "male3", "female3","participantDetails4", 
                        "male4", "female4", "participantDetails5", "male5", "female5", "participantDetails6", "male6", "female6", "participantDetails7", "male7", "female7", 
                        "participantDetails8", "male8", "female8","participantDetails9", "male9","female9","useCase_FR", "useCase_IC", "useCase_WM_PP", "useCase_SP_HS")

main_ds3 <- as_tibble(main_ds2)
main_ds3[main_ds3 == "n/a"] <- NA
main_ds3$event <- ifelse(main_ds3$event == "media_event", "media", as.character(main_ds3$event))

#by total participants
tot_participant <- main_ds3 %>%
  dplyr::select(country, HASC_1,region, starts_with("male"), starts_with("female")) %>%
  gather(id, participant, starts_with("participantDetails"), na.rm = T) %>%
  gather(id, males, starts_with("male"), na.rm = T) %>%
  gather(id, females, starts_with("female"), na.rm = T) %>%
  dplyr::select(-id)

#tot_participant$event <- as.factor(tot_participant$event)
tot_participant$females <- as.integer(tot_participant$females)
tot_participant$males <- as.integer(tot_participant$males)
#tot_participant$partner <- as.factor(tot_participant$partner)

tot_participant$total_participants <- tot_participant$males + tot_participant$females

tot_participant <- tot_participant[!is.na(tot_participant$total_participants), ]

tot_participant$country <- ifelse(tot_participant$country == "TZ", "Tanzania",
                             ifelse(tot_participant$country == "NG", "Nigeria", as.character(tot_participant$country)))

#total participants in NG
tot_part_NG <- tot_participant[tot_participant$country == "Nigeria", ]
tot_part_NG2 <- tot_part_NG %>%
  group_by(HASC_1)%>%
  summarise(
    total =(sum(total_participants,na.rm=T ))
  )

#total participants in TZ
tot_part_TZ <- tot_participant[tot_participant$country == "Tanzania", ]
tot_part_TZ2 <- tot_part_TZ %>%
  group_by(region)%>%
  summarise(
    total =(sum(total_participants,na.rm=T ))
  )


#READ THE SHAPE FILES
NGmyDFW <- st_read("www/data/shape files/boundaryNG/gadm36_NGA_1.shp", stringsAsFactors = FALSE)

TanzmyDFW <- st_read("www/data/shape files/boundaryTZ/gadm36_TZA_1.shp", stringsAsFactors = FALSE)

TanzmyDFW3 <- st_read("www/data/shape files/boundaryTZ/tza_adm1.shp", stringsAsFactors = FALSE)


#MERGE SHP FILES WITH THE DATA
NGReg <- merge(NGmyDFW, unique(tot_part_NG2), by="HASC_1" )

TZReg <- merge(TanzmyDFW, unique(tot_part_TZ2), by.x ="HASC_1", by.y ="region" )

GEN_ds <- main_ds3 %>%
  dplyr::select(country, event, starts_with("male"), starts_with("female"), starts_with("useCase"), starts_with("participantDetails")) %>%
  gather(id, participant, starts_with("participantDetails"), na.rm = T) %>%
  gather(id, males, starts_with("male"), na.rm = T) %>%
  gather(id, females, starts_with("female"), na.rm = T) %>%
  gather(id, usecase, starts_with("useCase"), na.rm = T)%>%
  mutate(event = str_replace(event, "training_event", "Training event"),
         event = str_replace(event, "agric_show", "Agricultural show"),
         event = str_replace(event, "demonstration_field", "Demonstration field"),
         event = str_replace(event, "field_day", "Field day"),
         event = str_replace(event, "media", "Media Event"),
         event = str_replace(event, "video_show", "Video show"),
         event = str_replace(event, "sensitization_event", "Sensitization event"),
       ) %>%

mutate(id = str_replace(id, "useCase_FR", "FR"),
id = str_replace(id, "useCase_SP_HS", "SP"),
id = str_replace(id, "useCase_IC", "IC"),
id = str_replace(id, "useCase_WM_PP", "WM/PP"),
participant = str_replace(participant,"farmers","Farmers"),
participant = str_replace(participant,"Government_EAs","Government EAs"),
participant = str_replace(participant,"Government_organization_staff","Government Organization Staff"),
participant = str_replace(participant,"NGO_EAs","NGO EAs"),
participant = str_replace(participant,"NGO_staff","NGO staff"),
participant = str_replace(participant,"others","Others"),
participant = str_replace(participant,"private_EAs","Private EAs"),
participant = str_replace(participant,"private_organization","Private Organization"),
participant = str_replace(participant,"researchers","Researchers"))
  

#BY EVENT NG

event_ds_ng<- main_ds3 %>%
  dplyr::select(country, HASC_1, partner, event) 
event_ds_ng$event <- as.factor(event_ds_ng$event)
event_ds_ng$country <- ifelse(event_ds_ng$country == "TZ", "Tanzania",
                              ifelse(event_ds_ng$country == "NG", "Nigeria", as.character(event_ds_ng$country)))
event_ng <- event_ds_ng[event_ds_ng$country == "Nigeria", ]
event_ng2 <- event_ng %>% 
  mutate(event = str_replace(event, "training_event", "Training event"),
         event = str_replace(event, "agric_show", "Agricultural show"),
         event = str_replace(event, "demonstration_field", "Demonstration field"),
         event = str_replace(event, "field_day", "Field day"),
         event = str_replace(event, "media", "Media event"),
         event = str_replace(event, "video_show", "Video show"),
         event = str_replace(event, "sensitization_event", "Sensitization event"),
  )


#BY EVENT TZ
event_ds<- main_ds3 %>%
  dplyr::select(country, region, partner, event) 

event_ds$event <- as.factor(event_ds$event)
event_ds$country <- ifelse(event_ds$country == "TZ", "Tanzania",
                              ifelse(event_ds$country == "NG", "Nigeria", as.character(event_ds$country)))
event_tz <- event_ds[event_ds$country == "Tanzania", ]
colnames(event_tz)<- c("country", "HASC_1", "partner", "event")
event_tz2 <- event_tz %>% 
  mutate(event = str_replace(event, "training_event", "Training event"),
         event = str_replace(event, "agric_show", "Agricultural show"),
         event = str_replace(event, "demonstration_field", "Demonstration field"),
         event = str_replace(event, "field_day", "Field day"),
         event = str_replace(event, "media", "Media event"),
         event = str_replace(event, "video_show", "Video show"),
         event = str_replace(event, "sensitization_event", "Sensitization event"),
         )

#BAR GRAPHS

#total number by events and usecase

reach_use <- main_ds3 %>%
  dplyr::select(country, event, starts_with("useCase"),starts_with("male"), starts_with("female")) %>%
  #gather(id, participant, starts_with("participantDetails"), na.rm = T) %>%
  gather(gender, num, starts_with("male"), starts_with("female"), na.rm = T) %>%
  gather(id, usecase, starts_with("useCase"), na.rm = T)%>%
  mutate(event = str_replace(event, "training_event", "Training event"),
         event = str_replace(event, "agric_show", "Agricultural show"),
         event = str_replace(event, "demonstration_field", "Demonstration field"),
         event = str_replace(event, "field_day", "Field day"),
         event = str_replace(event, "media_event", "Media event"),
         event = str_replace(event, "video_show", "Video show"),
         event = str_replace(event, "sensitization_event", "Sensitization event"),
         id = str_replace(id, "useCase_FR", "FR"),
         id = str_replace(id, "useCase_SP_HS", "SP"),
         id = str_replace(id, "useCase_IC", "IC"),
         id = str_replace(id, "useCase_WM_PP", "WM/PP"),
  )


colnames(reach_use) <- c("country", "Event", "Gender", "num", "id", "Usecase")
reach_use2 <- reach_use[reach_use$Usecase == "True", ]
reach_use2$Usecase <- as.factor(reach_use2$Usecase)
reach_use2$Event <- as.factor(reach_use2$Event)
reach_use2$num <- as.numeric(reach_use2$num)
reach_use3 <- reach_use2 %>%
  dplyr::select(-Gender)%>%
                  group_by(country,id, num, Event)%>%
  
  summarize(
    total =(sum(num,na.rm=T ))
  )

reach_use3$country <- ifelse(reach_use3$country == "TZ", "Tanzania",
                           ifelse(reach_use3$country == "NG", "Nigeria", as.character(reach_use3$country)))
#TZ
reach_use_TZ <- reach_use3[reach_use3$country == "Tanzania", ]

#NG
reach_use_NG <- reach_use3[reach_use3$country == "Nigeria", ]

#BY usecase by participants

usept_ds <- GEN_ds[GEN_ds$usecase == "True", ]

usept_ds <- subset(usept_ds, select=-c(usecase))
usept_ds$females <- as.numeric(usept_ds$females)
usept_ds$males <- as.numeric(usept_ds$males)
usept_ds$total_participants <- usept_ds$males + usept_ds$females
usept_ds$participant <- as.factor(usept_ds$participant)

colnames(usept_ds) <- c("country", "Event", "Participant", "males", "females", "id","total_participants")

usept_ds$country <- ifelse(usept_ds$country == "TZ", "Tanzania",
                             ifelse(usept_ds$country == "NG", "Nigeria", as.character(usept_ds$country)))
#FOR NG
usept_ds_NG <- usept_ds[usept_ds$country=="Nigeria", ]
usept_ds2_NG <- usept_ds_NG %>%
  group_by(id, Participant)%>%
  dplyr::summarize(freq = dplyr::n()) %>% 
  mutate(per = freq/sum(freq))
usept_ds2_NG$per2 <- paste(round(usept_ds2_NG$per*100, digits = 0), "%", sep = "")

#FOR TZ
usept_ds_TZ <- usept_ds[usept_ds$country=="Tanzania", ]
usept_ds2_TZ <- usept_ds_TZ %>%
  group_by(id, Participant)%>%
  dplyr::summarize(freq = dplyr::n()) %>% 
  mutate(per = freq/sum(freq))
usept_ds2_TZ$per2 <- paste(round(usept_ds2_TZ$per*100, digits = 0), "%", sep = "")


#BY EVENT
eventpt_ds <- main_ds3 %>%
  dplyr::select(country, event, starts_with("participantDetails")) %>%
  gather(id, participant, starts_with("participantDetails")) %>%
  mutate(event = str_replace(event, "training_event", "Training event"),
         event = str_replace(event, "agric_show", "Agricultural show"),
         event = str_replace(event, "demonstration_field", "Demonstration field"),
         event = str_replace(event, "field_day", "Field day"),
         event = str_replace(event, "media", "Media event"),
         event = str_replace(event, "video_show", "Video show"),
         event = str_replace(event, "sensitization_event", "Sensitization event"),
  )

eventpt_ds$participant <- as.factor(eventpt_ds$participant)

colnames(eventpt_ds) <- c("country", "Event", "id","Participant")

eventpt_ds2 <- eventpt_ds %>%
mutate(Participant = str_replace(Participant,"farmers","Farmers"),
       Participant = str_replace(Participant,"Government_EAs","Government EAs"),
       Participant = str_replace(Participant,"Government_organization_staff","Government Organization Staff"),
       Participant = str_replace(Participant,"NGO_EAs","NGO EAs"),
       Participant = str_replace(Participant,"NGO_staff","NGO staff"),
       Participant = str_replace(Participant,"others","Others"),
       Participant = str_replace(Participant,"private_EAs","Private EAs"),
       Participant = str_replace(Participant,"private_organization","Private Organization"),
       Participant = str_replace(Participant,"researchers","Researchers"))

eventpt_ds2 <- eventpt_ds2[!is.na(eventpt_ds2$Participant), ]

eventpt_ds2$country <- ifelse(eventpt_ds2$country == "TZ", "Tanzania",
                           ifelse(eventpt_ds2$country == "NG", "Nigeria", as.character(eventpt_ds2$country)))
#FOR NG
eventpt_ds_NG <- eventpt_ds2[eventpt_ds2$country=="Nigeria", ]
eventpt_ds2_NG <- eventpt_ds_NG %>%
  group_by(Event, Participant)%>%
  dplyr::summarize(freq = dplyr::n()) %>% 
  mutate(per = freq/sum(freq))
eventpt_ds2_NG$per2 <- paste(round(eventpt_ds2_NG$per*100, digits = 0), "%", sep = "")

#FOR TZ
eventpt_ds_TZ <- eventpt_ds2[eventpt_ds2$country=="Tanzania", ]
eventpt_ds2_TZ <- eventpt_ds_TZ %>%
  group_by(Event, Participant)%>%
  dplyr::summarize(freq = dplyr::n()) %>% 
  mutate(per = freq/sum(freq))
eventpt_ds2_TZ$per2 <- paste(round(eventpt_ds2_TZ$per*100, digits = 0), "%", sep = "")


#by partner
reach_prt <- main_ds3 %>%
  dplyr::select(country, event, partner, starts_with("useCase"), starts_with("participantDetails")) %>%
  gather(id, participant, starts_with("participantDetails"), na.rm = T) %>%
  gather(id, males, starts_with("male"), na.rm = T) %>%
  gather(id, females, starts_with("female"), na.rm = T) %>% 
  gather(id, usecase, starts_with("useCase"), na.rm = T)%>%
  mutate(partner = str_replace(partner, "MA&RD ", "MA&RD"),
         partner = str_replace(partner, "Mtwara dc", "Mtwara DC"),
         partner = str_replace(partner, "Mtwara_District_Council", "Mtwara DC"),
         partner = str_replace(partner, "Newala district council", "Newala District Council"),
         partner = str_replace(partner, "Newala_District_Council", "Newala District Council"),
         partner = str_replace(partner, "Newala district council", "Newala District Council"),
         partner = str_replace(partner, "Newala dc" , "Newala District Council"),
         partner = str_replace(partner, "Newala Dc" , "Newala District Council"),
         partner = str_replace(partner, "Nachingwea dc", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea District", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea district", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea_District_Council", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea Dc", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea DC", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea District Council Council" , "Nachingwea District Council"),
         partner = str_replace(partner, "Kilimo Joint ", "KILIMO joint"),
         partner = str_replace(partner, "killimo joint", "KILIMO joint"),
         partner = str_replace(partner, "kilimo joint.", "KILIMO joint"),
         partner = str_replace(partner, "Kilimo Joint", "KILIMO joint"),
         partner = str_replace(partner, "Peace Corps benue", "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria Benue state" , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps", "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria Benue state command ." , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command command", "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command command .", "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command" , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace corps of Nigeria, Benue state command" , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command of Nigeria  Benue state command"  , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command of Nigeria  Benue state command command ." , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "MULTIFACET DYNAMIC GLOBAL VENTURES" , "Multifacet Dynamic Global Ventures Delta" ),
         partner = str_replace(partner, "FUNAAB " , "FUNAAB"),
         partner = str_replace(partner, "Sensitization event " , "Sensitization event"),
         partner = str_replace(partner, "Extention  Agent" , "Extension Agent"),
         partner = str_replace(partner, "Extension offier" , "Extension Agent"),
         partner = str_replace(partner, "Extension officer" , "Extension Agent"),
         partner = str_replace(partner, "Agriculture Extension Agent" , "Extension Agent"),
         partner = str_replace(partner, "Mtwara Dc"  , "Mtwara DC"),
         partner = str_replace(partner, "Mtwara Dc"  , "Mtwara_District_Council"),
         partner = str_replace(partner, "Handeni DC " , "Handeni DC"),
         partner = str_replace(partner, "Mtama_District_Council"   , "Mtama District council"),
         partner = str_replace(partner, "MTAMADC"  , "Mtama District council"),
         partner = str_replace(partner, "Mtama district council"  , "Mtama District council"),
         partner = str_replace(partner, "Mtama DC"  , "Mtama District council"),
         partner = str_replace(partner, "Mtama Dc"  , "Mtama District council"),
         partner = str_replace(partner, "MTAMA DC"  , "Mtama District council"),
         partner = str_replace(partner, "Mtama dc"  , "Mtama District council"),
         partner = str_replace(partner, "MtamaDc"  , "Mtama District council"),
         partner = str_replace(partner, "Nanguruwe village " , "Nanguruwe village" ),
         partner = str_replace(partner, "TYEGD "  , "TYEGD"),
         partner = str_replace(partner, "Tyegd" , "TYEGD"),
         partner = str_replace(partner, "Tygd" , "TYEGD"),
         partner = str_replace(partner, "Tyedg" , "TYEGD"),
         partner = str_replace(partner, "tuegd"  , "TYEGD"),
         partner = str_replace(partner, "tyegd"  , "TYEGD"),
         partner = str_replace(partner, "Handen dc"  , "Handeni DC"),
         partner = str_replace(partner, "BUNDA TC" , "BUNDA DC"),
         partner = str_replace(partner, "Handeni DC.", "Handeni DC"),
         partner = str_replace(partner, " Handeni DC.", "Handeni DC"),
         partner = str_replace(partner, "Handeni_District_Council" , "Handeni DC"),
         partner = str_replace(partner, "Handeni dc", "Handeni DC"),
         partner = str_replace(partner, "Handeni Dc", "Handeni DC"),
         partner = str_replace(partner, "Handeni DC " , "Handeni DC"),
         partner = str_replace(partner, "HANDENI DC", "Handeni DC"),
         partner = str_replace(partner, "Cedro Royal"  , "Cedro Royal Multiventures"),
         partner = str_replace(partner, "Chalinze District" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze dc" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze Dc" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze district council" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council Council" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze_District_Council" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council_District_Council"  , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council District Councilt"  , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council District Council"  , "Chalinze District Council"),
         partner = str_replace(partner, " Kolping society Tanzania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping society Tanzania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping society Tasmania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping  society of Tanzania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "KOLPING" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping society of Tanzania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Gemehaam Bees Limited " , "Gemehaam Bees Limited" ),
         partner = str_replace(partner, "Kolping Society of Tanzania " , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping Society Tanzania " , "Kolping Society of Tanzania"),
         partner = str_replace(partner, " Kolping Society Tanzania " , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping Society Tanzania"  , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Killing Society of Tanzania", "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Cia-ged" , "CIA-GED"),
         partner = str_replace(partner, "CAVA2/CIA-GED", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CIA_GED", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CAVAII/CIA_GED ", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CAVAII", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CAVA II/CAVA II/CIA_GED", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CAVA II/CIA_GED/CAVA II/CIA_GED ", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "Kilimo joint", "KILIMO joint"),
         partner = str_replace(partner, "Kilimo Joint", "KILIMO joint"),
         partner = str_replace(partner, "kilimo joint", "KILIMO joint"),
         partner = str_replace(partner, "Kilimo_Joint", "KILIMO joint"),
         partner = str_replace(partner, "Kilimojoint", "KILIMO joint"),
         partner = str_replace(partner, "KILIMO joint ", "KILIMO joint"),
         partner = str_replace(partner, " KILIMO joint", "KILIMO joint"),
         partner = str_replace(partner, "JDPC,Ibadan", "JDPC, Ibadan"),
         partner = str_replace(partner, "JDPC Ibadan" , "JDPC, Ibadan"),
         partner = str_replace(partner, "JDPCIbadan", "JDPC, Ibadan"),
         partner = str_replace(partner, "KILIMO JOINT", "KILIMO joint"),
         partner = str_replace(partner, "Chalinze District Council Councilt" , "Chalinze District Council"),
         partner = str_replace(partner, "chalinze district council" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council DC"  , "Chalinze District Council"),
         partner = str_replace(partner, "Village execative officer and farmers", "Village executive officer and farmers"),
         partner = str_replace(partner, "Village Excecative officer (veo),farmers", "Village executive officer and farmers"),
         partner = str_replace(partner, "Veo", "Village executive officer and farmers"),
         partner = str_replace(partner, "SOLADUKE AGRO ALLIED VENTURES " , "SOLADUKE AGRO ALLIED VENTURE"),
         partner = str_replace(partner, "SOLADUKE_AGRO_ALLIED_VENTURES" , "SOLADUKE AGRO ALLIED VENTURE"),
         partner = str_replace(partner, "Cedro_Royal" , "Cedro Royal Multiventures"),
         partner = str_replace(partner, "Cedro Royal Multiventures Multiventures", "Cedro Royal Multiventures"),
         partner = str_replace(partner, "Handeni District Council"  , "Handeni DC"),
         partner = str_replace(partner, "Handeni District Council"  , "Handeni DC"),
         partner = str_replace(partner, "Government extension agent" , "Extension Agent"),
         partner = str_replace(partner, "Government" , "Extension Agent"),
         partner = str_replace(partner, "IITA " , "IITA"),
         partner = str_replace(partner, "Cato foods " , "Cato Foods"),
         partner = str_replace(partner, "Cato foods" , "Cato Foods"),
         partner = str_replace(partner, "Bagamoyo District Council" , "Bagamoyo District Council"),
         partner = str_replace(partner, "Bagamoyo District" , "Bagamoyo District Council"),
         partner = str_replace(partner, "BAGAMOYO DISTRICT COUNCIL" , "Bagamoyo District Council"),
         partner = str_replace(partner, "Bagamoyo District Council Council" , "Bagamoyo District Council"),
         partner = str_replace(partner, "Bagamoyo_District_Council" , "Bagamoyo District Council"),
         partner = str_replace(partner, "OYSADEP " , "OYSADEP"),
         partner = str_replace(partner, "Justice, Development and Peace Makers' Centre" , "JDPMC"),
         partner = str_replace(partner, "JDPC, Ibadan" , "JDPMC"),
         partner = str_replace(partner, "JDPM" , "JDPMC"),
         partner = str_replace(partner, "JDPMCC" , "JDPMC"),
         partner = str_replace(partner, "Capafo"  , "CAPAFO"),
         partner = str_replace(partner, "CAPAFO"  , "CAPOFO"),
         partner = str_replace(partner, "SUFAN(JDPMC)Kwara" , "JDPMC"),
         partner = str_replace(partner, "Biharamulo_District_Council" , "Biharamulo District Council"),
         partner = str_replace(partner, "Biharamulo", "Biharamulo District Council"),
         partner = str_replace(partner, "BIHARAMULO", "Biharamulo District Council"),
         partner = str_replace(partner, "BIHARAMULO DISTRICT COUNCIL", "Biharamulo District Council"),
         partner = str_replace(partner, "Biharamulo District Council District Council", "Biharamulo District Council"),
         partner = str_replace(partner, "Musoma_District_Council", "Musoma District Council"),
         partner = str_replace(partner, "Musoma District Council ", "Musoma District Council"),
         partner = str_replace(partner, "Ogadep", "OGADEP"),
         partner = str_replace(partner, "Asadep", "OYSADEP"),
         partner = str_replace(partner, "OSSADEP ", "OYSADEP"),
         partner = str_replace(partner, "Perfect_Impact", "Perfect Impact"),
         partner = str_replace(partner, "SARO", "SARO AGROSCIENCES"),
         partner = str_replace(partner, "SARO AGROSCIENCES AGROSCIENCES", "SARO AGROSCIENCES"),
         partner = str_replace(partner, "SOLADUKE AGRO ALLIED VENTURES", "SOLADUKE AGRO ALLIED VENTURE"),
         partner = str_replace(partner, "Lindi dc", "Lindi District council"),
         partner = str_replace(partner, "Lindi district council", "Lindi District council"),
         partner = str_replace(partner, "Biharamulo District Council DISTRICT COUNCIL", "Biharamulo District Council"),
         partner = str_replace(partner, "Tanzania youth espouse for gender and development (TYEGD)", "TYEGD"),
         partner = str_replace(partner, "Zanzibar Agricultural Research Institute", "ZARI"),
         partner = str_replace(partner, "Denkosin agric concept"   , "DENKOSIN AGRIC CONCEPT"),
         partner = str_replace(partner, "Village Excecative officer (veo),farmers", "Village executive officer and farmers"),
         partner = str_replace(partner, "Justice, Development and Peace Makers Center, Osogbo" , "JDPMC"),
         partner = str_replace(partner, "JDPC, Ibadan" , "JDPMC"),
         partner = str_replace(partner, "Justice, Development and Peace Makers Center (JDPMC), Osogbo" , "JDPMC"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command command" , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command command ." , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Handeni DC.", "Handeni DC"),
         partner = str_replace(partner, "Newala District Council ", "Newala District Council"),
         partner = str_replace(partner, "Newala District council", "Newala District Council"),
         partner = str_replace(partner, "CIA-GED", "CAVA II/CIA_GED")
       
          )


reach_prt$partner <- ifelse(reach_prt$partner=="Village Excecative officer (veo),farmers", "Village executive officer and farmers", as.character(reach_prt$partner))

reach_prt$partner <- ifelse(reach_prt$partner=="Justice, Development and Peace Makers Center (JDPMC), Osogbo" , "JDPMC", as.character(reach_prt$partner))

reach_prt$partner <- ifelse(reach_prt$partner=="Peace Corps of Nigeria  Benue state command .", "Peace Corps of Nigeria  Benue state command", as.character(reach_prt$partner))

reach_prt$partner <- ifelse(reach_prt$partner=="Initiative for the Empowerment of Vulnerable Persons in the Society (IEVPS)", "IEVPS", as.character(reach_prt$partner))

reach_prt$partner <- ifelse(reach_prt$partner=="Tanzania youth espouse for gender and development (TYEGD)", "TYEGD", as.character(reach_prt$partner))

reach_prt2 <- reach_prt[reach_prt$usecase == "True", ]
reach_prt2$usecase <- as.factor(reach_prt2$usecase)
reach_prt2$partner <- as.factor(reach_prt2$partner)

colnames(reach_prt2) <- c( "country", "Event", "Partner", "Participant", "id", "Usecase")

reach_prt3 <- reach_prt2 %>%
mutate(Participant = str_replace(Participant,"farmers","Farmers"),
Participant = str_replace(Participant,"Government_EAs","Government EAs"),
Participant = str_replace(Participant,"Government_organization_staff","Government Organization Staff"),
Participant = str_replace(Participant,"NGO_EAs","NGO EAs"),
Participant = str_replace(Participant,"NGO_staff","NGO staff"),
Participant = str_replace(Participant,"others","Others"),
Participant = str_replace(Participant,"private_EAs","Private EAs"),
Participant = str_replace(Participant,"private_organization","Private Organization"),
Participant = str_replace(Participant,"researchers","Researchers"))
  
reach_prt3$country <- ifelse(reach_prt3$country == "TZ", "Tanzania",
                             ifelse(reach_prt3$country == "NG", "Nigeria", as.character(reach_prt3$country)))


#FOR NG
reach_prt_ng <- reach_prt3[reach_prt3$country=="Nigeria", ]
reach_prt2_ng <- reach_prt_ng %>%
  group_by(Partner, Participant)%>%
  dplyr::summarize(freq = dplyr::n()) %>% 
  mutate(per = freq/sum(freq))

#FOR TZ
reach_prt_tz <- reach_prt3[reach_prt3$country=="Tanzania", ]
reach_prt2_tz <- reach_prt_tz %>%
  group_by(Partner, Participant)%>%
  dplyr::summarize(freq = dplyr::n()) %>% 
  mutate(per = freq/sum(freq))

#BY GENDER

gender_ds <- main_ds3 %>%
  dplyr::select(country, event, starts_with("male"), starts_with("female")) %>%
  gather(Gender, num, starts_with("male"), starts_with("female"), na.rm = T) %>%
   #gather(id, usecase, starts_with("useCase"), na.rm = T)%>%
  mutate(event = str_replace(event, "training_event", "Training event"),
         event = str_replace(event, "agric_show", "Agricultural show"),
         event = str_replace(event, "demonstration_field", "Demonstration field"),
         event = str_replace(event, "field_day", "Field day"),
         event = str_replace(event, "media", "Media event"),
         event = str_replace(event, "video_show", "Video show"),
         event = str_replace(event, "sensitization_event", "Sensitization event"))

gender_ds$Gender <- gsub("^male\\w+", "Male", gender_ds$Gender)
gender_ds$Gender <- gsub("^female\\w+", "Female", gender_ds$Gender)
#gen_ds <- unique(gen_ds$id)

gender_ds$event <- as.factor(gender_ds$event)
gender_ds$num <- as.numeric(gender_ds$num)

gender_ds2 <- gender_ds%>%
group_by(country, Gender, event)%>%
  summarise(
    total =(sum(num,na.rm=T ))
  )

gender_ds2$country <- ifelse(gender_ds2$country == "TZ", "Tanzania",
                             ifelse(gender_ds2$country == "NG", "Nigeria", as.character(gender_ds2$country)))
gender_ds_NG <- gender_ds2[gender_ds2$country == "Nigeria", ]
gender_ds_TZ <- gender_ds2[gender_ds2$country == "Tanzania", ]
events <- unique(gender_ds$event)
events_NG <- unique(gender_ds_NG$event)
events_TZ <- unique(gender_ds_TZ$event)

#total attendees by gender
gen_att <- main_ds3 %>%
  dplyr::select(country, starts_with("male"), starts_with("female"))%>%
  gather(Gender, num, starts_with("male"), starts_with("female"), na.rm = T) 

gen_att$Gender <- gsub("^male\\w+", "Male", gen_att$Gender)
gen_att$Gender <- gsub("^female\\w+", "Female", gen_att$Gender)

#attendees
total_attend <- main_ds3 %>%
  dplyr::select(country, event,  starts_with("useCase"), starts_with("male"), starts_with("female")) %>%
  gather(Gender, num, starts_with("male"), starts_with("female"), na.rm = T) %>%
  gather(id, usecase, starts_with("useCase"), na.rm = T)%>%
  mutate(event = str_replace(event, "training_event", "Training event"),
         event = str_replace(event, "agric_show", "Agricultural show"),
         event = str_replace(event, "demonstration_field", "Demonstration field"),
         event = str_replace(event, "field_day", "Field day"),
         event = str_replace(event, "media", "Media event"),
         event = str_replace(event, "video_show", "Video show"),
         event = str_replace(event, "sensitization_event", "Sensitization event"),
            id = str_replace(id, "useCase_FR", "FR"),
                id = str_replace(id, "useCase_SP_HS", "SP"),
                id = str_replace(id, "useCase_IC", "IC"),
                id = str_replace(id, "useCase_WM_PP", "WM/PP"))


total_attend$Gender <- gsub("^male\\w+", "Male", total_attend$Gender)
total_attend$Gender <- gsub("^female\\w+", "Female", total_attend$Gender)
#gen_ds <- unique(gen_ds$id)

total_attend$event <- as.factor(total_attend$event)
total_attend$id <- as.factor(total_attend$id)
total_attend$Gender <- as.factor(total_attend$Gender)

total_attend$num <- as.numeric(total_attend$num)

total_attend2 <- total_attend%>%
  group_by(country, id, Gender, event)%>%
  summarise(
    total =(sum(num,na.rm=T ))
  )

total_attend2$country <- ifelse(total_attend2$country == "TZ", "Tanzania",
                             ifelse(total_attend2$country == "NG", "Nigeria", as.character(total_attend2$country)))
total_event_ng <- total_attend2[total_attend2$country == "Nigeria", ]
total_event_tz <- total_attend2[total_attend2$country == "Tanzania", ]

#attendees by usecase
attend_use <- main_ds3 %>%
  dplyr::select(country, starts_with("useCase"), starts_with("male"), starts_with("female")) %>%
  gather(Gender, num, starts_with("male"), starts_with("female"), na.rm = T) %>%
  gather(usecase, id, starts_with("useCase"), na.rm = T)%>%
  mutate(
         usecase = str_replace(usecase, "useCase_FR", "FR"),
         usecase = str_replace(usecase, "useCase_SP_HS", "SP"),
         usecase = str_replace(usecase, "useCase_IC", "IC"),
         usecase = str_replace(usecase, "useCase_WM_PP", "WM/PP"))

attend_use2 <- attend_use[attend_use$id == "True", ]
attend_use2$Gender <- gsub("^male\\w+", "Male", attend_use2$Gender)
attend_use2$Gender <- gsub("^female\\w+", "Female", attend_use2$Gender)
#gen_ds <- unique(gen_ds$usecase)

attend_use2$usecase <- as.factor(attend_use2$usecase)
attend_use2$Gender <- as.factor(attend_use2$Gender)

attend_use2$num <- as.numeric(attend_use2$num)

attend_use3 <- attend_use2 %>%
  select(country, num,usecase)%>%
  group_by(country,  usecase)%>%
  summarise(
    total =(sum(num,na.rm=T ))
  )

attend_use3$country <- ifelse(attend_use3$country == "TZ", "Tanzania",
                                ifelse(attend_use3$country == "NG", "Nigeria", as.character(attend_use3$country)))
attend_use_ng <- attend_use3[attend_use3$country == "Nigeria", ]
attend_use_tz <- attend_use3[attend_use3$country == "Tanzania", ]

#by events and number
reach_graph <- main_ds3 %>%
  dplyr::select(country, event, starts_with("male"), starts_with("female")) %>%
  gather(Gender, num, starts_with("male"), starts_with("female"), na.rm = T) %>%
  mutate(event = str_replace(event, "training_event", "Training event"),
         event = str_replace(event, "agric_show", "Agricultural show"),
         event = str_replace(event, "demonstration_field", "Demonstration field"),
         event = str_replace(event, "field_day", "Field day"),
         event = str_replace(event, "media", "Media event"),
         event = str_replace(event, "video_show", "Video show"),
         event = str_replace(event, "sensitization_event", "Sensitization event"),
  )

reach_graph$Gender <- gsub("^male\\w+", "Male", reach_graph$Gender)
reach_graph$Gender <- gsub("^female\\w+", "Female", reach_graph$Gender)
#gen_ds <- unique(gen_ds$id)

reach_graph$Gender <- as.factor(reach_graph$Gender)
reach_graph$num <- as.numeric(reach_graph$num)

reach_graph2 <- reach_graph %>%
  select(country, num,event)%>%
  group_by(country, event)%>%
  summarise(
    total =(sum(num,na.rm=T ))
  )

reach_graph2$country <- ifelse(reach_graph2$country == "TZ", "Tanzania",
                              ifelse(reach_graph2$country == "NG", "Nigeria", as.character(reach_graph2$country)))
#NG
reach_NG <- reach_graph2[reach_graph2$country == "Nigeria", ]

#TZ
reach_TZ <- reach_graph2[reach_graph2$country == "Tanzania", ]

#total attendees by participants
#by partner
parti_attendees <- main_ds3 %>%
  dplyr::select(country, partner,  starts_with("male"), starts_with("female")) %>%
 # gather(id, attendees, starts_with("participantDetails"), na.rm = T) %>%
 
  gather(Gender, num, starts_with("male"), starts_with("female"), na.rm = T) %>% 
  
  mutate(partner = str_replace(partner, "MA&RD ", "MA&RD"),
         partner = str_replace(partner, "Mtwara dc", "Mtwara DC"),
         partner = str_replace(partner, "Mtwara_District_Council", "Mtwara DC"),
         partner = str_replace(partner, "Newala district council", "Newala District Council"),
         partner = str_replace(partner, "Newala_District_Council", "Newala District Council"),
         partner = str_replace(partner, "Newala district council", "Newala District Council"),
         partner = str_replace(partner, "Newala dc" , "Newala District Council"),
         partner = str_replace(partner, "Newala Dc" , "Newala District Council"),
         partner = str_replace(partner, "Nachingwea dc", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea District", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea district", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea_District_Council", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea Dc", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea DC", "Nachingwea District Council"),
         partner = str_replace(partner, "Nachingwea District Council Council" , "Nachingwea District Council"),
         partner = str_replace(partner, "Kilimo Joint ", "KILIMO joint"),
         partner = str_replace(partner, "killimo joint", "KILIMO joint"),
         partner = str_replace(partner, "kilimo joint.", "KILIMO joint"),
         partner = str_replace(partner, "Kilimo Joint", "KILIMO joint"),
         partner = str_replace(partner, "Peace Corps benue", "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria Benue state" , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps", "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria Benue state command ." , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command command", "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command command .", "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command" , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace corps of Nigeria, Benue state command" , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command of Nigeria  Benue state command"  , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command of Nigeria  Benue state command command ." , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "MULTIFACET DYNAMIC GLOBAL VENTURES" , "Multifacet Dynamic Global Ventures Delta" ),
         partner = str_replace(partner, "FUNAAB " , "FUNAAB"),
         partner = str_replace(partner, "Sensitization event " , "Sensitization event"),
         partner = str_replace(partner, "Extention  Agent" , "Extension Agent"),
         partner = str_replace(partner, "Extension offier" , "Extension Agent"),
         partner = str_replace(partner, "Extension officer" , "Extension Agent"),
         partner = str_replace(partner, "Agriculture Extension Agent" , "Extension Agent"),
         partner = str_replace(partner, "Mtwara Dc"  , "Mtwara DC"),
         partner = str_replace(partner, "Mtwara Dc"  , "Mtwara_District_Council"),
         partner = str_replace(partner, "Handeni DC " , "Handeni DC"),
         partner = str_replace(partner, "Mtama_District_Council"   , "Mtama District council"),
         partner = str_replace(partner, "MTAMADC"  , "Mtama District council"),
         partner = str_replace(partner, "Mtama district council"  , "Mtama District council"),
         partner = str_replace(partner, "Mtama DC"  , "Mtama District council"),
         partner = str_replace(partner, "Mtama Dc"  , "Mtama District council"),
         partner = str_replace(partner, "MTAMA DC"  , "Mtama District council"),
         partner = str_replace(partner, "Mtama dc"  , "Mtama District council"),
         partner = str_replace(partner, "MtamaDc"  , "Mtama District council"),
         partner = str_replace(partner, "Nanguruwe village " , "Nanguruwe village" ),
         partner = str_replace(partner, "TYEGD "  , "TYEGD"),
         partner = str_replace(partner, "Tyegd" , "TYEGD"),
         partner = str_replace(partner, "Tygd" , "TYEGD"),
         partner = str_replace(partner, "Tyedg" , "TYEGD"),
         partner = str_replace(partner, "tuegd"  , "TYEGD"),
         partner = str_replace(partner, "tyegd"  , "TYEGD"),
         partner = str_replace(partner, "Handen dc"  , "Handeni DC"),
         partner = str_replace(partner, "BUNDA TC" , "BUNDA DC"),
         partner = str_replace(partner, "Handeni DC.", "Handeni DC"),
         partner = str_replace(partner, " Handeni DC.", "Handeni DC"),
         partner = str_replace(partner, "Handeni_District_Council" , "Handeni DC"),
         partner = str_replace(partner, "Handeni dc", "Handeni DC"),
         partner = str_replace(partner, "Handeni Dc", "Handeni DC"),
         partner = str_replace(partner, "Handeni DC " , "Handeni DC"),
         partner = str_replace(partner, "HANDENI DC", "Handeni DC"),
         partner = str_replace(partner, "Cedro Royal"  , "Cedro Royal Multiventures"),
         partner = str_replace(partner, "Chalinze District" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze dc" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze Dc" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze district council" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council Council" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze_District_Council" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council_District_Council"  , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council District Councilt"  , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council District Council"  , "Chalinze District Council"),
         partner = str_replace(partner, " Kolping society Tanzania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping society Tanzania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping society Tasmania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping  society of Tanzania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "KOLPING" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping society of Tanzania" , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Gemehaam Bees Limited " , "Gemehaam Bees Limited" ),
         partner = str_replace(partner, "Kolping Society of Tanzania " , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping Society Tanzania " , "Kolping Society of Tanzania"),
         partner = str_replace(partner, " Kolping Society Tanzania " , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Kolping Society Tanzania"  , "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Killing Society of Tanzania", "Kolping Society of Tanzania"),
         partner = str_replace(partner, "Cia-ged" , "CIA-GED"),
         partner = str_replace(partner, "CAVA2/CIA-GED", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CIA_GED", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CAVAII/CIA_GED ", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CAVAII", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CAVA II/CAVA II/CIA_GED", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "CAVA II/CIA_GED/CAVA II/CIA_GED ", "CAVA II/CIA_GED"),
         partner = str_replace(partner, "Kilimo joint", "KILIMO joint"),
         partner = str_replace(partner, "Kilimo Joint", "KILIMO joint"),
         partner = str_replace(partner, "kilimo joint", "KILIMO joint"),
         partner = str_replace(partner, "Kilimo_Joint", "KILIMO joint"),
         partner = str_replace(partner, "Kilimojoint", "KILIMO joint"),
         partner = str_replace(partner, "KILIMO joint ", "KILIMO joint"),
         partner = str_replace(partner, " KILIMO joint", "KILIMO joint"),
         partner = str_replace(partner, "JDPC,Ibadan", "JDPC, Ibadan"),
         partner = str_replace(partner, "JDPC Ibadan" , "JDPC, Ibadan"),
         partner = str_replace(partner, "JDPCIbadan", "JDPC, Ibadan"),
         partner = str_replace(partner, "KILIMO JOINT", "KILIMO joint"),
         partner = str_replace(partner, "Chalinze District Council Councilt" , "Chalinze District Council"),
         partner = str_replace(partner, "chalinze district council" , "Chalinze District Council"),
         partner = str_replace(partner, "Chalinze District Council DC"  , "Chalinze District Council"),
         partner = str_replace(partner, "Village execative officer and farmers", "Village executive officer and farmers"),
         partner = str_replace(partner, "Village Excecative officer (veo),farmers", "Village executive officer and farmers"),
         partner = str_replace(partner, "Veo", "Village executive officer and farmers"),
         partner = str_replace(partner, "SOLADUKE AGRO ALLIED VENTURES " , "SOLADUKE AGRO ALLIED VENTURE"),
         partner = str_replace(partner, "SOLADUKE_AGRO_ALLIED_VENTURES" , "SOLADUKE AGRO ALLIED VENTURE"),
         partner = str_replace(partner, "Cedro_Royal" , "Cedro Royal Multiventures"),
         partner = str_replace(partner, "Cedro Royal Multiventures Multiventures", "Cedro Royal Multiventures"),
         partner = str_replace(partner, "Handeni District Council"  , "Handeni DC"),
         partner = str_replace(partner, "Handeni District Council"  , "Handeni DC"),
         partner = str_replace(partner, "Government extension agent" , "Extension Agent"),
         partner = str_replace(partner, "Government" , "Extension Agent"),
         partner = str_replace(partner, "IITA " , "IITA"),
         partner = str_replace(partner, "Cato foods " , "Cato Foods"),
         partner = str_replace(partner, "Cato foods" , "Cato Foods"),
         partner = str_replace(partner, "Bagamoyo District Council" , "Bagamoyo District Council"),
         partner = str_replace(partner, "Bagamoyo District" , "Bagamoyo District Council"),
         partner = str_replace(partner, "BAGAMOYO DISTRICT COUNCIL" , "Bagamoyo District Council"),
         partner = str_replace(partner, "Bagamoyo District Council Council" , "Bagamoyo District Council"),
         partner = str_replace(partner, "Bagamoyo_District_Council" , "Bagamoyo District Council"),
         partner = str_replace(partner, "OYSADEP " , "OYSADEP"),
         partner = str_replace(partner, "Justice, Development and Peace Makers' Centre" , "JDPMC"),
         partner = str_replace(partner, "JDPC, Ibadan" , "JDPMC"),
         partner = str_replace(partner, "JDPM" , "JDPMC"),
         partner = str_replace(partner, "JDPMCC" , "JDPMC"),
         partner = str_replace(partner, "Capafo"  , "CAPAFO"),
         partner = str_replace(partner, "CAPAFO"  , "CAPOFO"),
         partner = str_replace(partner, "SUFAN(JDPMC)Kwara" , "JDPMC"),
         partner = str_replace(partner, "Biharamulo_District_Council" , "Biharamulo District Council"),
         partner = str_replace(partner, "Biharamulo", "Biharamulo District Council"),
         partner = str_replace(partner, "BIHARAMULO", "Biharamulo District Council"),
         partner = str_replace(partner, "BIHARAMULO DISTRICT COUNCIL", "Biharamulo District Council"),
         partner = str_replace(partner, "Biharamulo District Council District Council", "Biharamulo District Council"),
         partner = str_replace(partner, "Musoma_District_Council", "Musoma District Council"),
         partner = str_replace(partner, "Musoma District Council ", "Musoma District Council"),
         partner = str_replace(partner, "Ogadep", "OGADEP"),
         partner = str_replace(partner, "Asadep", "OYSADEP"),
         partner = str_replace(partner, "OSSADEP ", "OYSADEP"),
         partner = str_replace(partner, "Perfect_Impact", "Perfect Impact"),
         partner = str_replace(partner, "SARO", "SARO AGROSCIENCES"),
         partner = str_replace(partner, "SARO AGROSCIENCES AGROSCIENCES", "SARO AGROSCIENCES"),
         partner = str_replace(partner, "SOLADUKE AGRO ALLIED VENTURES", "SOLADUKE AGRO ALLIED VENTURE"),
         partner = str_replace(partner, "Lindi dc", "Lindi District council"),
         partner = str_replace(partner, "Lindi district council", "Lindi District council"),
         partner = str_replace(partner, "Biharamulo District Council DISTRICT COUNCIL", "Biharamulo District Council"),
         partner = str_replace(partner, "Tanzania youth espouse for gender and development (TYEGD)", "TYEGD"),
         partner = str_replace(partner, "Zanzibar Agricultural Research Institute", "ZARI"),
         partner = str_replace(partner, "Denkosin agric concept"   , "DENKOSIN AGRIC CONCEPT"),
         partner = str_replace(partner, "Village Excecative officer (veo),farmers", "Village executive officer and farmers"),
         partner = str_replace(partner, "Justice, Development and Peace Makers Center, Osogbo" , "JDPMC"),
         partner = str_replace(partner, "JDPC, Ibadan" , "JDPMC"),
         partner = str_replace(partner, "Justice, Development and Peace Makers Center (JDPMC), Osogbo" , "JDPMC"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command command" , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Peace Corps of Nigeria  Benue state command command ." , "Peace Corps of Nigeria  Benue state command"),
         partner = str_replace(partner, "Handeni DC.", "Handeni DC"),
         partner = str_replace(partner, "Newala District Council ", "Newala District Council"),
         partner = str_replace(partner, "Newala District council", "Newala District Council"),
         partner = str_replace(partner, "CIA-GED", "CAVA II/CIA_GED")
         
  )


parti_attendees$partner <- ifelse(parti_attendees$partner=="Village Excecative officer (veo),farmers", "Village executive officer and farmers", as.character(parti_attendees$partner))

parti_attendees$partner <- ifelse(parti_attendees$partner=="Justice, Development and Peace Makers Center (JDPMC), Osogbo" , "JDPMC", as.character(parti_attendees$partner))

parti_attendees$partner <- ifelse(parti_attendees$partner=="Peace Corps of Nigeria  Benue state command .", "Peace Corps of Nigeria  Benue state command", as.character(parti_attendees$partner))

parti_attendees$partner <- ifelse(parti_attendees$partner=="Initiative for the Empowerment of Vulnerable Persons in the Society (IEVPS)", "IEVPS", as.character(parti_attendees$partner))

parti_attendees$partner <- ifelse(parti_attendees$partner=="Tanzania youth espouse for gender and development (TYEGD)", "TYEGD", as.character(parti_attendees$partner))

parti_attendees$partner <- as.factor(parti_attendees$partner)

parti_attendees$Gender <- gsub("^male\\w+", "Male", parti_attendees$Gender)
parti_attendees$Gender <- gsub("^female\\w+", "Female", parti_attendees$Gender)

parti_attendees$Gender <- as.factor(parti_attendees$Gender)
parti_attendees$num <- as.numeric(parti_attendees$num)

parti_attendees2 <- parti_attendees %>%
  select(country, num,partner)%>%
  group_by(country, partner)%>%
  summarise(
    total =(sum(num,na.rm=T ))
  )

parti_attendees2$country <- ifelse(parti_attendees2$country == "TZ", "Tanzania",
                               ifelse(parti_attendees2$country == "NG", "Nigeria", as.character(parti_attendees2$country)))

#NG
parti_attendees_NG <- parti_attendees2[parti_attendees2$country == "Nigeria", ]

#TZ
parti_attendees_TZ <- parti_attendees2[parti_attendees2$country == "Tanzania", ]

