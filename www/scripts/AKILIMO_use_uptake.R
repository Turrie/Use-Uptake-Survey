#' Import odk data and preprocess
#' Author: Pieter Pypers, IITA
#' First updated on : 16th April 2021
#' First updated by : Fridah Wanjala (wanjala.n.fridah@gmail.com)
#' Last updated on : September 2021
#' Last updated by : Ouma Turry, IITA

setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/UseUptake3")

source("www/scripts/AKILIMO_use_uptake_functions.R")
library(dplyr, warn.conflicts = FALSE)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

#----------------------------------------------------------
# reading in and preparing the data
#----------------------------------------------------------
ds1 <- read.csv("www2/data/form_data/AKILIMO_use_uptake.csv")
ds1 <- dropGroupNames(ds1)
ds1 <- droplevels(ds1[ds1$accepts == "yes",]) # dropping some rows without relevant data - didn't consent

#----------------------------------------------------------
# some preprocessing
#----------------------------------------------------------
ds1$time <- parse_date_time(ds1$end, "%b %d, %Y %I:%M:%S %p") - parse_date_time(ds1$start0, "%b %d, %Y %I:%M:%S %p")
ds1$country <- as.factor(plyr::revalue(ds1$country, c("NG"="Nigeria", "TZ"="Tanzania")))
ds1$FR <- grepl("FR", ds1$useCasesOther) | ds1$useCase == "FR"
ds1$IC <- grepl("IC", ds1$useCasesOther) | ds1$useCase == "IC"
ds1$PP_WM <- grepl("PP_WM", ds1$useCasesOther) | ds1$useCase == "PP_WM"
ds1$SPHS <- grepl("SP_HS", ds1$useCasesOther) | ds1$useCase == "SP_HS"
ds1$tool_app <- grepl("app", ds1$tool_format)
ds1$tool_paper <- grepl("paper", ds1$tool_format)
ds1$tool_Viamo <- grepl("Viamo", ds1$tool_format)
ds1$tool_Arifu <- grepl("Arifu", ds1$tool_format)
ds1$tool_eSOKO <- grepl("eSOKO", ds1$tool_format)

#----------------------------------------------------------
# preparing dependent variables use and uptake
#----------------------------------------------------------
ds1$use <- ifelse(is.na(ds1$use), "0", ds1$use) # is use is NA - replace with 0 (Low)
ds1$use <- as.factor(ds1$use)
ds1$use <- plyr::revalue(ds1$use, c("0"="Low", "1"="Medium", "2"="High"))
ds1$uptake <- as.factor(ds1$uptake)
ds1$uptake <- plyr::revalue(ds1$uptake, c("0"="Low", "1"="Medium", "2"="High"))

#note: the variable 'uptake' as an overall assessment of uptake was discontinued as of 20/11/2020. Instead, we switch to a calculated class. See code below:
ds1$uptake_old <- ds1$uptake
ds1$FRmean <- rowMeans(subset(ds1, select=paste0("FR", 1:6)))
ds1$CIMmean <- rowMeans(subset(ds1, select=paste0("CIM", 1:6)))
ds1$CISmean <- rowMeans(subset(ds1, select=paste0("CIS", 1:6)))
ds1$PP_WMmean <- rowMeans(subset(ds1, select=paste0("PP_WM", 1:6)))
ds1$SP_HSmean <- rowMeans(subset(ds1, select=paste0("SP_HS", 1:6)))
ds1$uptake_calc <- rowMeans(subset(ds1, select=c(FRmean, CIMmean, CISmean, PP_WMmean, SP_HSmean)), na.rm = TRUE)
ds1$uptake <- as.character(cut(ds1$uptake_calc, c(-Inf, 0.5, 1.5, Inf), labels = c("Low", "Medium", "High")))
ds1$uptake <- ifelse(is.na(ds1$uptake), "Low", ds1$uptake)
ds1$uptake <- factor(ds1$uptake, levels=c("Low", "Medium", "High"))

ds1$FRuptake <- as.character(cut(ds1$FRmean, c(-Inf, 0.5, 1.5, Inf), labels = c("Low", "Medium", "High")))
ds1$FRuptake <- ifelse(is.na(ds1$FRuptake), "Low", ds1$FRuptake)
ds1$FRuptake <- factor(ds1$FRuptake, levels=c("Low", "Medium", "High"))

ds1$CISuptake <- as.character(cut(ds1$CISmean, c(-Inf, 0.5, 1.5, Inf), labels = c("Low", "Medium", "High")))
ds1$CISuptake <- ifelse(is.na(ds1$CISuptake), "Low", ds1$CISuptake)
ds1$CISuptake <- factor(ds1$CISuptake, levels=c("Low", "Medium", "High"))

ds1$CIMuptake <- as.character(cut(ds1$CIMmean, c(-Inf, 0.5, 1.5, Inf), labels = c("Low", "Medium", "High")))
ds1$CIMuptake <- ifelse(is.na(ds1$CIMuptake), "Low", ds1$CIMuptake)
ds1$CIMuptake <- factor(ds1$CIMuptake, levels=c("Low", "Medium", "High"))

ds1$PP_WMuptake <- as.character(cut(ds1$PP_WMmean, c(-Inf, 0.5, 1.5, Inf), labels = c("Low", "Medium", "High")))
ds1$PP_WMuptake <- ifelse(is.na(ds1$PP_WMuptake), "Low", ds1$PP_WMuptake)
ds1$PP_WMuptake <- factor(ds1$PP_WMuptake, levels=c("Low", "Medium", "High"))

ds1$SP_HSuptake <- as.character(cut(ds1$SP_HSmean, c(-Inf, 0.5, 1.5, Inf), labels = c("Low", "Medium", "High")))
ds1$SP_HSuptake <- ifelse(is.na(ds1$SP_HSuptake), "Low", ds1$SP_HSuptake)
ds1$SP_HSuptake <- factor(ds1$SP_HSuptake, levels=c("Low", "Medium", "High"))

#----------------------------------------------------------
# review xstics of HH dataset
#----------------------------------------------------------
xstics_vars <- c("totalArea", "unitArea", "propArea_cassava", "propAgriculture", "propCassava", "assets", "poschanges")

# separate multiple select variables / convert them to factor
ds1 <- ds1 %>%
  separate("assets", sep = " ", into = paste0("asset", 1:14), remove = TRUE) %>%
  separate("poschanges", sep = " ", into = paste0("poschange", 1:8), remove = TRUE)

miss_dist <- sapply(ds1 %>% dplyr::select(contains("asset"), contains("poschange")), function(x) sum(is.na(x)))
to_drop <- names(miss_dist[miss_dist == nrow(ds1)])

ds1 <- ds1 %>%
  dplyr::select(-one_of(to_drop))

choices <- read_excel("www/data/supplementary/AKILIMO use and uptake survey (without farmer details).xlsx", sheet = "choices")

asset_choices <- choices %>%
  filter(list_name == "assets")

poschanges_choices <- choices %>%
  filter(list_name == "poschanges")

ds1 <- ds1 %>%
  mutate_at(vars(matches("asset")), function(x) return(factor(x, levels = asset_choices$name, labels = asset_choices$label))) %>%
  mutate_at(vars(matches("poschange")), function(x) return(factor(x, levels = poschanges_choices$name, labels = poschanges_choices$label)))

# convert land area to same unit -- take absolute of totalArea to get rid of negatives
ha_to_acre <- 2.47105

ds1 <- ds1 %>%
  mutate(totalArea = ifelse(unitArea == "ha", totalArea * ha_to_acre, totalArea) %>% abs(.))

# summarize variables in this section 
(ds1 %>%
    dplyr::select(one_of(xstics_vars), one_of(names(miss_dist))) %>%
    summary(.))

#----------------------------------------------------------
# explore/clean varibales in the participantDetails section
#----------------------------------------------------------
ds1 <- ds1 %>%
  mutate(event = as.character(event) %>% 
           ifelse(. == "", NA, .) %>%
           plyr::mapvalues(.,
                           from = c("video_show", "sensitization_event", "training_event",
                                    "demonstration_field", "field_day", "agric_show", "EsokoSMS"),
                           to = c("Video Show", "Sensitization", "Training", "Field Demonstration", 
                                  "Field Day", "Agricultural Show", "Esoko SMS")))

# Cleanup the date variable
ds1 <- ds1 %>%
  mutate(start0 = as.character(start0),
         SurveyStartDate = as.POSIXct(strptime(start0, "%b %d, %Y %H:%M:%S"))
  )

# rename some variables
# ds1 <- ds1 %>%
#   rename("DateOfEvent" = "eventDate",
#          "VenueOfEvent" = "eventVenue",
#          "CityOfEvent" = "eventCity")
  
#----------------------------------------------------------
# explore/clean varibales in the AKILIMO_use section
#----------------------------------------------------------
akilimo_use_vars <- c("use", "useCase", "useCasesOther", "tool_format", 
                      "supporting_material", "personal_support", "digital_channel")

# separate multiple select features
ds1 <- ds1 %>%
  separate("useCasesOther", sep = " ", into = paste0("useCasesOther", 1:4), remove = TRUE) %>%
  separate("tool_format", sep = " ", into = paste0("tool_format", 1:9), remove = TRUE) %>% 
  separate("supporting_material", sep = " ", into = paste0("supporting_material", 1:8), remove = TRUE) %>% 
  separate("personal_support", sep = " ", into = paste0("personal_support", 1:4), remove = TRUE) %>% 
  separate("digital_channel", sep = " ", into = paste0("digital_channel", 1:5), remove = FALSE) %>%
  mutate(digital_channel1 = ifelse(digital_channel1 == "" | is.na(digital_channel1), "None", digital_channel1),
         supporting_material1 = ifelse(supporting_material1 == "" | is.na(supporting_material1), "None", supporting_material1),
         personal_support1 = ifelse(personal_support1 == "" | is.na(personal_support1), "None", personal_support1))

miss_use_dist <- sapply(ds1 %>% dplyr::select(contains("useCasesOther"), 
                                              contains("tool_format"), 
                                              contains("supporting_material"),
                                              contains("personal_support"),
                                              contains("digital_channel")),
                        function(x) sum(is.na(x)))

to_drop <- names(miss_use_dist[miss_use_dist == nrow(ds1)])

ds1 <- ds1 %>%
  dplyr::select(-one_of(to_drop))

# Convert to factor
tool_choices <- choices %>%
  filter(list_name == "tool_format") %>%
  dplyr::select(name, label) %>%
  distinct(., .keep_all = TRUE)

supporting_choices <- choices %>%
  filter(list_name == "supporting_material") %>%
  dplyr::select(name, label) %>%
  distinct(., .keep_all = TRUE)

personal_choices <- choices %>%
  filter(list_name == "personal_support") %>%
  dplyr::select(name, label) %>%
  distinct(., .keep_all = TRUE)

digital_choices <- choices %>%
  filter(list_name == "digital_channel") %>%
  dplyr::select(name, label) %>%
  distinct(., .keep_all = TRUE)

ds1 <- ds1 %>%
  mutate_at(vars(matches("usecase")), function(x) return(factor(x, levels = c("FR", "IC", "PP_WM", "SP_HS"), labels = c("Fertilizer Recommendation", "Intercropping", "Best Planting practices/Weed management", "Scheduled planting/High starch content")))) %>%
  mutate_at(vars(matches("tool_format")), function(x) return(factor(x, levels = tool_choices$name, labels = tool_choices$label))) %>%
  mutate_at(vars(matches("supporting_material")), function(x) return(factor(x, levels = c("worksheet", "instructional_video", "farmer_video", "farmer_guide", "None"), labels = c("AKILIMO worksheets", "Instructional videos", "Farmer-friendly videos", "Farmer's guide", "None")))) %>%
  mutate_at(vars(matches("personal_support")), function(x) return(factor(x, levels = c("EA", "lead_farmer", "fellow_farmer", "other", "None"), labels = c("EA", "Lead farmer", "Fellow farmers", "Others", "None")))) %>%
  mutate_at(vars(matches("digital_channel")), function(x) return(factor(x, levels = c("radio", "television", "YouTube", "Facebook", "WhatsApp", "None"), labels = c("Radio program", "Television", "YouTube", "Facebook", "WhatsApp", "None"))))

(ds1 %>%
    dplyr::select(one_of(akilimo_use_vars), one_of(names(miss_use_dist))) %>%
    summary(.))

#----------------------------------------------------------
# explore/clean varibales in the sampling section
#----------------------------------------------------------
sampling_vars <- c("country", "partner", "monthYear", "gender")

(ds1 %>%
    dplyr::select(one_of(sampling_vars)) %>%
    map(~table(., useNA = "always")))

#----------------------------------------------------------
# explore/clean varibales in the HH section
#----------------------------------------------------------
hh_vars <- c("genderHH", "HHH", "ageHH", "educationHH")

# For age combine the first 2 groups
ds1 <- ds1 %>%
  mutate(
    ageHH = as.character(ageHH) %>%
      ifelse(. %in% c("0-17", "18-24"), "<=24", .) %>%
      factor(., levels = c("<=24", "25-34", "35-44", "45-54", "55-64", "65+")),
    educationHH = as.character(educationHH) %>%
      factor(., 
             levels = c("none", "primary", "secondary", "tertiary"), 
             labels = c("No formal education", "Primary", "Secondary", "Tertiary")),
    genderHH = as.character(genderHH) %>% 
      factor(., levels = c("F", "M"), labels = c("Female", "Male"))
  )

(ds1 %>%
    dplyr::select(one_of(hh_vars)) %>%
    map(~table(., useNA = "always")))

#----------------------------------------------------------
# create wealth class / cassava orientation
#----------------------------------------------------------
# wealth class
ds1 <- ds1 %>%
  mutate(
    wealth_class = cut(totalArea, 
                       breaks = c(-Inf, 3, Inf),
                       right = TRUE, 
                       labels = c("Poor", "Rich")),
  )

# cassava_orientation 
ds1 <- ds1 %>%
  mutate(
    cassava_orientation = propAgriculture * propCassava,
    cassava_orientation = ifelse(cassava_orientation < 0.5, "Subsistence", "Commercial")
  ) %>%
  mutate(farm_typology = glue("{wealth_class} {cassava_orientation}") %>%
           factor(., levels = c("Poor Subsistence", "Poor Commercial", "Rich Subsistence", "Rich Commercial"), 
                  labels = c("Poor Subsistence", "Poor Commercial", "Rich Subsistence", "Rich Commercial")))

# manually categorize land area
ds1 <- ds1 %>%
  mutate(
    totalArea_fct = cut(totalArea, 
                        breaks = c(-Inf, 1, 5, 10, Inf),
                        right = TRUE,
                        include.lowest = TRUE,
                        labels = c("<=1", ">1 to 5", ">5 to 10", ">10")),
  )

# proportion of income from agriculture
ds1 <- ds1 %>%
  mutate(
    propAgriculture_fct = factor(propAgriculture, 
                                 levels = c(0.00, 0.25, 0.50, 0.75, 1.00), 
                                 labels = c("0%", "25%", "50%", "75%", "100%"))
  )

# land area on cassava and income from cassave
ds1 <- ds1 %>%
  mutate(
    cassavaTotalArea = propArea_cassava * totalArea,
    propIncomeCassava = propCassava * propAgriculture, 
    cassavaTotalArea_fct = cut(cassavaTotalArea, 
                               breaks = c(-Inf, 1, 5, 10, Inf),
                               right = TRUE,
                               include.lowest = TRUE,
                               labels = c("<=1", ">1 to 5", ">5 to 10", ">10")),
    propIncomeCassava_fct = cut(propIncomeCassava, 
                                breaks = c(-Inf, 0.0625, 0.25, 0.5, 0.75, Inf),
                                right = TRUE,
                                include.lowest = TRUE,
                                labels = c("<=6.25%", ">6.25% to 25%", ">25% to 50%", ">50% to 75%", ">75% to 100%"))
  )

#----------------------------------------------------------
# create new features for the model
#----------------------------------------------------------
## Assets : number of assets and categorize
# a. wealth xstics
# have_tv, have_electicity, have_tap_water, have_car_motorcyle_truck, have_phone, number_of_assets
assets_df <- ds1 %>%
  dplyr::select(KEY, starts_with("asset")) %>%
  gather(Variable, value, asset1:asset12) %>%
  mutate(value = ifelse(value == "None", NA, value)) %>%
  filter(!is.na(value)) %>%
  group_by(KEY) %>%
  mutate(have_tv = grepl("television", value, ignore.case = T),
         have_electicity = grepl("electricity", value, ignore.case = T), 
         have_tap_water = grepl("tap|water", value, ignore.case = T), 
         have_car_motorcyle_truck = grepl("car|motor|truck", value, ignore.case = T), 
         have_phone = grepl("phone", value, ignore.case = T)) %>%
  dplyr::summarise(number_assets = n(),
            have_tv = max(have_tv),
            have_electicity = max(have_electicity),
            have_tap_water = max(have_tap_water),
            have_car_motorcyle_truck = max(have_car_motorcyle_truck),
            have_phone = max(have_phone)) %>%
  ungroup() 

ds1 <- ds1 %>%
  left_join(., assets_df, by = "KEY") %>%
  mutate_at(setdiff(names(assets_df), "KEY"), function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(number_assets_fct = case_when(number_assets <= 1           ~ 1,
                                       between(number_assets, 2, 4) ~ 2,
                                       between(number_assets, 5, 7) ~ 3,
                                       number_assets >= 8           ~ 4) %>%
           factor(., levels = c(1, 2, 3, 4), labels = c("None or 1", "2 to 4", "5 to 7", "8 to 14")))


# Number of positive changes
poschange <- ds1 %>%
  dplyr::select(KEY, starts_with("poschange")) %>%
  gather(Variable, value, poschange1:poschange8) %>%
  mutate(value = ifelse(value == "None", NA, value)) %>%
  filter(!is.na(value)) %>%
  group_by(KEY) %>%
  dplyr::summarise(number_changes = n()) %>%
  ungroup() 

ds1 <- ds1 %>%
  left_join(., poschange, by = "KEY") %>%
  mutate(number_changes = ifelse(is.na(number_changes), 0, number_changes),
         number_changes_fct = case_when(between(number_changes, 1, 2) ~ 1,
                                        between(number_changes, 3, 5) ~ 2,
                                        between(number_changes, 6, 8) ~ 3,
                                        TRUE ~ number_changes) %>%
           factor(., levels = c(0, 1, 2, 3), labels = c("None", "1 to 2", "3 to 5", "6 to 8")))

# number of tool formats
ds1$number_tool_formats <- rowSums(ds1[, c("tool_app", "tool_paper", "tool_Viamo", "tool_Arifu", "tool_eSOKO")])

ds1 <- ds1 %>%
  mutate(number_tool_formats_fct = ifelse(number_tool_formats > 1, 2, number_tool_formats) %>%
           factor(., levels = c(1, 2), labels = c("One", "Atleast two")))

# number of supporting material
supporting <- ds1 %>%
  dplyr::select(KEY, starts_with("supporting_material")) %>%
  gather(Variable, value, supporting_material1:supporting_material3) %>%
  mutate(value = ifelse(value == "None", NA, value)) %>%
  filter(!is.na(value)) %>%
  group_by(KEY) %>%
  dplyr::summarise(number_supporting_material = n()) %>%
  ungroup()

ds1 <- ds1 %>%
  left_join(., supporting, by = "KEY") %>%
  mutate(number_supporting_material = ifelse(is.na(number_supporting_material), 0, number_supporting_material)) %>%
  mutate(number_supporting_material_fct = ifelse(number_supporting_material > 1, 2, number_supporting_material) %>%
           factor(., levels = c(0, 1, 2), labels = c("None", "One", "Atleast two")))

# number personal support
personal_support_df <- ds1 %>%
  dplyr::select(KEY, starts_with("personal_support")) %>%
  mutate_at(vars(matches("personal_support")), function(x) ifelse(grepl("farmer|lead", x, ignore.case = T), "Fellow farmer", as.character(x))) %>%
  mutate(personal_support1 = ifelse(grepl("none", personal_support1, ignore.case = T), "Not discussed", as.character(personal_support1))) %>%
  mutate_at(c("personal_support2", "personal_support3", "personal_support4"), function(x) ifelse(grepl("none", x, ignore.case = T), NA, as.character(x))) %>%
  gather(Variable, Value, personal_support1:personal_support4) %>%
  dplyr::select(KEY, Value) %>%
  filter(!is.na(Value)) %>%
  distinct(., .keep_all = TRUE) %>%
  group_by(KEY) %>%
  mutate(id = glue("personal_support{row_number()}")) %>%
  spread(id, Value)

personal_support_df$no_miss <- apply(personal_support_df, 1, function(x) sum(is.na(x)))

personal_support_df <- personal_support_df %>%
  mutate(personal_support_fct = case_when(no_miss == 2 ~ personal_support1,
                                          no_miss == 1 ~ paste(personal_support1, personal_support2, sep = " and "),
                                          no_miss == 0 ~ paste(paste(personal_support1, personal_support2, sep = ", "), personal_support3, sep = " and "))) %>%
  dplyr::select(KEY, personal_support_fct)

ds1 <- ds1 %>%
  left_join(., personal_support_df, by = "KEY") 

# number of digital channels
digital_channel_df <- ds1 %>%
  dplyr::select(KEY, starts_with("digital_channel")) %>%
  dplyr::select(-digital_channel) %>%
  mutate(digital_channel1 = ifelse(grepl("none", digital_channel1, ignore.case = T) | is.na(digital_channel1), "Not heard", as.character(digital_channel1))) %>%
  mutate_at(c("digital_channel2", "digital_channel3", "digital_channel4", "digital_channel5"), function(x) ifelse(grepl("none", x, ignore.case = T), NA, as.character(x))) %>%
  mutate(digital_channel_fct = ifelse(grepl("not", digital_channel1, ignore.case = T), "None", "Atleast one channel")) %>%
  dplyr::select(KEY, digital_channel_fct)

ds1 <- ds1 %>%
  left_join(., digital_channel_df, by = "KEY") 

# rename some variables
ds1 <- ds1 %>%
  rename("old_digital_channel" = "digital_channel",
         "fct_digital_channel" = "digital_channel_fct")

#----------------------------------------------------------
# reading in and preparing the data with statements
#----------------------------------------------------------
ds2 <- read.csv("www/data/form_data/AKILIMO_use_uptake-perceptions_repeat.csv")
ds2 <- dropGroupNames(ds2)
ds2 <- mergeODKforms(ds1, ds2)

ds2 <- ds2 %>%
  mutate(response = factor(response, levels = c(-2, -1, 0, 1, 2), 
                           labels = c("Strongly disagree",
                                      "Disagree",
                                      "Neither agree nor disagree",
                                      "Agree",
                                      "Strongly agree"
                           )))

ds2 <- ds2 %>%
  dplyr::filter(!is.na(statement_nr))

all_statements <- ds2 %>%
  dplyr::select(statement_nr, statement) %>%
  distinct(., .keep_all = TRUE)

statements2 <- read_excel("www/data/supplementary/statements.xlsx") %>%
  mutate(category2 = gsub("_", "\n", category))

ds2 <- left_join(ds2, statements2 %>% 
                   dplyr::select(-statement)) %>%
  mutate(statement_fct = factor(statement_nr, levels = statements2$statement_nr, labels = statements2$category2))

#----------------------------------------------------------
# End of script
#----------------------------------------------------------

unique(ds2$gender)
