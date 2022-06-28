#' Plot functions
#' Author : Pieter Pypers, IITA
#' Last updated on : 16th April 2021
#' Last updated by : Fridah Wanjala (wanjala.n.fridah@gmail.com)

#------------------------------------------------------------------------------------------
# Function 1 : Drop group names from raw ODK briefcase output file 
#------------------------------------------------------------------------------------------

dropGroupNames <- function(ds){
  #SHORT DEF:   Simple function to drop groupnames from the raw ODK briefcase output file 
  #RETURNS:     dataset with renamed variables
  #DESCRIPTION: Function to cut away group names, essentially all parts of column.names before "."
  #INPUT:       ds: dataset as exported by ODK Briefcase
  
  names(ds)[grepl('\\.', names(ds))] <- sub('.*\\.', '', names(ds)[grepl('\\.', names(ds))])
  return(ds)
}

#------------------------------------------------------------------------------------------
# Function 2 : Function to merge ODK forms with repeat loops 
#------------------------------------------------------------------------------------------

mergeODKforms <- function(ds1, ds2, ds3 = NULL, ds4 = NULL){
  #SHORT DEF:   Function to merge ODK forms with repeat loops (up to 3 deep = 4 datasets).
  #RETURNS:     Merged dataframe with renamed KEYS (KEY1, KEY2, KEY3, KEY4).
  #DESCRIPTION: Assumes that there are no consistency errors, merges up to 3 deep = 4 datasets, and 
  #             requires at least two datasets.
  #INPUT:       ds1: highest level (parent) dataset (required)
  #             ds2: second level (daughter) dataset (required)
  #             ds3: third level (grand daughter) dataset (optional)
  #             ds4: fourth level (grand grand daughter) dataset (optional)
  
  names(ds1)[names(ds1) == "KEY"] <- "KEY1"
  names(ds2)[names(ds2) == "KEY"] <- "KEY2"
  names(ds2)[names(ds2) == "PARENT_KEY"] <- "KEY1"
  ds <- merge(ds1, ds2)
  if(!is.null(ds3)) {
    names(ds3)[names(ds3) == "KEY"] <- "KEY3"
    names(ds3)[names(ds3) == "PARENT_KEY"] <- "KEY2"
    ds <- merge(ds, ds3)
    if(!is.null(ds4)) {
      names(ds4)[names(ds4) == "KEY"] <- "KEY4"
      names(ds4)[names(ds4) == "PARENT_KEY"] <- "KEY3"
      ds <- merge(ds, ds4)
    }
  }
  return(ds)
}

#------------------------------------------------------------------------------------------
# Function 3 : Create set of candle plots for responses to statements
#------------------------------------------------------------------------------------------
makeCandlePlotsGroup <- function(statement_nrs = 1:32,
                                 ds, 
                                 use_or_uptake = "use",
                                 group = "genderHH", 
                                 filter_var = NULL, 
                                 filter_value = NULL){
  #SHORT DEF:   Function to create set of candle plots for responses to statements reflecting knowledge and behaviour.
  #RETURNS:     list of ggplot objects depending on the number of levels of the group variable
  #DESCRIPTION: Candle plots depicting agreement levels to statements disaggregated by country and use or uptake level.
  #INPUT:       statement_nrs <- subset of statements to select (from 32 statements).
  #             ds: dataset (daughter dataset after preprocessing)
  #             group: facetting variable as a string; for example genderHH, educationHH, partner, country
  #             filter_var: filtering variable(s) as a string; applies country, partner or c("country", "partner")
  #             filter_value: filter(s) for the filtering variable as a string; for example for country - "Nigeria" or c("Nigeria", "SG2000")
  
  # prepare dataset
  default_select_vars <- c(use_or_uptake, "statement_nr", "response", "statement_fct", group, "MainCategory", "statement", "KEY1")
  if(is.null(filter_var)) {
    select_vars <- default_select_vars
  } else {
    select_vars <- unique(c(default_select_vars, filter_var))
  }
  
  data <- subset(ds, select = select_vars)
  data <- data[data$statement_nr %in% statement_nrs, ]
  data <- na.omit(data)
  data$response <- factor(data$response, levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))
  names(data)[names(data) == use_or_uptake] <- "value"
  
  cols <- colorRampPalette(c("deepskyblue", "grey90", "grey50"))(5)
  rev_cols <- rev(cols)
  names(rev_cols) <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
  
  names(data)[names(data) == group] <- "group_var"
  
  # if group var == country
  if(group == "country") {
    data$country <- data$group_var
  }
  
  if(group == "partner") {
    data$partner <- data$group_var
  }
  
  # filter
  if(length(filter_var) == 1 & length(filter_value) == 1) {
    data <- data[data[filter_var] == filter_value, ]
  } else if(length(filter_var) == 2 & length(filter_value) == 2) {
    data <- data[data[filter_var[1]] == filter_value[1] & data[filter_var[2]] == filter_value[2], ]
  } else {
    data <- data
  }
  
  # title
  grouping_var <- tools::toTitleCase(gsub("hh", "", group, ignore.case = T))
  
  # summary of the data
  data_summary <- data %>%
    count(group_var, value, statement, response) %>%
    group_by(group_var, value, statement) %>%
    mutate(Percent = round(n/sum(n), 4)) %>%
    ungroup() %>%
    left_join(., 
              data %>%
                group_by(group_var) %>%
                summarise(Total = n_distinct(KEY1)) %>%
                ungroup()) %>%
    mutate(group_var = glue("{group_var} (n={Total})"))
  
  # main category of the statements
  main_category <- tolower(unique(data$MainCategory))
  
  # do the plot
  gg <- ggplot(data_summary, 
               aes(x = value,
                   fill = response,
                   y = Percent,
                   text = glue("Perception: {response}
                               n = {n}
                               Percent: {scales::percent(Percent)}"))) + 
    geom_bar(position = position_stack(), 
             stat = "identity") +
    scale_fill_manual(values = rev_cols) +
    facet_grid(group_var ~ statement, labeller = label_wrap_gen(width = 20)) +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.25), expand = c(0,0)) +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_text(size = rel(1.1)),
          strip.text.y = element_text(angle = 0, hjust = 0),
          legend.position = "right",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          panel.spacing = unit(1, "lines"),
          plot.margin = unit(c(2.5, 3, 0.5, 0.5), "cm"),
          axis.text.y = element_text(hjust = -0.25, angle = 0)) 

  return(gg)
  
}

#------------------------------------------------------------------------------------------
# End of script
#------------------------------------------------------------------------------------------

