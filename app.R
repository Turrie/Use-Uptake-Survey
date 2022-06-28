library(shiny)
library(shinythemes)
library(ggplot2)
library(tmap)
library(shinybusy)

#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/UseUptake3")
source("global.R")
#READ THE SHAPE FILES


ui <- fluidPage(
 
  navbarPage(windowTitle ="AKILIMO Tools Use and Uptake Survey",
             title=div(img(src = "https://akilimo.sirv.com/images/akilimo_icon.png", height = "50px", style = "position: relative; top: -3px; left: -1000px;"),"Akilimo Tools Use and Uptake Survey" ),
             inverse = F, # for diff color view
             theme = shinytheme("flatly"),

          tabPanel("Overall",
                   fluidRow(valueBoxOutput("ng_respondents", width = 6),
                            valueBoxOutput("tz_respondents", width = 6)
                   ),
                   br(),
                   br(),

                   fluidRow(
                     box(title = NULL, status = "primary", id = "box_use_uptake_ng", 
                          fluidRow(column(width = 6,
                                         
                                         plotOutput("use_uptake_ng_gh", height = "400px")),
                                  column(width = 6,
                                         plotOutput("uptake_use_ng_gh", height = "400px"))),
                                         
                         style='margin-bottom:30px;'
                     ),
                     
                   
                     
                     box(id = "box_use_uptake_tz", 
                         fluidRow(column(width = 6,
                                         plotOutput("use_uptake_tz_gh", height = "400px")),
                                  tags$head(
                                    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                                  ),
                                  column(width = 6,
                                         plotOutput("uptake_use_tz_gh", height = "400px"))),
                                 
                         style='margin-bottom:30px;border-left:0.5px solid; padding: 10px;'
                     )
                   ),
                   plotOutput("use_codes", height = "120px"),
                   plotOutput("uptake_codes", height = "120px"),
                   
                   absolutePanel(id = "logo", class = "card", top = 50, right = 50, width = 100, fixed=TRUE, draggable = FALSE, 
                                 tags$img(src='https://akilimo.sirv.com/images/akilimo_icon.png',height = 150, width = 150
                                 ),
                                 onclick = sprintf("window.open('%s')",
                                                   "https://akilimo.org")
                                 
                   )),
             tabPanel("Use & Uptake",
                      
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                       conditionalPanel(
                                         condition = "input.main_tabs != 'dash_tab'", 
                                         h3(tags$p("Select report filters", style = "margin-left: 16px; font-size: 16px")),
                                         selectInput("country_sel", label = h5(tags$p("Country:", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = c("Compare countries", countries_lst), selected = "Compare countries")
                                       ),
                                       
                                       conditionalPanel(
                                         condition = "input.country_sel != 'Compare countries' && input.main_tabs != 'tab_drivers'",
                                         selectInput(inputId = "partner_sel", label = h5(tags$p("Partner (optional)", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = partners_lst, selected = "All partners")
                                       ),
                                       
                                       conditionalPanel(
                                         condition = "input.main_tabs == 'by_covariates' || input.main_tabs == 'tab_perceptions' || input.main_tabs == 'tab_drivers'", 
                                         selectInput("use_uptake", label = h5(tags$p("Use or uptake", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = use_uptake_lst, selected = "Use")
                                       ),
                                       
                                        conditionalPanel(
                                         condition = "input.country_sel != 'Compare countries' && input.main_tabs != 'by_covariates' && input.main_tabs != 'tab_drivers'",
                                         selectInput("facet_sel1_2", label = h5(tags$p("Facetting variable (optional)", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = facet_all, selected = "None")
                                       ),
                                       
                                       conditionalPanel(
                                         condition = "input.country_sel == 'Compare countries' && input.main_tabs == 'by_covariates'",
                                         selectInput("group_sel1_1", label = h5(tags$p("Grouping variable", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = compare_countries_grps, selected = "genderHH"),
                                         
                                         selectInput("plot_type2", label = h5(tags$p("Type of plot", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = plot_types, selected = "Proportion bar")
                                       ),

                                       conditionalPanel(
                                         condition = "input.main_tabs == 'steps6'", 
                                         selectInput("use_case_type", label =  h5(tags$p("Use case", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = use_case_types),
                                         verbatimTextOutput("steps6_text")
                                       ),
                                       
                                       conditionalPanel(
                                         condition = "input.main_tabs == 'by_covariates' && input.country_sel != 'Compare countries'", 

                                         selectInput("facet_sel1_1", label = h5(tags$p("Facetting variable (optional)", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = facet_all, selected = "None"),
                                         
                                         selectInput("group_sel1_0", label = h5(tags$p("Grouping variable", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = subgroups_all),
                                         
                                         selectInput("plot_type", label = h5(tags$p("Type of plot", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = plot_types, selected = "Proportion bar"),
                                         verbatimTextOutput("covariates_text")
                                       ),
                                       
                                       conditionalPanel(
                                         condition = "input.main_tabs == 'tab_perceptions'", 
                                         selectInput("perception_category", label = h5(tags$p("Select statements category", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = perception_main_categories),
                                         verbatimTextOutput("perceptions_text")
                                       ),
                                       
                                       conditionalPanel(
                                         condition = "input.country_sel != 'Compare countries' && input.main_tabs == 'tab_drivers'",
                                         selectInput("facet_sel1_3", label = h5(tags$p("Facetting variable (optional)", style = "margin-left: 16px; font-size: 16px")),
                                                     choices = drivers_facets, selected = "None")
                                       ),
                                       
                                       conditionalPanel(
                                         condition = "input.main_tabs == 'tab_drivers'", 
                                         verbatimTextOutput("drivers_text")
                                      
                                       )),
                                       mainPanel(
                                        tabsetPanel(id = "main_tabs",
                                        
                                   
            tabPanel(tags$p("Uptake of 6 steps", style = "margin-left: 50px; font-size: 16px"), value = "steps6",
                      
                      uiOutput('ui_uptake6', width=12),
                      
                      fluidRow(
                        box(title = NULL,
                            status = "primary", solidHeader = F, collapsible = F, width = 12,
                            tableOutput("step6_table")
                        )
                      )),
             tabPanel(tags$p("By covariates", style = "margin-left: 16px; font-size: 16px"), value = "by_covariates",
                      uiOutput('ui_by_covariates', width=12)),
             tabPanel(tags$p("By perceptions", style = "margin-left: 16px; font-size: 16px"), value = 'tab_perceptions', width=12,
                      uiOutput('ui_perceptions', width=12)),
             tabPanel(tags$p("Drivers of use and uptake", style = "margin-left: 16px; font-size: 16px"), value = 'tab_drivers', 
                      uiOutput("ui_drivers", width=12),
                      
                      fluidRow(
                        box(title = NULL,
                            status = "primary", solidHeader = T, collapsible = F, width = 12,
                            tableOutput("drivers_table")
                        ) ))
           
                                     )
                                   )),
            absolutePanel(id = "logo", class = "card", top = 50, right = 50, width = 100, fixed=TRUE, draggable = FALSE, 
                          tags$img(src='https://akilimo.sirv.com/images/akilimo_icon.png',height = 150, width = 150
                          ),
                          onclick = sprintf("window.open('%s')",
                                            "https://akilimo.org")
                          
            )),
          
             tabPanel("Map of events organized",
                      tabPanel("Reach",
                               
                               div(class="outer",
                                   tags$head(includeCSS("styles2.css")),
                                   tmapOutput(outputId = "event_plt", width="100%", height="100%"),
                                   
                                   absolutePanel(id = "controls", class = "panel panel-default",
                                                 top = 95, left = 70, width = 320, fixed=TRUE,
                                                 draggable = TRUE, height = "auto",
                                                 selectInput("country", "Country:",
                                                             c("Nigeria", "Tanzania")),
                                                 
                                                 
                                                 selectInput("selection", "View by:",
                                                             choice = c("Events", "Total participants"), multiple = FALSE,
                                                             selected = "Total participants"),
                                                 
                                                 conditionalPanel(
                                                   condition = "input.selection == 'Events'",
                                                   uiOutput("event")),
                                                 
                                                 actionButton("btn_go", "Click here to view map", icon("map"),
                                                              style="color: #fff; background-color: green; border-color: #2e6da4"))
                               )),
                      absolutePanel(id = "logo", class = "card", bottom = 35, left = 50, width = 100, fixed=TRUE, draggable = FALSE, 
                                    tags$img(src='https://akilimo.sirv.com/images/akilimo_icon.png',height = 150, width = 150
                                    ),
                                    onclick = sprintf("window.open('%s')",
                                                      "https://akilimo.org")
                                    
                      )
                      ),
            tabPanel("AKILIMO reach",
                     
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    
                                    selectInput("country_r", "View by country:",
                                                choice = c("Nigeria", "Tanzania", "Compare countries"), multiple = FALSE,
                                                selected = " "),
                                    
                                    selectInput("disagg", "View attendees by:",
                                                choice = c("Events", "Usecase", "Partners", "Gender"), multiple = FALSE,
                                                selected = "Events")
                                   
                         ),
                       
                       mainPanel(
                         tabsetPanel(id = "main_tabs3",
                                     
                                      tabPanel("Overview of events organized",
                                              fluidRow(
                                                box(title = textOutput("attendees"), id = "box_uptake7",
                                             
                                              #by partner
                                              conditionalPanel(
                                                condition = "input.disagg == 'Partners'",
                                             
                                              conditionalPanel(
                                                condition = "input.country_r == 'Nigeria'",
                                              plotlyOutput("prtnr_pt_ng", height = '700px', width = '1500px')),
                                              conditionalPanel(
                                                condition = "input.country_r == 'Tanzania'",
                                              plotlyOutput("prtnr_pt_tz", height = '700px', width = '1500px')),
                                             
                                              conditionalPanel(
                                                
                                                condition = "input.country_r == 'Compare countries'",
                                                plotlyOutput("prtnr_pt_fct", height = '1500px', width = '1500px'))
                                                    
                                              ),
                                              
                                              #by usecase
                                              conditionalPanel(
                                                condition = "input.disagg == 'Usecase'",
                                                
                                                conditionalPanel(
                                                  condition = "input.country_r == 'Nigeria'",
                                                  plotlyOutput("part_usecase_NG", height = '450px', width = '1500px')),
                                                conditionalPanel(
                                                  condition = "input.country_r == 'Tanzania'",
                                                  plotlyOutput("part_usecase_TZ", height = '450px', width = '1500px')),
                                                
                                                conditionalPanel(
                                                  condition = "input.country_r == 'Compare countries'",
                                                  plotlyOutput("part_usecase", height = '700px', width = '1500px'))
                                              ),
                                              
                                              
                                              #by event
                                              conditionalPanel(
                                                condition = "input.disagg == 'Events'",
                                                conditionalPanel(
                                                  condition = "input.country_r == 'Nigeria'",
                                              plotlyOutput("event_pt_ng", height = '450px', width = '1500px')),
                                              
                                              conditionalPanel(
                                                condition = "input.country_r == 'Tanzania'",
                                              plotlyOutput("event_pt_tz", height = '450px', width = '1500px')),
                                              
                                              conditionalPanel(
                                                condition = "input.country_r == 'Compare countries'",
                                            
                                              plotlyOutput("event_pt_fct", height = '700px', width = '1500px'))
                                              
                                              ),
                                              
                                              #by gender
                                              conditionalPanel(
                                                condition = "input.disagg == 'Gender'",
                                                conditionalPanel(
                                                  condition = "input.country_r == 'Nigeria'",
                                                  plotlyOutput("gen_NG", height = '450px', width = '1500px')),
                                                
                                                conditionalPanel(
                                                  condition = "input.country_r == 'Tanzania'",
                                                  plotlyOutput("gen_TZ", height = '450px', width = '1500px')),
                                                
                                                conditionalPanel(
                                                  condition = "input.country_r == 'Compare countries'",
                                                  
                                                  plotlyOutput("gen_facet", height = '700px', width = '1500px'))
                                                
                                              )
                                      ))),
                                     tabPanel("Total number of attendees",
                                              fluidRow(
                                                box(title = textOutput("Events"), id = "box_uptake6",
                                                    status = "primary", solidHeader = F, collapsible = F, width = 12,
                                                    conditionalPanel(
                                                      condition = "input.country_r == 'Compare countries' && input.disagg == 'Events'",
                                                      
                                                      plotlyOutput("event_plotly", height = '600px', width = '1150px')),
                                                    conditionalPanel(
                                                      condition = "input.country_r == 'Compare countries' && input.disagg == 'Usecase'", 
                                                   
                                                       plotlyOutput("use_plotly", height = '600px', width = '1300px')),
                                                    conditionalPanel(
                                                       condition = "input.country_r == 'Nigeria' && input.disagg == 'Usecase'", 
                                                      
                                                      plotlyOutput("use_plotly_NG", height = '500px', width = '1300px')),
                                                    conditionalPanel(
                                                       condition = "input.country_r == 'Tanzania' && input.disagg == 'Usecase'", 
                                                      
                                                      plotlyOutput("use_plotly_TZ", height = '500px', width = '1300px')),
                                                    conditionalPanel(
                                                       condition = "input.country_r == 'Nigeria' && input.disagg == 'Events'",
                                                      
                                                      plotlyOutput("event_plotly_NG", height = '500px', width = '1150px')),
                                                    conditionalPanel(
                                                       condition = "input.country_r == 'Tanzania' && input.disagg == 'Events'", 
                                                      
                                                      plotlyOutput("event_plotly_TZ", height = '500px', width = '1300px')),
                                                    
                                                    #by partner
                                                    conditionalPanel(
                                                       condition = "input.country_r == 'Compare countries' && input.disagg == 'Partners'", 
                                                      
                                                      plotlyOutput("partnr_plotly", height = '1500px', width = '1300px')),
                                                    conditionalPanel(
                                                       condition = "input.country_r == 'Nigeria' && input.disagg == 'Partners'", 
                                                      
                                                      plotlyOutput("partnr_plotly_NG", height = '800px', width = '1300px')),
                                                    conditionalPanel(
                                                       condition = "input.country_r == 'Tanzania' && input.disagg == 'Partners'", 
                                                      
                                                      plotlyOutput("partnr_plotly_TZ", height = '800px', width = '1300px')),
                                                    
                                                    #by gender
                                                    conditionalPanel(
                                                       condition = "input.disagg == 'Gender'",
                                                    conditionalPanel(
                                                        condition = "input.country_r == 'Nigeria'",
                                                        plotlyOutput("gen_att_NG", height = '450px', width = '1500px')),
                                                      
                                                    conditionalPanel(
                                                        condition = "input.country_r == 'Tanzania'",
                                                        plotlyOutput("gen_att_TZ", height = '450px', width = '1500px')),
                                                      
                                                    conditionalPanel(
                                                        condition = "input.country_r == 'Compare countries'",
                                                        
                                                        plotlyOutput("gen_att_facet", height = '700px', width = '1500px'))
                                                      
                                                    )
                                                )
                                                
                                              )
                                              
                                     )
                         ))),
                     
                     
                     absolutePanel(id = "logo", class = "card", top = 50, right = 50, width = 100, fixed=TRUE, draggable = FALSE, 
                                   tags$img(src='https://akilimo.sirv.com/images/akilimo_icon.png',height = 150, width = 150
                                   ),
                                   onclick = sprintf("window.open('%s')",
                                                     "https://akilimo.org")
                                   
                     ))
           

     )
)
#options(shiny.maxRequestSize=10*1000*1024^2)   ###  maximum input dataset size is set to 10GB

server <- function(input, output,session) {
  
  #----------------------------------------------------------------------------
  # Change width of box reactively
  #----------------------------------------------------------------------------
  
  values <- reactiveValues()
  
  values$adaptive_width <- NULL
  values$adaptive_height <- "400px"
  
  #----------------------------------------------------------------------------
  ## Second tab (Uptake of 6 steps): Render the ui with flexible width 
  #----------------------------------------------------------------------------
  
  output$ui_uptake6 <- renderUI({
    
    if(input$country_sel == "Compare countries") {
      
      fluidRow(
        box(title = textOutput("title_uptake6"), id = "box_uptake6",
            
            status = "primary", solidHeader = F, collapsible = F, width = 12,
            plotlyOutput("uptake_most_gh", height = 'auto', width = '1000px')       
        )
      )
      
    } else {
      
      if(!is.null(values$adaptive_width)){
        
        a1_width <- as.numeric(as.character(values$adaptive_width))
        
        fluidRow(
          box(title = textOutput("title_uptake6"), id = "box_uptake6",
              status = "primary", solidHeader = F, collapsible = F, width = 12,
              plotlyOutput("uptake_most_gh", height = "auto", width = '1000px')
          )
        )
      }
    }
  })
  
  #----------------------------------------------------------------------------
  ## Third tab (Use and uptake by covariates): Render the ui with flexible width and height
  #----------------------------------------------------------------------------
  
  output$ui_by_covariates <- renderUI({
    
   # a2_height <- as.character(values$adaptive_height)
    
    if(input$country_sel == "Compare countries") {
      
      fluidRow(
        box(title = textOutput("title_by_covariates"), id = "box_covariates",
            status = "primary", solidHeader = F, collapsible = F, width = 12,
            plotlyOutput("use_uptake_gh", height = "auto", width = "1500px")
        )
      ) 
      
    } else {
      
      if(!is.null(values$adaptive_width)){
        
        #a2_width <- as.numeric(as.character(values$adaptive_width))
        
        fluidRow(
          box(title = textOutput("title_by_covariates"), id = "box_covariates",
              status = "primary", solidHeader = F, collapsible = F, width = 12,
              plotlyOutput("use_uptake_gh", height = "auto", width = "1500px")
          )
        )  
      }
    }
  })
  
  #----------------------------------------------------------------------------
  ## Fourth tab (Perceptions): Render the ui with flexible width 
  #----------------------------------------------------------------------------
  
  output$ui_perceptions <- renderUI({
    
    a4_height <- as.character(values$adaptive_height)
    
    fluidRow(
      box(id = "box_perceptions", title = textOutput("title_perceptions"),
          status = "primary", solidHeader = T, collapsible = F, width = "1400px",
          plotlyOutput("perceptions_gh", height = 'auto', width = '1400px')
      )
    )
  })
  
  #----------------------------------------------------------------------------
  ## Last tab (Drivers of use/uptake): Render the ui with flexible width 
  #----------------------------------------------------------------------------
  
  output$ui_drivers <- renderUI({
    
    a5_height <- as.character(values$adaptive_height)
    
    if(!is.null(values$adaptive_width)){
      
      a3_width <- as.numeric(as.character(values$adaptive_width))
      
      fluidRow(
        box(title = textOutput("title_ranks"), id = "box_drivers",
            status = "primary", solidHeader = T, collapsible = F, width = a3_width,
            plotlyOutput("rank_percepions_gh", height = 'auto', width = '1000px')
        )
      )  
    }
  })
  #----------------------------------------------------------------------------
  # If country is not selected -- reset all the filters [facets and grouping]
  #----------------------------------------------------------------------------
  
  observe({
    
    if(input$country_sel == 'Compare countries') {
      
      updateSelectInput(session,
                        inputId = "group_sel1_1",
                        choices = compare_countries_grps,
                        selected = "genderHH")
      
      updateSelectInput(session, 
                        inputId = "facet_sel1_1",
                        choices = facet_all,
                        selected = "None")
    } 
  })
  
  #----------------------------------------------------------------------------
  # Filter partner list if country is selected
  #----------------------------------------------------------------------------
  
  observe({
    
    if(grepl("nigeria", input$country_sel, ignore.case = T)) {
      
      updateSelectInput(session,
                        inputId = "partner_sel",
                        choices = ng_partners_lst,
                        selected = "All partners")
      
    } else if(grepl("tanzania", input$country_sel, ignore.case = T)) {
      
      updateSelectInput(session,
                        inputId = "partner_sel",
                        choices = tz_partners_lst,
                        selected = "All partners")
    } 
  })
  
  #----------------------------------------------------------------------------
  # Disaggregation variables --  the options gender, event, partner, education, age are 
  # only available if not chosen as filter2 or faceting variable
  #----------------------------------------------------------------------------
  
  observe({
    
    # first condition - partner is filtered // facet // subgroup chosen
    partner_chosen <- (input$partner_sel != "All partners")
    facet_selected <- (input$facet_sel1_1 != "None")
    group_selected <- (input$group_sel1_0 %in% facet_all[facet_all != "None"])
    
    # which group/facet is selected
    selected_facet <- facet_all[facet_all == input$facet_sel1_1]
    selected_sub_group <- subgroups_all[subgroups_all == input$group_sel1_0]
    group_is_facet <- selected_sub_group %in% facetting_vars
    group_is_not_facet <- !(selected_sub_group %in% facetting_vars)
    
    # change choices list based on whether any of the three conditions are met
    if(partner_chosen) {
      
      updateSelectInput(session,
                        inputId = "group_sel1_0",
                        choices = subgroups_all[subgroups_all != "partner"],
                        selected = selected_sub_group)
      
      updateSelectInput(session,
                        inputId = "facet_sel1_1",
                        choices = facet_all[facet_all != "partner"],
                        selected = selected_facet)
      
    } else if(facet_selected) {
      
      updateSelectInput(session,
                        inputId = "group_sel1_0",
                        choices = subgroups_all[subgroups_all != selected_facet],
                        selected = selected_sub_group)
      
      if(group_is_facet) {
        
        updateSelectInput(session,
                          inputId = "facet_sel1_1",
                          choices = facet_all[facet_all != selected_sub_group],
                          selected = selected_facet)
      }
      
    } else if(group_selected & group_is_facet) {
      
      updateSelectInput(session,
                        inputId = "facet_sel1_1",
                        choices = facet_all[facet_all != selected_sub_group],
                        selected = selected_facet)
      
      updateSelectInput(session,
                        inputId = "group_sel1_0",
                        choices = subgroups_all[subgroups_all != selected_facet],
                        selected = selected_sub_group)
    }else if(group_is_not_facet) {
      
      updateSelectInput(session,
                        inputId = "facet_sel1_1",
                        choices = facet_all,
                        selected = selected_facet)
      
      updateSelectInput(session,
                        inputId = "group_sel1_0",
                        choices = subgroups_all,
                        selected = selected_sub_group)
    }
    
  })
  
  #----------------------------------------------------------------------------
  ## Analysis data for the use and uptake tab : Tab 3 (use and uptake by covariates)
  #----------------------------------------------------------------------------
  
  analysisData <- reactive({
    
    # use or uptake // selected facet // selected grouping
    use_uptake_var <- tolower(input$use_uptake)
    
    #use_uptake_var <- tolower(use_uptake)
    
    # selected_grouping <- subgroups_all[subgroups_all == group_sel1_0] %>% names(.) %>% tolower()
    # selected_grouping_var <- subgroups_all[subgroups_all == group_sel1_0]
    # names(selected_grouping_var) <- NULL
    # groupselected <- input$group_sel1_1
    # 
    # selected_grouping <- groupselected %>% names(.) %>% tolower()
    # selected_grouping_var <- groupselected
    # names(selected_grouping_var) <- NULL
    
    # use or uptake // selected facet // selected grouping
    selected_grouping <- subgroups_all[subgroups_all == input$group_sel1_0] %>% names(.) %>% tolower()
    selected_grouping_var <- subgroups_all[subgroups_all == input$group_sel1_0]
    names(selected_grouping_var) <- NULL
    
    # if(country_sel == "Compare countries") {
    #   selected_facet <- "Country"
    #   selected_facet_var <- "country"
    # } else {
    #   if(input$facet_sel1_1 == "None") {
    #     selected_facet <- "Country"
    #     selected_facet_var <- "country"
    #   } else {
    #     selected_facet <- facet_all[facet_all == facet_sel1_1] %>% names(.) %>% tolower()
    #     selected_facet_var <- facet_all[facet_all == facet_sel1_1]
    #     names(selected_facet_var) <- NULL
    #   }
    # }
    
    if(input$country_sel == "Compare countries") {
      selected_facet <- "Country"
      selected_facet_var <- "country"
    } else {
      if(input$facet_sel1_1 == "None") {
        selected_facet <- "Country"
        selected_facet_var <- "country"
      } else {
        selected_facet <- facet_all[facet_all == input$facet_sel1_1] %>% names(.) %>% tolower()
        selected_facet_var <- facet_all[facet_all == input$facet_sel1_1]
        names(selected_facet_var) <- NULL
      }
    }
    
    #selected_grouping_var = "use_case"
    # Analysis datases
    # If grouping variable is multiple select or use_case, prepare analysis data differently
    if(selected_grouping_var == "None") {
      
      analysis_dataset <- ds1 %>%
        dplyr::select(unique(c(filter_vars, selected_facet_var)), use_uptake_var) %>%
        rename("use_or_uptake" = use_uptake_var,
               "FacetVar" = selected_facet_var)
      
    } else if(selected_grouping_var == "useCase") {
      
      analysis_dataset <- ds1 %>%
        dplyr::select(unique(c(filter_vars, selected_facet_var)), use_uptake_var, FR, IC, PP_WM, SPHS) %>%
        rename("use_or_uptake" = use_uptake_var) %>%
        gather(GrpVar, Value, FR:SPHS) %>%
        mutate(GrpVar = factor(GrpVar, levels = c( "FR", "IC", "PP_WM", "SPHS"), labels = c( "FR", "IC", "PP/WM", "SP/HS"))) %>%
        filter(Value) %>%
        rename("FacetVar" = selected_facet_var)
      
    } else {
      
      multiple_select <- (ds1 %>% dplyr::select(starts_with(selected_grouping_var)) %>% ncol(.)) > 1
      
      if(multiple_select) {
        analysis_dataset <- ds1 %>%
          dplyr::select(unique(c(filter_vars, selected_facet_var)), use_uptake_var, starts_with(selected_grouping_var)) %>%
          gather(Variable, value, starts_with(selected_grouping_var)) %>%
          filter(!is.na(value)) %>%
          rename("use_or_uptake" = use_uptake_var,
                 "GrpVar" = "value",
                 "FacetVar" = selected_facet_var)
        
      } else {
        analysis_dataset <- ds1 %>%
          dplyr::select(unique(c(filter_vars, selected_facet_var)), use_uptake_var, selected_grouping_var) %>%
          rename("GrpVar" = selected_grouping_var,
                 "use_or_uptake" = use_uptake_var,
                 "FacetVar" = selected_facet_var)
      }
    }
    
    # If selected facet is country -- then create a new country variable
    if(selected_facet_var == "country"){
      
      analysis_dataset <- analysis_dataset %>%
        mutate(country = FacetVar)
      
    }
    
    # If selected facet is partner -- then createa new partner variable
    if(selected_facet_var == "partner"){
      
      analysis_dataset <- analysis_dataset %>%
        mutate(partner = FacetVar)
      
    }
    
    # return final analysis dataset
    analysis_dataset
  })
  
  #----------------------------------------------------------------------------
  # First tab: Dashboard tab
  #----------------------------------------------------------------------------
  
  # Total Nr of respondents by country
  ng_no <- n_country %>% filter(grepl("nigeria", country, ignore.case = T))
  tz_no <- n_country %>% filter(grepl("tanzania", country, ignore.case = T))
  ng_fperc <- n_gender_country %>% filter(grepl("nigeria", country, ignore.case = T))
  tz_fperc <- n_gender_country %>% filter(grepl("tanzania", country, ignore.case = T))
  
  output$ng_respondents <- renderValueBox({
    
    #flexdashboard::valueBox(42, paste0('Number of Cars',':','<br>','City ','is Chicago'))
    
    valueBox(value = tags$p(div(img(src = 'https://akilimo.sirv.com/images/ng.png', height = '50px', style = 'font-size: 16px', br(),br(),
                                    paste0('NIGERIA :', ' ', format( ng_no$n, big.mark = ',', scientific = FALSE), ' respondents (', scales::percent(ng_fperc$Percent),' female)')))),
             #   shinydashboard::valueBox(tags$p(tags$span("Hello"),tags$span("World!", style = "float:right"), style ="color : black"), "Hi!")
             
             subtitle = "",
             color = "green")
    # valueBox(
    #   value = tags$p(paste0(format(ng_no$n, big.mark = ',', scientific = FALSE), " (", scales::percent(ng_fperc$Percent)," female)"), div(img(src = "images/ng.png", height = "30px", style = "font-size: 16px"))),
    #   title = tags$p(paste0("Nigeria : Nr of respondents (", scales::percent(ng_no$Percent), " of total)"), style = "font-size: 16px")
    #   #icon = icon('id-card', lib = 'font-awesome'),
    #   #color = "green"
    # )
  })
  
  output$tz_respondents <- renderValueBox({
    
    valueBox(value = tags$p( div(img(src = "https://akilimo.sirv.com/images/tz.PNG", height = "50px", style = "font-size: 16px", br(),br(),
                                     paste0("TANZANIA :", " ", format(tz_no$n, big.mark = ',', scientific = FALSE), " respondents (", scales::percent(tz_fperc$Percent)," female)")))),
             #   
             subtitle = "",
             color = "teal")
    # valueBox(
    #   value = tags$p(paste0(format(ng_no$n, big.mark = ',', scientific = FALSE), " (", scales::percent(ng_fperc$Percent)," female)"), div(img(src = "images/ng.png", height = "30px", style = "font-size: 16px"))),
    #   title = tags$p(paste0("Nigeria : Nr of respondents (", scales::percent(ng_no$Percent), " of total)"), style = "font-size: 16px")
    #   #icon = icon('id-card', lib = 'font-awesome'),
    #   #color = "green"
    # )
  })
  
  # output$tz_respondents <- renderInfoBox({
  #   infoBox(
  #     value = tags$p(paste0(format(tz_no$n, big.mark = ',', scientific = FALSE), " (", scales::percent(tz_fperc$Percent)," female)"), style = "font-size: 16px"),
  #     title = tags$p(paste0("Tanzania : Nr of respondents (", scales::percent(tz_no$Percent), " of total)"), style = "font-size: 16px"),
  #     icon = icon('address-card', lib = 'font-awesome'),
  #     color = "blue"
  #   )
  # })
  
  # uptake and usage in Nigeria
  output$use_uptake_ng_gh <- renderPlot({
    
    gg <- ggPieDonutMine(ds1 %>% filter(country == "Nigeria"), aes(pies = use, donuts = uptake), colors = color) +
      ggtitle("Use \n Use in pies and Uptake in donuts") +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)))
    gg
    
  })
  
  
  output$uptake_use_ng_gh <- renderPlot({
    
    gg <- ggPieDonutMine(ds1 %>% filter(country == "Nigeria"), aes(pies = uptake, donuts = use), colors = color) +
      ggtitle("Uptake \n Uptake in pies and Uptake in donuts") +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)))
    gg
    
  })
  
  # uptake and usage in Tanzania
  output$use_uptake_tz_gh <- renderPlot({
    
    gg <- ggPieDonutMine(ds1 %>% filter(country == "Tanzania"), aes(pies = use, donuts = uptake), colors = color) +
      ggtitle("Use \n Use in pies and Uptake in donuts") +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)))
    gg
    
  })
  
  
  output$uptake_use_tz_gh <- renderPlot({
    
    gg <- ggPieDonutMine(ds1 %>% filter(country == "Tanzania"), aes(pies = uptake, donuts = use), colors = color) +
      ggtitle("Uptake \n Uptake in pies and Uptake in donuts") +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)))
    gg
    
  })
  
  # render description of the Low, Medium and High levels of use
  output$use_codes <- renderPlot({
    
    temp_df %>%
      ggplot(aes(x = DescriptionUse,
                 fill = DescriptionUse,
                 width = 0.4,
                 y = Percent)) +
      geom_bar(stat = "identity",
               position = "dodge") +
      labs(title = "Use") +
      geom_text(aes(label = DescriptionUse),
                size = 4, 
                hjust = 0,
                position = position_dodge2(width = 0.9)) +
      scale_fill_manual(values = color) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(size = rel(1.2), hjust = 0, face = "bold")) +
      coord_flip(ylim = c(0, 1))
    
  })
  
  # render description of the Low, Medium and High levels of uptake
  output$uptake_codes <- renderPlot({
    
    temp_df %>%
      ggplot(aes(x = DescriptionUptake,
                 fill = DescriptionUptake,
                 width = 0.4,
                 y = Percent)) +
      geom_bar(stat = "identity",
               position = "dodge") +
      labs(title = "Uptake") +
      geom_text(aes(label = DescriptionUptake),
                size = 4, 
                hjust = 0,
                position = position_dodge2(width = 0.9)) +
      scale_fill_manual(values = color) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(size = rel(1.2), hjust = 0, face = "bold")) +
      coord_flip(ylim = c(0, 1))
    
  })
  
  #---------------------------------------------------------------------------
  # Second tab : Uptake: of the most important use case
  #---------------------------------------------------------------------------
  
  ### Verbatim text -- returns NULL -- used for purposes of updating the reactive box width
  output$steps6_text <- renderText({
    
    # Logical checks
    country_miss1_1 <- (input$country_sel == "Compare countries")
    partner_miss1_1 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    default1_1 <- (country_miss1_1 & partner_miss1_1)
    country_only_sel1_1 <- (!country_miss1_1 & partner_miss1_1)
    all_sel1_1 <- (!country_miss1_1 & !partner_miss1_1)
    
    # selected facet // use case
    selected_use_case_col <- use_case_types[use_case_types == input$use_case_type]
    names(selected_use_case_col) <- NULL
    
    if(country_miss1_1) {
      selected_facet_col <- "country"
      selected_facet_name <- "country"
    } else {
      if(input$facet_sel1_2 == "None") {
        selected_facet_col <- "country"
        selected_facet_name <- "country"
      } else {
        selected_facet_name <- facet_all[facet_all == input$facet_sel1_2] %>% names(.) %>% tolower()
        selected_facet_col <- facet_all[facet_all == input$facet_sel1_2]
        names(selected_facet_col) <- NULL
      }
    }
    
    # Analysis dataset
    data_uptake <- data_uptake_full %>%
      rename("FacetVar" = selected_facet_col)
    
    if(selected_facet_col == "country") {
      data_uptake <- data_uptake %>%
        mutate(country = FacetVar)
    }
    
    if(selected_facet_col == "partner") {
      data_uptake <- data_uptake %>%
        mutate(partner = FacetVar)
    }
    
    # box width size
    if(default1_1) {
      
      plot_df_summarized <- data_uptake %>%
        filter(grepl(input$use_case_type, variable0, ignore.case = T)) %>%
        filter(!is.na(FacetVar))
      
    } else if(country_only_sel1_1) {
      
      plot_df_summarized <- data_uptake %>%
        filter(country == input$country_sel, grepl(input$use_case_type, variable0, ignore.case = T)) %>%
        filter(!is.na(FacetVar)) 
      
    } else if(all_sel1_1) {
      
      plot_df_summarized <- data_uptake %>%
        filter(partner == input$partner_sel, country == input$country_sel, grepl(input$use_case_type, variable0, ignore.case = T)) %>%
        filter(!is.na(FacetVar)) 
      
    }
    
    n_facets <- plot_df_summarized %>% filter(!is.na(FacetVar)) %>% pull(FacetVar) %>% unique() %>% length()
    
    
    if (n_facets == 1) {
      
      values$adaptive_width <- 3
      
    } else if (n_facets == 2) {
      
      values$adaptive_width <- 6
      
    } else if (n_facets == 3) {
      
      values$adaptive_width <- 9
      
    } else if (n_facets >= 4) {
      
      values$adaptive_width <- 12
      
    } else {
      
      values$adaptive_width <- 6
    }
    
    NULL
    
  })
  
  ### Box title
  output$title_uptake6 <- renderText({
    
    # Logical checks
    country_miss1_1 <- (input$country_sel == "Compare countries")
    partner_miss1_1 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    default1_1 <- (country_miss1_1 & partner_miss1_1)
    country_only_sel1_1 <- (!country_miss1_1 & partner_miss1_1)
    all_sel1_1 <- (!country_miss1_1 & !partner_miss1_1)
    
    # selected facet // use case
    selected_use_case_name <- use_case_types[use_case_types == input$use_case_type] %>% names(.)
    selected_use_case_col <- use_case_types[use_case_types == input$use_case_type]
    names(selected_use_case_col) <- NULL
    
    if(country_miss1_1) {
      selected_facet_col <- "country"
      selected_facet_name <- "country"
    } else {
      if(input$facet_sel1_2 == "None") {
        selected_facet_col <- "country"
        selected_facet_name <- "country"
      } else {
        selected_facet_name <- facet_all[facet_all == input$facet_sel1_2] %>% names(.) %>% tolower()
        selected_facet_col <- facet_all[facet_all == input$facet_sel1_1]
        names(selected_facet_col) <- NULL
      }
    }
    
    # box title 
    if(default1_1) {
      
      gtitle = ifelse(selected_facet_col == "country",
                      glue("Uptake of the Six Steps of {selected_use_case_name} in {input$country_sel}"),
                      glue("Uptake of the Six Steps of {selected_use_case_name} by {selected_facet_name}"))
      
    } else if(country_only_sel1_1) {
      
      gtitle = ifelse(selected_facet_col == "country",
                      glue("Uptake of the Six Steps of {selected_use_case_name} in {input$country_sel}"),
                      glue("Uptake of the Six Steps of {selected_use_case_name} by {selected_facet_name} in {input$country_sel}"))
      
    } else if(all_sel1_1) {
      
      gtitle = ifelse(selected_facet_col == "country",
                      glue("Uptake of the Six Steps of {selected_use_case_name} in {input$country_sel} for partner: {input$partner_sel}"),
                      glue("Uptake of the Six Steps of {selected_use_case_name} by {selected_facet_name} in {input$country_sel} for partner: {input$partner_sel}"))
    }
    
    #gtitle
    
  })
  
  ### Table with descriptions of the 6 steps
  output$step6_table <- renderTable({
    
    # Logical checks
    country_miss1_1 <- (input$country_sel == "Compare countries")
    partner_miss1_1 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    country_only_sel1_1 <- (!country_miss1_1 & partner_miss1_1)
    all_sel1_1 <- (!country_miss1_1 & !partner_miss1_1)
    
    # Selected use case
    selected_use_case_col <- use_case_types[use_case_types == input$use_case_type]
    names(selected_use_case_col) <- NULL
    
    # Do the filters return an empty dataset
    data_uptake <- data_uptake_full
    
    if(country_only_sel1_1) {
      
      data_uptake <- data_uptake %>%
        filter(country == input$country_sel, grepl(input$use_case_type, variable0, ignore.case = T))
      
    } else if(all_sel1_1) {
      
      data_uptake <- data_uptake %>%
        filter(partner == input$partner_sel, country == input$country_sel, grepl(input$use_case_type, variable0, ignore.case = T))
      
    }
    
    if(nrow(data_uptake) > 0) {
      
      most_imp_use_cases %>%
        filter(grepl(selected_use_case_col, newUseCaseCategory, ignore.case = T)) %>%
        select(Steps, Description)
      
    } else {
      NULL
    }
  }, spacing = "xs")
  
  ### Plot  ### 
  output$uptake_most_gh <- renderPlotly({
    
    # Logical checks
    country_miss1_1 <- (input$country_sel == "Compare countries")
    partner_miss1_1 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    default1_1 <- (country_miss1_1 & partner_miss1_1)
    country_only_sel1_1 <- (!country_miss1_1 & partner_miss1_1)
    all_sel1_1 <- (!country_miss1_1 & !partner_miss1_1)
    
    # selected facet // use case
    selected_use_case_col <- use_case_types[use_case_types == input$use_case_type]
    names(selected_use_case_col) <- NULL
    
    if(country_miss1_1) {
      selected_facet_col <- "country"
      selected_facet_name <- "country"
    } else {
      if(input$facet_sel1_2 == "None") {
        selected_facet_col <- "country"
        selected_facet_name <- "country"
      } else {
        selected_facet_name <- facet_all[facet_all == input$facet_sel1_2] %>% names(.) %>% tolower()
        selected_facet_col <- facet_all[facet_all == input$facet_sel1_2]
        names(selected_facet_col) <- NULL
      }
    }
    
    # Analysis dataset
    data_uptake <- data_uptake_full %>%
      rename("FacetVar" = selected_facet_col)
    
    if(selected_facet_col == "country") {
      data_uptake <- data_uptake %>%
        mutate(country = FacetVar)
    }
    
    if(selected_facet_col == "partner") {
      data_uptake <- data_uptake %>%
        mutate(partner = FacetVar)
    }
    
    # default -- no selections / facets by country by default
    if(default1_1) {
      
      # plotting dataset
      plot_df_summarized <- data_uptake %>%
        filter(grepl(input$use_case_type, variable0, ignore.case = T)) %>%
        filter(!is.na(FacetVar)) %>%
        select(FacetVar, variable, value) %>%
        count(FacetVar, variable, value) %>%
        group_by(variable, FacetVar) %>%
        mutate(Percent = round(n/sum(n), 4)) %>%
        ungroup() %>%
        group_by(FacetVar) %>%
        mutate(Total = sum(n)) %>%
        ungroup() %>%
        mutate(FacetVar = glue("{FacetVar} (n = {Total})"))
      
      # check number of rows
      validate(need(nrow(plot_df_summarized) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df_summarized$FacetVar))
      
      if(n_facets <= 2) {
        n_cols <- 2
      } else if (n_facets %in% c(3, 5, 6)) {
        n_cols <- 3
      } else if (n_facets == 4 | between(n_facets, 7, 9)){
        n_cols <- 4
      } else if (n_facets >= 10){
        n_cols <- 5
      }
      
      # plot
      gg <- plot_df_summarized %>%
        ggplot(aes(x = variable,
                   fill = value,
                   y = Percent,
                   text = glue("Uptake: {value}
                                Count: {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
        geom_bar(colour = "black",
                 stat = "identity",
                 position = position_stack(reverse = TRUE, vjust = 0.075),
                 size = 0.3) +
        labs(x = "",
             y = "") +
        coord_cartesian(ylim = c(0, 1.05)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                           breaks = seq(0, 1.05, 0.2), expand = c(0, 0)) +
        scale_fill_manual(values = color) +
        my_theme +
        facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
        theme(legend.position = "none",
              axis.ticks.x = element_blank(), axis.ticks.length.x = unit(3.25, "cm"))
      
      ggplotly(gg, tooltip = c("text")) %>%
        layout(xaxis = list(automargin = TRUE),
               yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
               margin = list(b = 90))
    }
    
    ## filter by country only
    else if(country_only_sel1_1) {
      
      # plotting dataset
      plot_df_summarized <- data_uptake %>%
        filter(country == input$country_sel, grepl(input$use_case_type, variable0, ignore.case = T)) %>%
        filter(!is.na(FacetVar)) %>%
        select(FacetVar, variable, value) %>%
        count(variable, FacetVar, value) %>%
        group_by(variable, FacetVar) %>%
        mutate(Percent = round(n/sum(n), 4)) %>%
        ungroup() %>%
        group_by(FacetVar) %>%
        mutate(Total = sum(n)) %>%
        ungroup() %>%
        mutate(FacetVar = glue("{FacetVar} (n = {Total})"))
      
      # check number of rows
      validate(need(nrow(plot_df_summarized) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df_summarized$FacetVar))
      
      if(n_facets <= 2) {
        n_cols <- 2
      } else if (n_facets %in% c(3, 5, 6)) {
        n_cols <- 3
      } else if (n_facets == 4 | between(n_facets, 7, 9)){
        n_cols <- 4
      } else if (n_facets >= 10){
        n_cols <- 5
      }
      
      # plot
      gg <- plot_df_summarized %>%
        ggplot(aes(x = variable,
                   fill = value,
                   y = Percent,
                   text = glue("Uptake: {value}
                                Count: {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
        geom_bar(colour = "black",
                 stat = "identity",
                 position = position_stack(reverse = TRUE, vjust = 0.075),
                 size = 0.3) +
        labs(x = "",
             y = "") +
        coord_cartesian(ylim = c(0, 1.05)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                           breaks = seq(0, 1.05, 0.2), expand = c(0, 0)) +
        scale_fill_manual(values = color) +
        my_theme +
        facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
        theme(legend.position = "none",
              axis.ticks.x = element_blank(), axis.ticks.length.x = unit(3.25, "cm"))
      
      ggplotly(gg, tooltip = c("text")) %>%
        layout(xaxis = list(automargin = TRUE),
               yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
               margin = list(b = 90))
    }
    
    ## filter by partner and country and/or facet
    else if(all_sel1_1) {
      
      # plotting dataset
      plot_df_summarized <- data_uptake %>%
        filter(partner == input$partner_sel, country == input$country_sel, grepl(input$use_case_type, variable0, ignore.case = T)) %>%
        filter(!is.na(FacetVar)) %>%
        select(FacetVar, variable, value) %>%
        count(variable, FacetVar, value) %>%
        group_by(variable, FacetVar) %>%
        mutate(Percent = round(n/sum(n), 4)) %>%
        ungroup() %>%
        group_by(FacetVar) %>%
        mutate(Total = sum(n)) %>%
        ungroup() %>%
        mutate(FacetVar = glue("{FacetVar} (n = {Total})"))
      
      # check number of rows
      validate(need(nrow(plot_df_summarized) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df_summarized$FacetVar))
      
      if(n_facets <= 2) {
        n_cols <- 2
      } else if (n_facets %in% c(3, 5, 6)) {
        n_cols <- 3
      } else if (n_facets == 4 | between(n_facets, 7, 9)){
        n_cols <- 4
      } else if (n_facets >= 10){
        n_cols <- 5
      }
      
      # plot
      gg <- plot_df_summarized %>%
        ggplot(aes(x = variable,
                   fill = value,
                   y = Percent,
                   text = glue("Uptake: {value}
                                Count: {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
        geom_bar(colour = "black",
                 stat = "identity",
                 position = position_stack(reverse = TRUE, vjust = 0.075),
                 size = 0.3) +
        labs(x = "",
             y = "") +
        coord_cartesian(ylim = c(0, 1.05)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                           breaks = seq(0, 1.05, 0.2), expand = c(0, 0)) +
        scale_fill_manual(values = color) +
        my_theme +
        facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
        theme(legend.position = "none",
              axis.ticks.x = element_blank(), axis.ticks.length.x = unit(3.25, "cm"))
      
      if(n_facets < 2) {
        gg <- gg +         
          labs(y = "")
      }
      
      ggplotly(gg, tooltip = c("text")) %>%
        layout(xaxis = list(automargin = TRUE),
               yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
               margin = list(b = 90))
    }
  })
  
  #---------------------------------------------------------------------------
  # Third tab : Use and uptake analysis : overall and by covariates
  #---------------------------------------------------------------------------
  
  ### Verbatim text -- for purposes of updating the reactive box width
  output$covariates_text <- renderText({
    
    # Logical checks
    country_miss1_0 <- (input$country_sel == "Compare countries")
    partner_miss1_0 <- (input$partner_sel == "All partners")
    subgroup_miss1_0 <- (input$group_sel1_0 == "None")
    
    # Combination of conditions
    country_only_sel1_0 <- (!country_miss1_0 & partner_miss1_0)
    country_partner_sel <- (!country_miss1_0 & !partner_miss1_0)
    
    # Analysis dataset for different scenarios
    analysis_dataset <- analysisData()
    
    if(country_only_sel1_0) {
      
      analysis_dataset <- analysis_dataset %>%
        filter(country == input$country_sel) 
      
    } else if (country_partner_sel) {
      
      analysis_dataset <- analysis_dataset %>%
        filter(country == input$country_sel, partner == input$partner_sel) 
      
    } else if (subgroup_miss1_0) {
      analysis_dataset <- analysis_dataset %>%
        mutate(GrpVar = as.factor(1))
    }
    
    # for proportion and frequency charts use numbers of facets to adjust width
    # for pie charts use number of groups
    n_groups <- analysis_dataset %>% filter(!is.na(GrpVar)) %>% pull(GrpVar) %>% unique() %>% length()
    n_facets <- analysis_dataset %>% filter(!is.na(FacetVar)) %>% pull(FacetVar) %>% unique() %>% length()
    
    if(input$plot_type2 == "Pie"){
      
      if (subgroup_miss1_0) {
        
        n_levels = 1
        
      } else {
        
        n_levels <- n_groups
        
      }
      
    } else {
      
      if(n_groups >= 8) {
        
        n_levels <- n_groups
        
      } else {
        
        n_levels <- n_facets
        
      }
      
    }
    
    # reactive box width
    if (n_levels == 1) {
      
      values$adaptive_width <- 3
      
    } else if (n_levels == 2) {
      
      values$adaptive_width <- 6
      
    } else if (n_levels == 3) {
      
      values$adaptive_width <- 9
      
    } else if (n_levels >= 4) {
      
      values$adaptive_width <- 12
      
    }
    
    # reactive plot height
    if(n_groups >= 7 & between(n_facets, 4, 8)) {
      
      values$adaptive_height = "600px"
      
    } else if(n_groups >= 7 & n_facets > 8) {
      
      values$adaptive_height = "800px"
      
    }
    
    NULL
  })
  
  ### Box title ###
  output$title_by_covariates <- renderText({
    # Logical checks
    #country_miss1_0 <- "Compare countries"
    country_miss1_0 <- (input$country_sel == "Compare countries")
    partner_miss1_0 <- (input$partner_sel == "All partners")
    subgroup_miss1_0 <- (input$group_sel1_0 == "None")
    
    # Combination of conditions
    default1_0 <- (country_miss1_0 & partner_miss1_0 & subgroup_miss1_0)
    country_only_sel1_0 <- (!country_miss1_0 & partner_miss1_0 & subgroup_miss1_0)
    subgroup_only_sel1_0 <- (country_miss1_0 & partner_miss1_0 & !subgroup_miss1_0)
    country_partner_sel <- (!country_miss1_0 & !partner_miss1_0 & subgroup_miss1_0)
    country_subgroup_sel1_0 <- (!country_miss1_0 & partner_miss1_0 & !subgroup_miss1_0)
    all_sel1_0 <- (!country_miss1_0 & !partner_miss1_0 & !subgroup_miss1_0)
    
    # use or uptake // selected facet // selected grouping
    if(input$country_sel == "Compare countries") {
    selected_grouping <- subgroups_all[subgroups_all == input$group_sel1_1] %>% names(.) %>% tolower()
    selected_grouping_var <- subgroups_all[subgroups_all == input$group_sel1_1]
    names(selected_grouping_var) <- NULL
    
    }else if(input$country_sel != "Compare countries") {
      selected_grouping <- subgroups_all[subgroups_all == input$group_sel1_0] %>% names(.) %>% tolower()
      selected_grouping_var <- subgroups_all[subgroups_all == input$group_sel1_0]
      names(selected_grouping_var) <- NULL
    
    }
    
    
    if(input$country_sel == "Compare countries") {
      selected_facet <- "country"
      selected_facet_var <- "country"
    } else {
      if(input$facet_sel1_1 == "None") {
        selected_facet <- "country"
        selected_facet_var <- "country"
      } else {
        selected_facet <- facet_all[facet_all == input$facet_sel1_1] %>% names(.) %>% tolower()
        selected_facet_var <- facet_all[facet_all == input$facet_sel1_1]
        names(selected_facet_var) <- NULL
      }
    }
    
    if(default1_0) {
      
      gtitle <- glue("{input$use_uptake} of AKILIMO tools by {tolower(selected_facet)}")
      
    } else if(subgroup_only_sel1_0) {
      
      gtitle = glue("{input$use_uptake} of AKILIMO tools by {selected_grouping} and {selected_facet}")
      
    } else if(country_only_sel1_0) {
      
      gtitle = ifelse(input$facet_sel1_1 == "country",
                      glue("{input$use_uptake} of AKILIMO tools in {input$country_sel}"),
                      glue("{input$use_uptake} of AKILIMO tools by {selected_facet} in {input$country_sel}"))
      
    } else if(country_subgroup_sel1_0) {
      
      gtitle = ifelse(input$facet_sel1_1 == "country",
                      glue("{input$use_uptake} of AKILIMO tools by {selected_grouping} in {input$country_sel}"),
                      glue("{input$use_uptake} of AKILIMO tools by {selected_facet} and {selected_grouping} in {input$country_sel}"))
      
    } else if(country_partner_sel) {
      
      gtitle = ifelse(input$facet_sel1_1 == "country",
                      glue("{input$use_uptake} of AKILIMO tools in {input$country_sel} for partner: {input$partner_sel}"),
                      glue("{input$use_uptake} of AKILIMO tools by {selected_facet} in {input$country_sel} for partner: {input$partner_sel}"))
      
    } else if(all_sel1_0) {
      
      gtitle = ifelse(input$facet_sel1_1 == "country",
                      glue("{input$use_uptake} of AKILIMO tools by {selected_grouping} in {input$country_sel} for partner: {input$partner_sel}"),
                      glue("{input$use_uptake} of AKILIMO tools by {selected_facet} and {selected_grouping} in {input$country_sel} for partner: {input$partner_sel}"))
    }
    
    #gtitle
    
  })
  
  ### Plot ###
  output$use_uptake_gh <- renderPlotly({
    
    # Logical checks
    country_miss1_0 <- (input$country_sel == "Compare countries")
    partner_miss1_0 <- (input$partner_sel == "All partners")
    subgroup_miss1_0 <- (input$group_sel1_0 == "None")
    
  # country_sel = "Compare countries"
  #  partner_sel = "All partners"
  #  group_sel1_0 = "None"
  # use_uptake = "Use"

# 
#     country_miss1_0 <- country_sel == "Compare countries"
#     partner_miss1_0 <- partner_sel == "All partners"
#     subgroup_miss1_0 <- group_sel1_0 == "None"

    # Combination of conditions
    default1_0 <- (country_miss1_0 & partner_miss1_0 & subgroup_miss1_0)
    country_only_sel1_0 <- (!country_miss1_0 & partner_miss1_0 & subgroup_miss1_0)
    subgroup_only_sel1_0 <- (country_miss1_0 & partner_miss1_0 & !subgroup_miss1_0)
    country_partner_sel <- (!country_miss1_0 & !partner_miss1_0 & subgroup_miss1_0)
    country_subgroup_sel1_0 <- (!country_miss1_0 & partner_miss1_0 & !subgroup_miss1_0)
    all_sel1_0 <- (!country_miss1_0 & !partner_miss1_0 & !subgroup_miss1_0)
    
    # use or uptake // selected facet // selected grouping

    #  use_uptake_var <- tolower(use_uptake)
    # # 
    # selected_grouping <- subgroups_all[subgroups_all == "useCase"] %>% names(.) %>% tolower()
    # selected_grouping_var <- subgroups_all[subgroups_all == "useCase"]
    # names(selected_grouping_var) <- NULL
    
    use_uptake_var <- tolower(input$use_uptake)
    
    # use or uptake // selected facet // selected grouping
    selected_grouping <- subgroups_all[subgroups_all == input$group_sel1_1] %>% names(.) %>% tolower()
    selected_grouping_var <- subgroups_all[subgroups_all == input$group_sel1_1]
    names(selected_grouping_var) <- NULL
    
  
    if(input$country_sel == "Compare countries") {
      selected_facet <- "Country"
      selected_facet_var <- "country"
    } else {
      if(input$facet_sel1_1 == "None") {
        selected_facet <- "Country"
        selected_facet_var <- "country"
      } else {
        selected_facet <- facet_all[facet_all == input$facet_sel1_1] %>% names(.) %>% tolower()
        selected_facet_var <- facet_all[facet_all == input$facet_sel1_1]
        names(selected_facet_var) <- NULL
      }
    }
    
    # if(country_sel == "Compare countries") {
    #   selected_facet <- "Country"
    #   selected_facet_var <- "country"
    # } else {
    #   if(facet_sel1_1 == "None") {
    #     selected_facet <- "Country"
    #     selected_facet_var <- "country"
    #   } else {
    #     selected_facet <- facet_all[facet_all == facet_sel1_1] %>% names(.) %>% tolower()
    #     selected_facet_var <- facet_all[facet_all == facet_sel1_1]
    #     names(selected_facet_var) <- NULL
    #   }
    # }
    
    # Analysis data
    analysis_dataset <- analysisData()
    
    ## default -- no selections
    if(default1_0 | country_miss1_0) {
      
      # new analysis dataset
      analysis_dataset2 <- ds1 %>%
        dplyr::select(selected_facet_var, use_uptake_var, selected_grouping_var) %>%
        rename("GrpVar" = selected_grouping_var,
               "use_or_uptake" = use_uptake_var,
               "FacetVar" = selected_facet_var)
      
      if(selected_facet_var == "country"){
        analysis_dataset <- analysis_dataset %>%
          mutate(country = FacetVar)
      }
      
      print(input$group_sel1_1)
      
      # plotting dataset
      if(input$group_sel1_1 == "None") {
        
        plot_df <- analysis_dataset2 %>%
          filter(!is.na(FacetVar)) %>%
          count(FacetVar, use_or_uptake) %>%
          mutate(GrpVar = as.factor(1)) %>%
          group_by(FacetVar, GrpVar) %>%
          mutate(Percent = n/sum(n),
                 Total = sum(n)) %>%
          ungroup() 
        
      } else {
        
        plot_df <- analysis_dataset2 %>%
          filter(!is.na(FacetVar)) %>%
          count(FacetVar, GrpVar, use_or_uptake) %>%
          group_by(FacetVar, GrpVar) %>%
          mutate(Percent = round(n/sum(n), 3),
                 Total = sum(n)) %>%
          ungroup()
        
      } 
      
      # check number of rows
      validate(need(nrow(plot_df) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df$FacetVar))
      
      if(n_facets <= 2) {
        n_cols <- 2
      } else if (n_facets %in% c(3, 5, 6)) {
        n_cols <- 3
      } else if (n_facets == 4 | between(n_facets, 7, 9)){
        n_cols <- 4
      } else if (n_facets >= 10){
        n_cols <- 4
      }
      
      # a. Proportion bar
      if(input$plot_type2 == "Proportion bar") {
        
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = Percent,
                     text = glue("{input$use_uptake}: {use_or_uptake}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position =  position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          geom_text(aes(y = 1.04,
                        label = Total, fontface = "plain"),
                    size = 3.5) +
          # coord_cartesian(ylim = c(0, 1.15)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                             breaks = seq(0, 1.15, 0.2), expand = c(0, 0)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
          theme(legend.position = "none") +
          coord_flip(ylim = c(0, 1.15))
        #axis.text.x = element_blank())
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
        
      }
      
      
      # b. Frequency
      else if (input$plot_type2 == "Frequency bar") {
        
        # y -axis upper bound
        y_upper_bound <- plot_df %>%
          pull(Total) %>%
          max()
        
        # plot
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = n,
                     text = glue("{use_uptake_var}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position =  position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          #coord_cartesian(ylim = c(0, y_upper_bound)) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left", scales = "free_y") +
          theme(legend.position = "none")+
          coord_flip()
        #axis.text.x = element_blank())
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Nr of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
      }
      
      
      # c. pie
      else if (input$plot_type2 == "Pie")  {
        
        facets <- unique(plot_df$FacetVar) %>% as.character()
        subgroups <- unique(plot_df$GrpVar) %>% as.character()
        n_facets <- length(facets)
        n_groups <- length(subgroups)
        
        code_lst <- list()
        
        for(j in seq_len(length(subgroups))){
          
          code_lst_j <- list()
          
          for(i in seq_len(length(facets))){
            
            facet <- facets[i]
            group <- subgroups[j]
            
            if(i == 1) {
              sb_title <- paste(group, "\n", facet)
            } else {
              sb_title <- paste(facet)
            }
            
            # prepare code
            code_snippet <- paste0('fig <- fig %>% add_pie(data = plot_df %>% filter(FacetVar == "', facet, '", GrpVar == "', group, '"), labels = ~use_or_uptake, values = ~n,
                               title = "', sb_title , '", domain = list(row = ', i-1, ', column = ', j-1, '),
                               textinfo = "percent",
                               hoverinfo = "text",
                               insidetextorientation = "horizontal",
                               text = ~glue("Use: {use_or_uptake}
                                              n = {n}
                                              Percent: {scales::percent(Percent, accuracy = .01)}"),
                               marker = list(colors = color))')
            
            code_lst_j[[i]] <- code_snippet
          }
          code_lst[[j]] <- paste0(unlist(code_lst_j), collapse = ";")
        }
        
        
        
        code_snippet_final <- paste0('fig <- plot_ly();',  paste0(unlist(code_lst), collapse = ";"), '; fig <- fig %>% layout(
                              showlegend = F,
                              grid = list(rows = ', n_facets, ', columns = ', n_groups, '))')
        
        fig <- eval(parse(text = code_snippet_final))
        
        fig
        
      }
      
    }
    
    ## filter by disaggregating variable
    else if(subgroup_only_sel1_0) {
      # default1_0 <- (country_miss1_0 & partner_miss1_0 & subgroup_miss1_0)
      # country_only_sel1_0 <- (!country_miss1_0 & partner_miss1_0 & subgroup_miss1_0)
      # subgroup_only_sel1_0 <- (country_miss1_0 & partner_miss1_0 & !subgroup_miss1_0)
      # country_partner_sel <- (!country_miss1_0 & !partner_miss1_0 & subgroup_miss1_0)
      # country_subgroup_sel1_0 <- (!country_miss1_0 & partner_miss1_0 & !subgroup_miss1_0)
      # all_sel1_0 <- (!country_miss1_0 & !partner_miss1_0 & !subgroup_miss1_0)
      # Analysis data
      analysis_dataset <- analysisData()
      # plotting dataset
      plot_df <- analysis_dataset %>%
        filter(!is.na(FacetVar)) %>%
        count(FacetVar, GrpVar, use_or_uptake) %>%
        group_by(FacetVar, GrpVar) %>%
        mutate(Percent = round(n/sum(n), 3),
               Total = sum(n)) %>%
        ungroup()
      
      # check number of rows
      validate(need(nrow(plot_df) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df$FacetVar))
      n_groups <- length(unique(plot_df$GrpVar))
      
      if(n_groups <= 6) {
        
        if(n_facets <= 2) {
          n_cols <- 2
        } else if (n_facets %in% c(3, 5, 6)) {
          n_cols <- 3
        } else if (n_facets == 4 | between(n_facets, 7, 9)){
          n_cols <- 4
        } else if (n_facets >= 10){
          n_cols <- 4
        }
        
      } else {
        
        n_cols <- 2
      }
      
      # a. proportion bar
      if(input$plot_type == "Proportion bar") {
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = Percent,
                     text = glue("{input$use_uptake}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          geom_text(aes(y = 1.04,
                        label = Total, fontface = "plain"),
                    size = 3.5) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
          coord_cartesian(ylim = c(0, 1.1)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                             breaks = seq(0, 1.1, 0.2), expand = c(0, 0)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
          theme(legend.position = "none")
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
        
      }else if (input$plot_type == "Frequency bar") {
        
        # y -axis upper bound
        y_upper_bound <- plot_df %>%
          pull(Total) %>%
          max()
        
        # plot
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = n,
                     text = glue("{use_uptake_var}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          #coord_cartesian(ylim = c(0, y_upper_bound)) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left", scales = "free_y") +
          theme(legend.position = "none")
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Nr of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
      }else if (input$plot_type == "Pie")  {
        
        facets <- unique(plot_df$FacetVar) %>% as.character()
        subgroups <- unique(plot_df$GrpVar) %>% as.character()
        n_facets <- length(facets)
        n_groups <- length(subgroups)
        
        code_lst <- list()
        
        for(j in seq_len(length(subgroups))){
          
          code_lst_j <- list()
          
          for(i in seq_len(length(facets))){
            
            facet <- facets[i]
            group <- subgroups[j]
            
            if(i == 1) {
              sb_title <- paste(group, "\n", facet)
            } else {
              sb_title <- paste(facet)
            }
            
            # prepare code
            code_snippet <- paste0('fig <- fig %>% add_pie(data = plot_df %>% filter(FacetVar == "', facet, '", GrpVar == "', group, '"), labels = ~use_or_uptake, values = ~n,
                               title = "', sb_title , '", domain = list(row = ', i-1, ', column = ', j-1, '),
                               textinfo = "percent",
                               hoverinfo = "text",
                               insidetextorientation = "horizontal",
                               text = ~glue("Use: {use_or_uptake}
                                              n = {n}
                                              Percent: {scales::percent(Percent, accuracy = .01)}"),
                               marker = list(colors = color))')
            
            code_lst_j[[i]] <- code_snippet
          }
          code_lst[[j]] <- paste0(unlist(code_lst_j), collapse = ";")
        }
        
        
        
        code_snippet_final <- paste0('fig <- plot_ly();',  paste0(unlist(code_lst), collapse = ";"), '; fig <- fig %>% layout(
                              showlegend = F,
                              grid = list(rows = ', n_facets, ', columns = ', n_groups, '))')
        
        fig <- eval(parse(text = code_snippet_final))
        
        fig
        
      }
      
    }
    
    ## filter by country only
    else if(country_only_sel1_0) {

   
      # plotting dataset
      plot_df <- analysis_dataset %>%
        filter(!is.na(FacetVar)) %>%
        filter(country == input$country_sel) %>%
        count(FacetVar, use_or_uptake) %>%
        mutate(GrpVar = as.factor(1)) %>%
        group_by(FacetVar, GrpVar) %>%
        mutate(Percent = n/sum(n),
               Total = sum(n)) %>%
        ungroup()  
      
      # check number of rows
      validate(need(nrow(plot_df) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df$FacetVar))
      
      if(n_facets <= 2) {
        n_cols <- 2
      } else if (n_facets %in% c(3, 5, 6)) {
        n_cols <- 3
      } else if (n_facets == 4 | between(n_facets, 7, 9)){
        n_cols <- 4
      } else if (n_facets >= 10){
        n_cols <- 5
      }
      
      # a. Proportion bar
      if(input$plot_type == "Proportion bar") {
        
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = Percent,
                     text = glue("{input$use_uptake}: {use_or_uptake}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          geom_text(aes(y = 1.04,
                        label = Total, fontface = "plain"),
                    size = 3.5) +
          coord_cartesian(ylim = c(0, 1.15)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                             breaks = seq(0, 1.15, 0.2), expand = c(0, 0)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
          theme(legend.position = "none", 
                axis.text.x = element_blank())
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
      }
      
      # b. Frequency
      else if (input$plot_type == "Frequency bar") {
        
        # y -axis upper bound
        y_upper_bound <- plot_df %>%
          pull(Total) %>%
          max()
        
        # plot
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = n,
                     text = glue("{use_uptake_var}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          #coord_cartesian(ylim = c(0, y_upper_bound)) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left", scales = "free_y") +
          theme(legend.position = "none", 
                axis.text.x = element_blank())
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Nr of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
      }
      
      # c. pie
      else if (input$plot_type == "Pie")  {
        
        facets <- unique(plot_df$FacetVar) %>% as.character()
        n_facets <- length(facets)
        
        code_lst <- list()
        for(i in seq_len(length(facets))){
          
          facet <- facets[i]
          
          # prepare code
          code_snippet <- paste0('fig <- fig %>% add_pie(data = plot_df %>% filter(FacetVar == "', facet, '"), labels = ~use_or_uptake, values = ~n,
                               title = "', facet, '", domain = list(row = ', i-1, ', column = 0),
                               textinfo = "percent",
                               hoverinfo = "text",
                               insidetextorientation = "horizontal",
                               text = ~glue("Use: {use_or_uptake}
                                              n = {n}
                                              Percent: {scales::percent(Percent, accuracy = .01)}"),
                               marker = list(colors = color))')
          
          code_lst[[i]] <- code_snippet
        }
        
        
        
        code_snippet_final <- paste0('fig <- plot_ly();',  paste0(unlist(code_lst), collapse = ";"), '; fig <- fig %>% layout(
                              showlegend = F,
                              grid = list(rows = ', n_facets, ', columns = 1))')
        
        fig <- eval(parse(text = code_snippet_final))
        
        fig
        
      }
    }
    
    ## filter by country and disaggregate by variable
    else if(country_subgroup_sel1_0) {
      
      # plotting dataset
      plot_df <- analysis_dataset %>%
        filter(!is.na(FacetVar)) %>%
        filter(country == input$country_sel) %>%
        count(FacetVar, GrpVar, use_or_uptake) %>%
        group_by(FacetVar, GrpVar) %>%
        mutate(Percent = n/sum(n),
               Total = sum(n)) %>%
        ungroup()
      
      # plot_df <- analysis_dataset %>%
      #   filter(!is.na(FacetVar)) %>%
      #   filter(country == "Nigeria") %>%
      #   count(FacetVar, GrpVar, use_or_uptake) %>%
      #   group_by(FacetVar, GrpVar) %>%
      #   mutate(Percent = n/sum(n),
      #          Total = sum(n)) %>%
      #   ungroup()
      
      
      # check number of rows
      validate(need(nrow(plot_df) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df$FacetVar))
      n_groups <- length(unique(plot_df$GrpVar))
      
      if(n_facets <= 2) {
        n_cols <- 2
      } else if (n_facets %in% c(3, 5, 6)) {
        n_cols <- 3
      } else if (n_facets == 4 | between(n_facets, 7, 9)){
        n_cols <- 4
      } else if (n_facets >= 10){
        n_cols <- 4
      }
      
      # a. proportion bar
      if(input$plot_type == "Proportion bar") {
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = Percent,
                     text = glue("{input$use_uptake}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          geom_text(aes(y = 1.06,
                        label = Total, fontface = "plain"),
                    size = 3) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
          theme(legend.position = "none")
        
        if(n_groups >= 5 & n_facets > 2) {
          gg <- gg +
            scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                               breaks = seq(0, 1.15, 0.4), expand = c(0, 0)) +
            coord_flip(ylim = c(0, 1.5))
        } else {
          gg <- gg + 
            coord_cartesian(ylim = c(0, 1.15)) +
            scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                               breaks = seq(0, 1.15, 0.2), expand = c(0, 0)) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
        }
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
        
      }else if (input$plot_type == "Frequency bar") {
        
        # y -axis upper bound
        y_upper_bound <- plot_df %>%
          pull(Total) %>%
          max()
        
        # plot
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = n,
                     text = glue("{use_uptake_var}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          scale_y_continuous(expand = c(0, 0)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left", scales = "free_y") +
          theme(legend.position = "none")
        
        
        if(n_groups >= 5 & n_facets > 2) {
          gg <- gg +
            coord_flip()
        } else {
          gg <- gg + 
            #coord_cartesian(ylim = c(0, y_upper_bound)) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
        }
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Nr of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
      }else if (input$plot_type == "Pie")  {
        
        facets <- unique(plot_df$FacetVar) %>% as.character()
        subgroups <- unique(plot_df$GrpVar) %>% as.character()
        n_facets <- length(facets)
        n_groups <- length(subgroups)
        
        validate(need(n_groups < 10, "Too many levels of grouping variable. Cannot display graph. Try selecting another variable."))
        
        
        code_lst <- list()
        
        for(j in seq_len(length(subgroups))){
          
          code_lst_j <- list()
          
          for(i in seq_len(length(facets))){
            
            facet <- facets[i]
            group <- subgroups[j]
            
            if(i == 1) {
              sb_title <- paste(group, "\n", facet)
            } else {
              sb_title <- paste(facet)
            }
            
            # prepare code
            code_snippet <- paste0('fig <- fig %>% add_pie(data = plot_df %>% filter(FacetVar == "', facet, '", GrpVar == "', group, '"), labels = ~use_or_uptake, values = ~n,
                               title = "', sb_title , '", domain = list(row = ', i-1, ', column = ', j-1, '),
                               textinfo = "percent",
                               hoverinfo = "text",
                               insidetextorientation = "horizontal",
                               text = ~glue("Use: {use_or_uptake}
                                              n = {n}
                                              Percent: {scales::percent(Percent, accuracy = .01)}"),
                               marker = list(colors = color))')
            
            code_lst_j[[i]] <- code_snippet
          }
          code_lst[[j]] <- paste0(unlist(code_lst_j), collapse = ";")
        }
        
        
        
        code_snippet_final <- paste0('fig <- plot_ly();',  paste0(unlist(code_lst), collapse = ";"), '; fig <- fig %>% layout(
                              showlegend = F,
                              grid = list(rows = ', n_facets, ', columns = ', n_groups, '))')
        
        fig <- eval(parse(text = code_snippet_final))
        
        fig
        
      }
    }else if(country_partner_sel) {
      
      # plotting dataset
      plot_df <- analysis_dataset %>%
        filter(!is.na(FacetVar)) %>%
        filter(country == input$country_sel, partner == input$partner_sel) %>%
        count(FacetVar, use_or_uptake) %>%
        mutate(GrpVar = as.factor(1)) %>%
        group_by(FacetVar, GrpVar) %>%
        mutate(Percent = n/sum(n),
               Total = sum(n)) %>%
        ungroup()  
      
      # check number of rows
      validate(need(nrow(plot_df) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df$FacetVar))
      
      if(n_facets <= 2) {
        n_cols <- 2
      } else if (n_facets %in% c(3, 5, 6)) {
        n_cols <- 3
      } else if (n_facets == 4 | between(n_facets, 7, 9)){
        n_cols <- 4
      } else if (n_facets >= 10){
        n_cols <- 4
      }
      
      # a. Proportion bar
      if(input$plot_type == "Proportion bar") {
        
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = Percent,
                     text = glue("{input$use_uptake}: {use_or_uptake}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          geom_text(aes(y = 1.06,
                        label = Total, fontface = "plain"),
                    size = 3.5) +
          coord_cartesian(ylim = c(0, 1.15)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                             breaks = seq(0, 1.15, 0.2), expand = c(0, 0)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
          theme(legend.position = "none", 
                axis.text.x = element_blank())
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
      }else if (input$plot_type == "Frequency bar") {
        
        # y -axis upper bound
        y_upper_bound <- plot_df %>%
          pull(Total) %>%
          max()
        
        # plot
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = n,
                     text = glue("{use_uptake_var}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          #coord_cartesian(ylim = c(0, y_upper_bound)) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left", scales = "free_y") +
          theme(legend.position = "none", 
                axis.text.x = element_blank())
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Nr of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
      }else if (input$plot_type == "Pie")  {
        
        facets <- unique(plot_df$FacetVar) %>% as.character()
        n_facets <- length(facets)
        
        code_lst <- list()
        for(i in seq_len(length(facets))){
          
          facet <- facets[i]
          
          # prepare code
          code_snippet <- paste0('fig <- fig %>% add_pie(data = plot_df %>% filter(FacetVar == "', facet, '"), labels = ~use_or_uptake, values = ~n,
                               title = "', facet, '", domain = list(row = ', i-1, ', column = 0),
                               textinfo = "percent",
                               hoverinfo = "text",
                               insidetextorientation = "horizontal",
                               text = ~glue("Use: {use_or_uptake}
                                              n = {n}
                                              Percent: {scales::percent(Percent, accuracy = .01)}"),
                               marker = list(colors = color))')
          
          code_lst[[i]] <- code_snippet
        }
        
        
        
        code_snippet_final <- paste0('fig <- plot_ly();',  paste0(unlist(code_lst), collapse = ";"), '; fig <- fig %>% layout(
                              showlegend = F,
                              grid = list(rows = ', n_facets, ', columns = 1))')
        
        fig <- eval(parse(text = code_snippet_final))
        
        fig
        
      }
    }else if(all_sel1_0) {
      
      # plotting dataset
      plot_df <- analysis_dataset %>%
        filter(!is.na(FacetVar)) %>%
        filter(country == input$country_sel, partner == input$partner_sel) %>%
        count(FacetVar, GrpVar, use_or_uptake) %>%
        group_by(FacetVar, GrpVar) %>%
        mutate(Percent = n/sum(n),
               Total = sum(n)) %>%
        ungroup()
      
      # check number of rows
      validate(need(nrow(plot_df) != 0, "No matches in the dataset. Try relaxing one or more filters."))
      
      # facet cols
      n_facets <- length(unique(plot_df$FacetVar))
      n_groups <- length(unique(plot_df$GrpVar))
      
      if(n_facets <= 2) {
        n_cols <- 2
      } else if (n_facets %in% c(3, 5, 6)) {
        n_cols <- 3
      } else if (n_facets == 4 | between(n_facets, 7, 9)){
        n_cols <- 4
      } else if (n_facets >= 10){
        n_cols <- 4
      }
      
      # a. proportion bar
      if(input$plot_type == "Proportion bar") {
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = Percent,
                     text = glue("{input$use_uptake}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          geom_text(aes(y = 1.06,
                        label = Total, fontface = "plain"),
                    size = 3) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left") +
          theme(legend.position = "none")
        
        if(n_groups >= 5 & n_facets > 2) {
          gg <- gg +
            scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                               breaks = seq(0, 1.15, 0.4), expand = c(0, 0)) +
            coord_flip(ylim = c(0, 1.5))
        } else {
          gg <- gg + 
            coord_cartesian(ylim = c(0, 1.15)) +
            scale_y_continuous(labels = scales::percent_format(accuracy = 2L),
                               breaks = seq(0, 1.15, 0.2), expand = c(0, 0)) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
        }
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Proportion of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
        
      }else if (input$plot_type == "Frequency bar") {
        
        # y -axis upper bound
        y_upper_bound <- plot_df %>%
          pull(Total) %>%
          max()
        
        # plot
        gg <- plot_df %>%
          ggplot(aes(x = GrpVar,
                     fill = use_or_uptake,
                     y = n,
                     text = glue("{use_uptake_var}: {use_or_uptake}
                                n = {n}
                                Percent: {scales::percent(Percent, accuracy = .01)}"))) +
          geom_bar(colour = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE, vjust = 0.075),
                   size = 0.3) +
          labs(x = "",
               y = "") +
          scale_y_continuous(expand = c(0, 0)) +
          scale_fill_manual(values = color) +
          theme_classic() +
          my_theme +
          facet_wrap(. ~ FacetVar, ncol = n_cols, strip.position = "left", scales = "free_y") +
          theme(legend.position = "none")
        
        
        if(n_groups >= 5 & n_facets > 2) {
          gg <- gg +
            coord_flip()
        } else {
          gg <- gg + 
            #coord_cartesian(ylim = c(0, y_upper_bound)) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
        }
        
        ggplotly(gg, tooltip = c("text")) %>%
          layout(xaxis = list(automargin = TRUE),
                 yaxis = list(automargin = TRUE, title = "Nr of respondents", titlefont = list(size = 14)),
                 margin = list(b = 90))
      }else if (input$plot_type == "Pie")  {
        
        facets <- unique(plot_df$FacetVar) %>% as.character()
        subgroups <- unique(plot_df$GrpVar) %>% as.character()
        n_facets <- length(facets)
        n_groups <- length(subgroups)
        
        validate(need(n_groups < 10, "Too many levels of grouping variable. Cannot display graph. Try selecting another variable."))
        
        code_lst <- list()
        
        for(j in seq_len(length(subgroups))){
          
          code_lst_j <- list()
          
          for(i in seq_len(length(facets))){
            
            facet <- facets[i]
            group <- subgroups[j]
            
            if(i == 1) {
              sb_title <- paste(group, "\n", facet)
            } else {
              sb_title <- paste(facet)
            }
            
            # prepare code
            code_snippet <- paste0('fig <- fig %>% add_pie(data = plot_df %>% filter(FacetVar == "', facet, '", GrpVar == "', group, '"), labels = ~use_or_uptake, values = ~n,
                               title = "', sb_title , '", domain = list(row = ', i-1, ', column = ', j-1, '),
                               textinfo = "percent",
                               hoverinfo = "text",
                               insidetextorientation = "horizontal",
                               text = ~glue("Use: {use_or_uptake}
                                              n = {n}
                                              Percent: {scales::percent(Percent, accuracy = .01)}"),
                               marker = list(colors = color))')
            
            code_lst_j[[i]] <- code_snippet
          }
          code_lst[[j]] <- paste0(unlist(code_lst_j), collapse = ";")
        }
        
        
        
        code_snippet_final <- paste0('fig <- plot_ly();',  paste0(unlist(code_lst), collapse = ";"), '; fig <- fig %>% layout(
                              showlegend = F,
                              grid = list(rows = ', n_facets, ', columns = ', n_groups, '))')
        
        fig <- eval(parse(text = code_snippet_final))
        
        fig
        
      }
    }
    
  })
  
  #---------------------------------------------------------------------------
  # 3 : Use and uptake: by perceptions
  #---------------------------------------------------------------------------
  ### Verbatim text -- for purposes of updating the reactive box width
  output$perceptions_text <- renderText({
    
    # Logical checks
    country_miss1_2 <- (input$country_sel == "Compare countries")
    partner_miss1_2 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    country_only_sel1_2 <- (!country_miss1_2 & partner_miss1_2)
    all_sel1_2 <- (!country_miss1_2 & !partner_miss1_2)
    
    # use or uptake // selected facet // selected analysis variable
    if(input$country_sel == "Compare countries") {
      selected_facet_var <- "country"
    } else {
      if(input$facet_sel1_2 == "None") {
        selected_facet_var <- "country"
      } else {
        selected_facet_var <- facet_all[facet_all == input$facet_sel1_2]
        names(selected_facet_var) <- NULL
      }
    }
    
    ds2_new <- ds2 %>%
      rename("FacetVar" = selected_facet_var)
    
    if(selected_facet_var == "country") {
      
      ds2_new <- ds2_new %>%
        mutate(country = FacetVar)
    }
    
    if(selected_facet_var == "partner") {
      
      ds2_new <- ds2_new %>%
        mutate(partner = FacetVar)
    }
    
    # Analysis dataset for different scenarios
    if(country_only_sel1_2) {
      
      temp_df <- ds2_new %>%
        filter(country == input$country_sel) 
      
    } else if (all_sel1_2) {
      
      temp_df <- ds2_new %>%
        filter(country == input$country_sel, partner == input$partner_sel) 
      
    } 
    
    # for proportion and frequency charts use numbers of facets to adjust width
    # for pie charts use number of groups
    n_facets <- ds2_new %>% filter(!is.na(FacetVar)) %>% pull(FacetVar) %>% unique() %>% length()
    
    # reactive plot height
    if(between(n_facets, 4, 8)) {
      
      values$adaptive_height = "800px"
      
    } else if(n_facets > 8) {
      
      values$adaptive_height = "1000px"
      
    } else {
      
      values$adaptive_height = "400px"
      
    }
    
    NULL
  })
  
  ### Box title ###
  output$title_perceptions <- renderText({
    
    # Logical checks
    country_miss1_2 <- (input$country_sel == "Compare countries")
    partner_miss1_2 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    default1_2 <- (country_miss1_2 & partner_miss1_2)
    country_only_sel1_2 <- (!country_miss1_2 & partner_miss1_2)
    all_sel1_2 <- (!country_miss1_2 & !partner_miss1_2)
    
    # use or uptake // selected facet // selected analysis variable
    use_uptake_var <- tolower(input$use_uptake)
    selected_category <- perception_main_categories[perception_main_categories == input$perception_category]
    
    if(input$country_sel == "Compare countries") {
      selected_facet <- "country"
      selected_facet_var <- "country"
    } else {
      if(input$facet_sel1_2 == "None") {
        selected_facet <- "country"
        selected_facet_var <- "country"
      } else {
        selected_facet <- facet_all[facet_all == input$facet_sel1_2] %>% names(.) %>% tolower()
        selected_facet_var <- facet_all[facet_all == input$facet_sel1_2]
        names(selected_facet_var) <- NULL
      }
    }
    
    if(default1_2) {
      
      gtitle <- glue("{tools::toTitleCase(use_uptake_var)} by perceptions on {paste0(selected_category, collapse = ",")} by {selected_facet}")
      
    }
    
    else if(country_only_sel1_2) {
      
      if(selected_facet_var == "country") {
        
        gtitle <- glue("{tools::toTitleCase(use_uptake_var)} by perceptions on {paste0(selected_category, collapse = ",")} in {input$country_sel}")
        
      } else{
        
        gtitle <- glue("{tools::toTitleCase(use_uptake_var)} by perceptions on {paste0(selected_category, collapse = ",")} by {selected_facet} in {input$country_sel}")
        
      }
      
    }
    
    else if(all_sel1_2) {
      
      if(selected_facet_var == "country") {
        
        gtitle <- glue("{tools::toTitleCase(use_uptake_var)} by perceptions on {paste0(selected_category, collapse = ",")} in {input$country_sel} for {input$partner_sel}")
        
      } else{
        
        gtitle <- glue("{tools::toTitleCase(use_uptake_var)} by perceptions on {paste0(selected_category, collapse = ",")} by {selected_facet} in {input$country_sel} for {input$partner_sel}")
        
      }
      
    }
    
    #gtitle
    
  })
  
  ### Plot ###
  output$perceptions_gh <- renderPlotly({
    
    # Logical checks
    country_miss1_2 <- (input$country_sel == "Compare countries")
    partner_miss1_2 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    default1_2 <- (country_miss1_2 & partner_miss1_2)
    country_only_sel1_2 <- (!country_miss1_2 & partner_miss1_2)
    all_sel1_2 <- (!country_miss1_2 & !partner_miss1_2)
    
    # use or uptake // selected facet // selected analysis variable
    use_uptake_var <- tolower(input$use_uptake)
    selected_category <- perception_main_categories[perception_main_categories == input$perception_category]
    
    if(input$country_sel == "Compare countries") {
      selected_facet <- "country"
      selected_facet_var <- "country"
    } else {
      if(input$facet_sel1_2 == "None") {
        selected_facet <- "country"
        selected_facet_var <- "country"
      } else {
        selected_facet <- facet_all[facet_all == input$facet_sel1_2] %>% names(.) %>% tolower()
        selected_facet_var <- facet_all[facet_all == input$facet_sel1_2]
        names(selected_facet_var) <- NULL
      }
    }
    
    ## default -- no selections
    if(default1_2) {
      
      # analysis dataset
      category_df <- ds2 %>%
        filter(MainCategory == input$perception_category)
      
      # plot
      gg <- makeCandlePlotsGroup(statement_nrs = c(sort(unique(category_df$statement_nr))),
                                 use_or_uptake = use_uptake_var,
                                 ds = category_df,
                                 group = selected_facet_var,
                                 filter_var = NULL,
                                 filter_value = NULL)
      
      ggplotly(gg, tooltip = c("text")) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.35,
          y = -0.2)
        )
    }
    
    ## filter by country only
    else if(country_only_sel1_2) {
      
      # analysis dataset
      category_df <- ds2 %>%
        filter(MainCategory == input$perception_category)
      
      # plot
      gg <- makeCandlePlotsGroup(statement_nrs = c(sort(unique(category_df$statement_nr))),
                                 use_or_uptake = use_uptake_var,
                                 ds = category_df,
                                 group = selected_facet_var,
                                 filter_var = "country",
                                 filter_value = input$country_sel)
      
      ggplotly(gg, tooltip = c("text")) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.35,
          y = -0.2)
        )
    }
    
    ## filter by partner and country and/or facet
    else if(all_sel1_2) {
      
      # analysis dataset
      category_df <- ds2 %>%
        filter(MainCategory == input$perception_category)
      
      # plot
      gg <- makeCandlePlotsGroup(statement_nrs = c(sort(unique(category_df$statement_nr))),
                                 use_or_uptake = use_uptake_var,
                                 ds = category_df,
                                 group = selected_facet_var,
                                 filter_var = c("country", "partner"),
                                 filter_value = c(input$country_sel, input$partner_sel))
      
      ggplotly(gg, tooltip = c("text")) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.35,
          y = -0.2))
    }
  })
  
  #---------------------------------------------------------------------------
  # 4 : Drivers of use and uptake
  #---------------------------------------------------------------------------
  ## Verbatim text -- for purposes of updating the reactive box width
  output$drivers_text <- renderText({
    
    # Logical checks
    country_miss1_1 <- (input$country_sel == "Compare countries")
    partner_miss1_1 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    default1_1 <- (country_miss1_1 & partner_miss1_1)
    country_only_sel1_1 <- (!country_miss1_1 & partner_miss1_1)
    all_sel1_1 <- (!country_miss1_1 & !partner_miss1_1)
    
    # use or uptake
    use_or_uptake_col <- tolower(use_uptake_lst[use_uptake_lst == input$use_uptake])
    
    if(country_miss1_1) {
      selected_facet_col <- "country"
    } else {
      if(input$facet_sel1_2 == "None") {
        selected_facet_col <- "country"
      } else {
        selected_facet_col <- facet_all[facet_all == input$facet_sel1_2]
        names(selected_facet_col) <- NULL
      }
    }
    
    # Analysis dataset
    analysis_dataset <- data_drivers_wide %>%
      rename("FacetVar" = selected_facet_col)
    
    if(selected_facet_col == "country") {
      analysis_dataset <- analysis_dataset %>%
        mutate(country = FacetVar)
    }
    
    if(selected_facet_col == "partner") {
      analysis_dataset <- analysis_dataset %>%
        mutate(partner = FacetVar)
    }
    
    # Box width size
    if(default1_1) {
      
      analysis_dataset1 <- analysis_dataset %>%
        filter(!is.na(FacetVar))
      
    }  else if(country_only_sel1_1) {
      
      analysis_dataset1 <- analysis_dataset %>%
        filter(country == input$country_sel) %>%
        filter(!is.na(FacetVar))
      
    } else if(all_sel1_1) {
      
      analysis_dataset1 <- analysis_dataset %>%
        filter(partner == input$partner_sel, country == input$country_sel) %>%
        filter(!is.na(FacetVar))
      
    }
    
    # Keep facets that have a count > 100
    facets_dist <- analysis_dataset1 %>%
      filter(!is.na(FacetVar)) %>%
      count(FacetVar) %>%
      filter(n >= 100)
    
    n_facets <- facets_dist %>% pull(FacetVar) %>% unique() %>% length()
    
    # reactive box width
    if (n_facets == 1) {
      
      values$adaptive_width <- 6
      
    } else if (n_facets == 2) {
      
      values$adaptive_width <- 12
      
    } else if (n_facets >= 3) {
      
      values$adaptive_width <- 12
      
    }
    
    # reactive plot height
    if (n_facets <= 2) {
      
      values$adaptive_height = "500px"
      
    } else if (between(n_facets, 3, 6)) {
      
      values$adaptive_height = "900px"
      
    } else if (n_facets > 6) {
      
      values$adaptive_height = "1400px"
      
    }
    
    NULL
  })
  
  ### Box title  ###
  output$title_ranks <- renderText({
    
    # Logical checks
    country_miss1_1 <- (input$country_sel == "Compare countries")
    partner_miss1_1 <- (input$partner_sel == "All partners")
    
    # Combination of conditions
    default1_1 <- (country_miss1_1 & partner_miss1_1)
    country_only_sel1_1 <- (!country_miss1_1 & partner_miss1_1)
    all_sel1_1 <- (!country_miss1_1 & !partner_miss1_1)
    
    # use or uptake
    use_or_uptake_col <- tolower(use_uptake_lst[use_uptake_lst == input$use_uptake])
    
    if(country_miss1_1) {
      selected_facet_col <- "country"
      selected_facet_name <- "country"
    } else {
      if(input$facet_sel1_2 == "None") {
        selected_facet_col <- "country"
        selected_facet_name <- "country"
      } else {
        selected_facet_name <- drivers_facets[drivers_facets == input$facet_sel1_3] %>% names(.) %>% tolower()
        selected_facet_col <- drivers_facets[drivers_facets == input$facet_sel1_3]
        names(selected_facet_col) <- NULL
      }
    }
    
    # box title
    if(default1_1) {
      
      gtitle = ifelse(input$facet_sel1_3 == "country",
                      glue("Rank of perceptions in order of impact on {use_or_uptake_col} by country"),
                      glue("Rank of perceptions in order of impact on {use_or_uptake_col} by {selected_facet_name}"))
      
    } else if(country_only_sel1_1) {
      
      gtitle = ifelse(input$facet_sel1_3 == "country",
                      glue("Rank of perceptions in order of impact on {use_or_uptake_col} in {input$country_sel}"),
                      glue("Rank of perceptions in order of impact on {use_or_uptake_col} by {selected_facet_name} in {input$country_sel}"))
      
    } else if(all_sel1_1) {
      
      gtitle = ifelse(input$facet_sel1_3 == "country",
                      glue("Rank of perceptions in order of impact on {use_or_uptake_col} in {input$country_sel} for partner: {input$partner_sel}"),
                      glue("Rank of perceptions in order of impact on {use_or_uptake_col} by {selected_facet_name} in {input$country_sel} for partner: {input$partner_sel}"))
    }
    
    #gtitle
    
  })
  
  ## Prepare RF output
  output$rank_percepions_gh <- renderPlotly({
    # Logical checks
    country_miss1_1 <- (input$country_sel == "Compare countries")
    
    # Combination of conditions
    default1_1 <- (country_miss1_1)
    country_only_sel1_1 <- (!country_miss1_1)
    
    # use or uptake
    use_or_uptake_col <- tolower(use_uptake_lst[use_uptake_lst == input$use_uptake])
    
    if(country_miss1_1) {
      selected_facet_col <- "country"
      selected_facet_name <- "country"
    } else {
      if(input$facet_sel1_3 == "None") {
        selected_facet_col <- "country"
        selected_facet_name <- "country"
      } else {
        selected_facet_name <- drivers_facets[drivers_facets == input$facet_sel1_3] %>% names(.) %>% tolower()
        selected_facet_col <- drivers_facets[drivers_facets == input$facet_sel1_3]
        names(selected_facet_col) <- NULL
      }
    }
    
    # Analysis dataset
    analysis_dataset <- data_drivers_wide %>%
      rename("FacetVar" = selected_facet_col)
    
    if(selected_facet_col == "country") {
      analysis_dataset <- analysis_dataset %>%
        mutate(country = FacetVar)
    }
    
    if(selected_facet_col == "partner") {
      analysis_dataset <- analysis_dataset %>%
        mutate(partner = FacetVar)
    }
    
    # default -- no selections / facets by country by default
    if(default1_1) {
      
      analysis_dataset1 <- analysis_dataset %>%
        filter(!is.na(FacetVar))
      
      facets <- unique(analysis_dataset1$FacetVar)
      
      # number of cases per facet
      dist_facets_def <- analysis_dataset1 %>%
        count(FacetVar) %>%
        mutate(FacetVar2 = glue("{FacetVar} (n = {n})"))
      
      # fit formula
      fit_formula <- as.formula(glue("{use_or_uptake_col} ~ {paste(predictors, collapse = ' + ')}"))
      
      output_df <- data.frame()
      
      # model by group
      for(facet in facets) {
        
        # subset data
        model_dataset <- analysis_dataset1 %>%
          filter(FacetVar == facet)
        
        # fit model
        fit_uptake_rf <- randomForest(fit_formula,
                                      data = model_dataset,
                                      importance = TRUE,
                                      mtry = mtry,
                                      ntree = trees)
        
        fit_uptake_rf
        
        rf_importance <- randomForest::importance(fit_uptake_rf, scale = T) %>%
          as.data.frame() %>%
          select(MeanDecreaseAccuracy)
        
        rf_importance$Statement = row.names(rf_importance)
        
        suppressMessages(rf_importance <- rf_importance %>%
                           left_join(., statements, by = c("Statement" = "category")) %>%
                           select(statement, MeanDecreaseAccuracy) %>%
                           rename("Statement" = "statement") %>%
                           mutate(MeanDecreaseAccuracy = round(MeanDecreaseAccuracy, 2)) %>%
                           mutate(Statement = as.character(Statement)) %>%
                           arrange(desc(MeanDecreaseAccuracy)))
        
        rf_importance$FacetVar <- facet
        
        rf_importance <- rf_importance %>%
          select(FacetVar, Statement, everything()) %>%
          group_by(FacetVar) %>%
          arrange(desc(MeanDecreaseAccuracy)) %>%
          mutate(`Rank Order` = row_number()) %>%
          ungroup() %>%
          select(`Rank Order`, everything())
        
        rf_importance <- inner_join(rf_importance, statements %>% select(statement, MainCategory), 
                                    by = c("Statement" = "statement"))
        
        suppressMessages(output_df <- plyr::rbind.fill(output_df, rf_importance))
      }
      
      # Prepare data for plotting
      # Drop facet levels with n < 100
      output_df <- output_df %>%
        mutate(Statement_fct = factor(Statement, levels = statement_labels, labels = statement_labels),
               MeanDecreaseAccuracy = ifelse(MeanDecreaseAccuracy < 0, 0.1, MeanDecreaseAccuracy)) %>%
        left_join(., dist_facets_def, by = "FacetVar") %>%
        filter(n >= 100) %>%
        group_by(FacetVar2, MainCategory) %>%
        summarise(MeanDecreaseAccuracy = mean(MeanDecreaseAccuracy)) %>%
        ungroup()
      
      # do the plot
      gg <- output_df %>%
        ggplot(aes(x = MainCategory, 
                   y = MeanDecreaseAccuracy, 
                   fill = MainCategory,
                   text = glue("Importance : {round(MeanDecreaseAccuracy, 0)}"))) +
        geom_bar(stat = "identity",
                 position = position_stack(reverse = TRUE, vjust = 0.075)) +
        theme_classic() +
        scale_fill_manual(values = unique(temp_statements$colors)) +
        labs(x = "", 
             y = "") +
        my_theme +
        theme(legend.position = "none",
              axis.text.y = element_text(size = 10,  angle = 0, hjust = 1)) +
        coord_flip() +
        facet_wrap( . ~ FacetVar2, ncol = 2)
      
      ggplotly(gg, tooltip = c("text")) %>%
        layout(xaxis = list(automargin = TRUE),
               yaxis = list(automargin = TRUE, title = "Importance", titlefont = list(size = 14)),
               margin = list(b = 90))
    }
    
    # ## filter by country only
    else if(country_only_sel1_1) {
      
      # analysis dataset
      analysis_dataset1 <- analysis_dataset %>%
        filter(country == input$country_sel) %>%
        filter(!is.na(FacetVar))
      
      facets <- unique(analysis_dataset1$FacetVar)
      
      dist_facets_country <- analysis_dataset1 %>%
        count(FacetVar) %>%
        mutate(FacetVar2 = glue("{FacetVar} (n = {n})"))
      
      # fit formula
      fit_formula <- as.formula(glue("{use_or_uptake_col} ~ {paste(predictors, collapse = ' + ')}"))
      
      output_df <- data.frame()
      
      # model by group
      for(facet in facets) {
        
        # subset data
        model_dataset <- analysis_dataset1 %>%
          filter(FacetVar == facet)
        
        # fit model
        fit_uptake_rf <- randomForest(fit_formula,
                                      data = model_dataset,
                                      importance = TRUE,
                                      mtry = mtry,
                                      ntree = trees)
        
        fit_uptake_rf
        
        rf_importance <- randomForest::importance(fit_uptake_rf, scale = T) %>%
          as.data.frame() %>%
          select(MeanDecreaseAccuracy)
        
        rf_importance$Statement = row.names(rf_importance)
        
        suppressMessages(rf_importance <- rf_importance %>%
                           left_join(., statements, by = c("Statement" = "category")) %>%
                           select(statement, MeanDecreaseAccuracy) %>%
                           rename("Statement" = "statement") %>%
                           mutate(MeanDecreaseAccuracy = round(MeanDecreaseAccuracy, 2)) %>%
                           mutate(Statement = as.character(Statement)) %>%
                           arrange(desc(MeanDecreaseAccuracy)))
        
        rf_importance$FacetVar <- facet
        
        rf_importance <- rf_importance %>%
          select(FacetVar, Statement, everything()) %>%
          group_by(FacetVar) %>%
          arrange(desc(MeanDecreaseAccuracy)) %>%
          mutate(`Rank Order` = row_number()) %>%
          ungroup() %>%
          select(`Rank Order`, everything())
        
        rf_importance <- inner_join(rf_importance, statements %>% select(statement, MainCategory), 
                                    by = c("Statement" = "statement"))
        
        suppressMessages(output_df <- plyr::rbind.fill(output_df, rf_importance))
      }
      
      # Prepare data for plotting
      # Drop facet levels with n < 100
      output_df <- output_df %>%
        mutate(Statement_fct = factor(Statement, levels = statement_labels, labels = statement_labels),
               MeanDecreaseAccuracy  = ifelse(MeanDecreaseAccuracy < 0, 0.1, MeanDecreaseAccuracy)) %>%
        left_join(., dist_facets_country, by = "FacetVar") %>%
        filter(n >= 100) %>%
        group_by(FacetVar2, MainCategory) %>%
        summarise(MeanDecreaseAccuracy = mean(MeanDecreaseAccuracy)) %>%
        ungroup()
      
      # Number of facet columns
      n_facets <- length(unique(output_df$FacetVar2))
      
      if(n_facets == 1) {
        n_cols <- 1
      } else if (between(n_facets, 2, 4)) {
        n_cols <- 2
      } else if (n_facets > 4) {
        n_cols <- 3
      }
      
      # do the plot
      gg <- output_df %>%
        ggplot(aes(x = MainCategory, 
                   y = MeanDecreaseAccuracy, 
                   fill = MainCategory,
                   text = glue("Importance : {round(MeanDecreaseAccuracy, 0)}"))) +
        geom_bar(stat = "identity",
                 position = position_stack(reverse = TRUE, vjust = 0.075)) +
        theme_classic() +
        scale_fill_manual(values = temp_statements$colors) +
        labs(x = "", 
             y = "") +
        my_theme +
        theme(legend.position = "none",
              axis.text.y = element_text(size = 10,  angle = 0, hjust = 1)) +
        coord_flip() +
        facet_wrap( . ~ FacetVar2, ncol = n_cols)
      
      ggplotly(gg, tooltip = c("text")) %>%
        layout(xaxis = list(automargin = TRUE),
               yaxis = list(automargin = TRUE, title = "Importance", titlefont = list(size = 14)),
               margin = list(b = 90))
    }
    
  })
  
  ## Table with descriptions of the statements
  output$drivers_table <- renderTable({
    
    statements %>%
      select(MainCategory, statement_letter, statement) %>%
      rename("Code" = "statement_letter",
             "Statement Description" = "statement") %>%
      arrange(MainCategory, Code)
    
  }, spacing = "xs")
  
  #REACH CHARTS
  #TURRY STARTED HERE
  output$event <- renderUI({
    
    selectInput("event", "Select event to view by:",
                c("Agricultural show", "Training event", "Demonstration field",
                  "Video show","Field day","Sensitization event"),
                selected = "Training event")
  })
  
  observeEvent(input$btn_go, {
    
    country <- input$country
    selection <- input$selection
    # NGmyDFW <- st_read("www/data/shape files/boundaryNG/gadm36_NGA_1.shp", stringsAsFactors = FALSE)
    # 
    # TanzmyDFW <- st_read("www/data/shape files/boundaryTZ/gadm36_TZA_1.shp", stringsAsFactors = FALSE)
    
    require(ggrepel)
    library(tmap)
    
    if (input$selection=="Events"){
      
      if (input$country=="Tanzania"){
        #BY EVENT
        event_dftz <- reactive({
          event_dftz <- event_tz2 %>%
            group_by(event, HASC_1)%>%
            dplyr::summarize(freq = dplyr::n())%>%
            filter(event==input$event)
        })
        
        
        event_map_tz <- reactive({
          
          event_map_tz <- merge(TanzmyDFW, unique(event_dftz(), by.x ="HASC_1", by.y ="region" ))
          
        })
        
        observeEvent(event_map_tz(),
                     {
                       #plot events
                       library(tmap)
                       if(event_map_tz()$event == "Training event"){
                       output$event_plt <- renderTmap({
                         #tmap_mode("view")
                         TZ_events <- tm_shape(event_map_tz()) +
                           tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "Greens", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                           tm_text(text = "NAME_1")
                         TZ_events
                         
                         })
                       }else if(event_map_tz()$event == "Video show"){
                         output$event_plt <- renderTmap({
                           #tmap_mode("view")
                           TZ_events <- tm_shape(event_map_tz()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "Blues", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           TZ_events
                           
                         })
                       }else if(event_map_tz()$event == "Agricultural show"){
                         output$event_plt <- renderTmap({
                           #tmap_mode("view")
                           TZ_events <- tm_shape(event_map_tz()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "PiYG", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           TZ_events
                           
                         })
                       }else if(event_map_tz()$event == "Demonstration field"){
                         output$event_plt <- renderTmap({
                           #tmap_mode("view")
                           TZ_events <- tm_shape(event_map_tz()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "YlOrBr", direction=1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           TZ_events
                           
                         })
                       }else if(event_map_tz()$event == "Field day"){
                         output$event_plt <- renderTmap({
                           #tmap_mode("view")
                           TZ_events <- tm_shape(event_map_tz()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "Reds", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           TZ_events
                           
                         })
                       }else if(event_map_tz()$event == "Sensitization event"){
                         output$event_plt <- renderTmap({
                           #tmap_mode("view")
                           TZ_events <- tm_shape(event_map_tz()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500,1000) , palette = "Purples", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           TZ_events
                           
                         })
                       }
                     })
        
      }else if(input$country=="Nigeria"){
                     #BY EVENT
                      event_dfng <- reactive({
                      event_dfng <- event_ng2 %>%
                      group_by(event, HASC_1)%>%
                      dplyr::summarize(freq = dplyr::n())%>%
                      filter(event==input$event)
                     })
 
                    event_map_ng <- reactive({
                    event_map_ng<- merge(NGmyDFW, unique(event_dfng(), by="HASC_1" ))
                     })
        
       #plot events})
        observeEvent(event_map_ng(),
                     {
                       if(event_map_ng()$event == "Training event"){ 
                       
                       output$event_plt = renderTmap({
                         NG_events <- tm_shape(event_map_ng()) +
                           tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "Greens", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                           tm_text(text = "NAME_1")
                         
                         NG_events
                        })
                       }else if(event_map_ng()$event == "Video show"){
                         output$event_plt = renderTmap({
                           NG_events <- tm_shape(event_map_ng()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "Blues", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           
                           NG_events
                         })
                       }else if(event_map_ng()$event == "Agricultural show"){
                         output$event_plt = renderTmap({
                           NG_events <- tm_shape(event_map_ng()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "PiYG", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           
                           NG_events
                         })
                       }else if(event_map_ng()$event == "Demonstration field"){
                         output$event_plt = renderTmap({
                           NG_events <- tm_shape(event_map_ng()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "YlOrBr", direction=1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           
                           NG_events
                         })
                       }else if(event_map_ng()$event == "Field day"){
                         output$event_plt = renderTmap({
                           NG_events <- tm_shape(event_map_ng()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500) , palette = "Reds", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           
                           NG_events
                         })
                       }else if(event_map_ng()$event == "Sensitization event"){
                         output$event_plt = renderTmap({
                           NG_events <- tm_shape(event_map_ng()) +
                             tm_polygons(col = "freq", breaks = c(0,10,20,30,60,90,100,200,500,1000) , palette = "Purples", direction=-1, title = paste("Number of", " ",  selection, sep = ""))+
                             tm_text(text = "NAME_1")
                           
                           NG_events
                         })
                       }
                     })
      }
    }
    
    else if(input$selection=="Total participants"){
      
      #TOTAL PARTICIPANTS MAP
      
      if(input$country=="Nigeria"){
        output$event_plt <- renderTmap({
          NG_participants <- tm_shape(NGReg) +
            tm_polygons(col = "total", breaks = c(0,200,500,1000, 2000, 3000, 5000, 10000, 30000, 40000, 60000),
                        palette = "Greys", direction=-1, title = selection)+
            tm_text(text = "NAME_1") 
          NG_participants
          
        })
        
      }else if (input$country=="Tanzania"){
        output$event_plt <- renderTmap({
          TZ_participants <- tm_shape(TZReg) +
            tm_polygons(col = "total", breaks = c(0,200,500,1000, 2000, 3000, 5000, 10000, 30000, 40000, 60000),
                        palette = "Greys", direction=-1, title = selection)+
            tm_text(text = "NAME_1")+
            tm_legend(outside=TRUE)
          TZ_participants
        })
        
      }
    }
  })
  
 
  output$event_plotly <- renderPlotly({
    cols <- c("grey", "#FF8080", "#FF8C66", "#D9B319", "#C0C000", "#11B932", "#9B25E7")
   # cols <- c("#f46d43", "#9dd2bf", "#ffd96a", "#1a9850", "#f46d43", "#66bd63", "#CA562C")
    names(cols) <- c("Agricultural show", "Field day", "Training event", "Media event", "Video show",
                     "Sensitization event", "Demonstration field")
   ev <- ggplot(reach_graph2, aes(event, total, fill = event,
                                  text = glue("Event: {event}
                                Count: {total}"))) +   
    geom_bar(
    stat = "identity", show.legend = FALSE)+
    geom_col() +
    scale_fill_manual(values = cols) +
    facet_grid(country ~ .)+
    labs(
       title=paste("Total number of attendees by events organized"), x='', y='') +
    
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     theme(legend.title = element_blank())+
     theme(legend.position = "none")+
    coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

    ggplotly(ev, tooltip = c("text"))
 
  })
  
  output$event_plotly_NG <- renderPlotly({
    cols <- c("#33ABB9", "#6850A0", "#609E25", "#FC8D62", "#CF6E64", "#FFC30F", "grey")
    #cols <- c("#f46d43", "#9dd2bf", "#ffd96a", "#1a9850", "#f46d43", "#66bd63", "#d73027")
    names(cols) <- c("Agricultural show", "Field day", "Training event", "Media event", "Video show",
                     "Sensitization event", "Demonstration field")
    ev_NG <- ggplot(reach_NG, aes(event, total, fill = event,
                                   text = glue("Event: {event}
                                Count: {total}"))) +   
      geom_bar(
        stat = "identity", show.legend = FALSE)+
      geom_col() +
      scale_fill_manual(values = cols)+
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by events organized"), x='', y='')+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.title = element_blank())+
      theme(legend.position = "none")+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

    ggplotly(ev_NG, tooltip = c("text"), textposition = "left")
    
  })
  
  output$event_plotly_TZ <- renderPlotly({
    
    cols <- c("#33ABB9", "#6850A0", "#609E25", "#FC8D62", "#CF6E64", "#FFC30F", "grey")
    names(cols) <- c("Agricultural show", "Field day", "Training event", "Media event", "Video show",
                     "Sensitization event", "Demonstration field")
    ev_TZ <- ggplot(reach_TZ, aes(event, total, fill = event,
                                   text = glue("Event: {event}
                                Count: {total}"))) +   
      geom_bar(stat="identity")+
      geom_col() +
      scale_fill_manual(values = cols)+
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by events organized"), x='', y='') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.title = element_blank())+
      theme(legend.position = "none")+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ggplotly(ev_TZ, tooltip = c("text"))
    
  })
  
  output$use_plotly <- renderPlotly({
    cols <- c("darkgreen", "navy", "lightgreen", "red")
    names(cols) <- c("WM/PP", "SP","IC",  "FR")
                   
  use <- ggplot(attend_use3, aes(usecase, total, fill = usecase,
                        text = glue("
                                     Use case: {usecase}
                                    Total: {total}"))) +   
    geom_bar(stat="identity")+
    geom_col() +
    scale_fill_manual(values = cols) +
    facet_grid(country ~ .)+
    labs(
      title=paste("Total number of attendees by use case"),  x='', y='') +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
   ggplotly(use, tooltip = c("text"))
  
  })
  
  output$use_plotly_NG <- renderPlotly({
    cols <- c("darkgreen", "navy", "lightgreen", "red")
    names(cols) <- c("WM/PP", "SP","IC",  "FR")
    use_NG <-  ggplot(attend_use_ng, aes(usecase, total, fill = usecase,
                                                text = glue("
                                     Use case: {usecase}
                                    Total: {total}")))+   
      geom_bar(
        stat = "identity")+
      geom_col() +
      scale_fill_manual(values = cols)+
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by use case"),   x='', y='')+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ggplotly(use_NG, tooltip = c("text"))
    
  })
  
  output$use_plotly_TZ <- renderPlotly({
    cols <- c("darkgreen", "navy", "lightgreen", "red")
    names(cols) <- c("WM/PP", "SP","IC",  "FR")
    use_TZ <-  ggplot(attend_use_tz, aes(usecase, total, fill = usecase,
                                                text = glue("
                                     Use case: {usecase}
                                    Total: {total}"))) +   
      geom_bar(
        stat = "identity")+
      geom_col() +
      scale_fill_manual(values = cols) +
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by use case"),  x='', y='') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
     ggplotly(use_TZ, tooltip = c("text"))
    
  })
  
  
  
  #USECASE ~ ATTENDEES
  output$part_usecase <- renderPlotly({
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
  pt_use <- ggplot(usept_ds, aes(x=id,
                                 text = glue("Participant: {Participant}
                                    
                                    ")
                                 )) +
    geom_bar(aes(fill=Participant), position="fill") +
    facet_grid(country ~ .)+
      coord_flip()+
    #scale_fill_brewer(palette='Dark2', direction=1)+
    scale_fill_manual(values = cols) +
    scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
      labs(
      title=paste("Participants by use case"),
      x='', y=''
    ) +
    theme_classic() +
    theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ggplotly(pt_use, tooltip = c("text"))
  })
  
  #USECASE ~ ATTENDEES TZ
  output$part_usecase_TZ <- renderPlotly({
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
    pt_use_TZ <- ggplot(usept_ds_TZ, aes(x=id)) +
      geom_bar(aes(fill=Participant), position="fill") +
       geom_text(
        data=usept_ds2_TZ,
        aes(y=freq, label= "", group=Participant,
            text = glue("Participant: {Participant}
                         Percent: {per2}
                         Count: {freq}")
        ),
        position=position_fill(vjust=0.5),
        color='gray25', size=3.5
      ) +
      coord_flip()+
      #scale_fill_brewer(palette='Dark2', direction=1)+
      scale_fill_manual(values = cols) +
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
       labs(
        title=paste("Participants by use case (Tanzania)"),
        x='', y=''
      ) +
      theme_classic() +
      theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    #ggplotly(pt_use_TZ)
    ggplotly(pt_use_TZ, tooltip = c("text")) 
    
  })
  
  #USECASE ~ ATTENDEES NG
  output$part_usecase_NG <- renderPlotly({
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
    cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#ffd96a")
    pt_use_NG <- ggplot(usept_ds_NG, aes(x=id)) +
      geom_bar(aes(fill=Participant), position="fill") +
        geom_text(
        data=usept_ds2_TZ,
        aes(y=freq, label= "", group=Participant,
            text = glue("Participant: {Participant}
                         Percent: {per2}
                         Count: {freq}")
        ),
        position=position_fill(vjust=0.5),
        color='gray25', size=3.5
      ) +
      coord_flip()+
      scale_fill_manual(values = cols) +
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
        labs(
        title=paste("Participants by use case (Nigeria)"),
        x='', y=''
      ) +
      theme_classic() +
      theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    ggplotly(pt_use_NG, tooltip = c("text")) 
    
  })
  
  #EVENT ~ PARTICIPANTS
  output$event_pt_fct <- renderPlotly({
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
    
    pt_event_fct <- ggplot(eventpt_ds2, aes(x=Event,
                                            text = glue("Participant: {Participant}")))+
      geom_bar(aes(fill=Participant), position="fill") +
      facet_grid(country ~ .)+
      coord_flip()+
      scale_fill_manual(values = cols) +
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
      labs(
        title=paste("Participants by events"),
        x='', y=''
      ) +
      theme_classic() +
      theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

    ggplotly(pt_event_fct, tooltip = c("text"))
   
  })
  
  #EVENT ~ ATTENDEES TZ
  output$event_pt_tz <- renderPlotly({
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
    pt_event_tz <- ggplot(eventpt_ds_TZ, aes(x=Event)) +
      geom_bar(aes(fill=Participant), position="fill") +
       geom_text(
        data=eventpt_ds2_TZ,
        aes(y=freq, label= "", group=Participant,
            text = glue("Participant: {Participant}
                         Percent: {per2}
                         Count: {freq}")
        ),
        position = position_fill(vjust = 0.5),
        color='gray25', size=3.5
      ) +
      coord_flip()+
      scale_fill_manual(values = cols) +
      
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
       labs(
        title=paste("Participants by events (Tanzania)"),
        x='', y=''
      ) +
      theme_classic() +
      theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ggplotly(pt_event_tz, tooltip = c("text")) 
    
  })
  
  #EVENT ~ ATTENDEES NG
  output$event_pt_ng <- renderPlotly({
    
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
    pt_event_ng <- ggplot(eventpt_ds_NG, aes(x=Event))+
       geom_text(
        data=eventpt_ds2_NG,
        aes(y=freq, label= "", group=Participant,
            text = glue("Participant: {Participant}
                         Percent: {per2}
                         Count: {freq}")
            ),
        position = position_fill(vjust = 0.5),
        color='gray25', size=3.5 )+
      geom_bar(aes(fill=Participant), position="fill")+
      coord_flip()+
      scale_fill_manual(values = cols) +
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format())+
        labs(
        title=paste("Participants by events (Nigeria)"), x='', y='')+
      theme_classic()+
      theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    ggplotly(pt_event_ng, tooltip = c("text")) 
   })
  
  #PARTNER ~ Participants NG
  output$prtnr_pt_ng <- renderPlotly({
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
    pt_prtnr_ng <- ggplot(reach_prt_ng, aes(x=Partner))+
      geom_bar(aes(fill=Participant), position="fill")+
      geom_text(
        data=reach_prt2_ng,
        aes(y=freq, label=" ", group=Participant,
            text = glue("Participant: {Participant}
                         Count: {freq}")),
        position = position_fill(vjust = 0.5),
        color='gray25', size=3.5)+
      coord_flip()+
      
     # scale_fill_brewer(palette='Dark2', direction=1)+
      scale_fill_manual(values = cols) +
      
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format())+
      labs(
        title=paste("Participants by partners (Nigeria)"),
        x='', y='')+
      
      theme_classic()+
      theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    
    ggplotly(pt_prtnr_ng, tooltip = c("text"))
    
    
  })
  
  #PARTNER ~ Participants TZ
  output$prtnr_pt_tz <- renderPlotly({
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
    pt_prtnr_tz <- ggplot(reach_prt_tz, aes(x=Partner)) +
      geom_bar(aes(fill=Participant), position="fill") +
      geom_text(

        data=reach_prt2_tz,
        aes(y=freq, label=" ", group=Participant,
            text = glue("Participant: {Participant}
                         Count: {freq}")),
        position = position_fill(vjust = 0.5),

        color='gray25', size=3.5
      ) +
      coord_flip()+
      
      #scale_fill_brewer(palette='Dark2', direction=1)+
      scale_fill_manual(values = cols) +
      
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
      labs(
        title=paste("Participants by partners (Tanzania)"),
        x='', y=''
      ) +
      theme_classic() +
      theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    
    ggplotly(pt_prtnr_tz, tooltip = c("text"))
    
    
  })
  
  #PARTNERS ~ Participants
  output$prtnr_pt_fct <- renderPlotly({
    cols <- c( "#CED6B0" ,"#F0CE9E", "#E19363", "#CA562C", "#7DAB98", "#008080", "#B1B3B3", "#989B9C" ,"#73737F")
    names(cols) <- c("Farmers", "Researchers","Government EAs", "Government Organization Staff", "NGO EAs", "NGO staff",
                     "Private EAs", "Private Organization", "Others" )
    pt_prtnr_fct <- ggplot(reach_prt3, aes(x=Partner,
                                           text = glue("Participant: {Participant}")))+
                                                        
      geom_bar(aes(fill=Participant), position="fill")+
               
      
      facet_grid(country ~ .)+
      
      coord_flip()+
      
      #scale_fill_brewer(palette='Set3', direction=1)+
      scale_fill_manual(values = cols) +
      
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
      labs(
        title=paste("Participants by partners"),
        x='', y=''
      ) +
      theme_classic() +
      theme(legend.position='top')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    
    ggplotly(pt_prtnr_fct)
    
    
  })
  
  #GENDER
  # faceted by country
  
  output$gen_facet <- renderPlotly({
    cols <- c("#FFB1CB", "#01A6EA")
    names(cols) <- c("Female", "Male")
    g <- ggplot(gender_ds2, 
                aes(x = event, 
                    y = total,
                    fill = Gender,
                    text = glue("Gender: {Gender}
                                 Count: {total}"))) + 
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
      labs(
        title=paste("Attendees by gender and event"),
        x='', y=''
      ) +
      scale_x_discrete(limits = events)+
      coord_flip()+
      facet_grid(country~ .)+
      scale_fill_manual(values = cols)+
  
      #scale_fill_brewer(palette='Dark2', direction=-1)+
      geom_bar(stat="identity", position="fill")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    ggplotly(g)
    ggplotly(g, tooltip = c("text")) 
    
  })
  
  # bY COUNTRY NG
  output$gen_NG <- renderPlotly({
    cols <- c("#FFB1CB", "#01A6EA")
    names(cols) <- c("Female", "Male")
  g_NG <- ggplot(gender_ds_NG, 
                 aes(x = event, 
                     y = total,
                     fill = Gender,
                     text = glue("Gender: {Gender}
                                  Count: {total}"))) + 
    scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
    labs(
      title=paste("Attendees by gender and event (Nigeria)"),
      x='', y=''
    ) +
    coord_flip()+
    scale_x_discrete(limits = events_NG)+
    scale_fill_manual(values = cols)+
    geom_bar(stat="identity", position="fill")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  #ggplotly(g_NG)
  ggplotly(g_NG, tooltip = c("text")) 
  })
  
  # bY COUNTRY Tanzania
  output$gen_TZ <- renderPlotly({
    cols <- c("#FFB1CB", "#01A6EA")
    names(cols) <- c("Female", "Male")
    g_TZ <- ggplot(gender_ds_TZ, 
                   aes(x = event, 
                       y = total,
                       fill = Gender,
                       text = glue("Gender: {Gender}
                                    Count: {total}")
                                
                       )) + 
  
      scale_y_continuous(expand=expansion(0), labels=scales::percent_format()) +
      labs(
        title=paste("Attendees by gender and event (Tanzania)"),
        x='', y=''
      ) +
      coord_flip()+
      scale_x_discrete(limits = events_TZ)+
      scale_fill_manual(values = cols)+
      geom_bar(stat="identity", position="fill")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    #ggplotly(g_TZ)
    
    ggplotly(g_TZ, tooltip = c("text")) 
  })
  
  ###############################################
  #total attendees
  ##############################################
  
  #GENDER
  # faceted by country
  
  output$gen_att_facet <- renderPlotly({
    cols <- c("#FFB1CB", "#01A6EA")
    names(cols) <- c("Female", "Male")
    g <- ggplot(gender_ds2, aes(event, total, fill=Gender,
                                text = glue("Event: {event}
                                             Total: {total}"))) +   
      geom_bar(
        stat = "identity", show.legend = FALSE)+
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by partners"), x='', y='') +
      scale_fill_manual(values = cols)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.title = element_blank())+
      theme(legend.position = "none")+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    
    ggplotly(g, tooltip = c("text")) 
    
  })
  
  # bY COUNTRY NG
  output$gen_att_NG <- renderPlotly({
    cols <- c("#FFB1CB", "#01A6EA")
    names(cols) <- c("Female", "Male")
    g_NG <- ggplot(gender_ds_NG, 
                   aes(event, total, fill=Gender,
                       text = glue("Event: {event}
                                    Total: {total}"))) +   
      geom_bar(
        stat = "identity", show.legend = FALSE)+
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by events"), x='', y='') +
      scale_fill_manual(values = cols)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.title = element_blank())+
      theme(legend.position = "none")+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    #ggplotly(g_NG)
    ggplotly(g_NG, tooltip = c("text")) 
  })
  
  # bY COUNTRY TZ
  output$gen_att_TZ <- renderPlotly({
    cols <- c("#FFB1CB", "#01A6EA")
    names(cols) <- c("Female", "Male")
    g_TZ <- ggplot(gender_ds_TZ, 
                   aes(event, total, fill=Gender,
                       text = glue("Event: {event}
                                    Total: {total}"))) +   
      geom_bar(
        stat = "identity", show.legend = FALSE)+
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by events"), x='', y='') +
      scale_fill_manual(values = cols)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.title = element_blank())+
      theme(legend.position = "none")+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    
    ggplotly(g_TZ, tooltip = c("text")) 
  })
  
  # #gender bar graph
  # output$partnr_plotly <- renderPlotly({
  #   
  #   prtnr <- ggplot(gender_ds2, aes(event, total, fill=gender,
  #                                         text = glue("Event: {event}
  #                               Total: {total}"))) +   
  #     geom_bar(
  #       stat = "identity", show.legend = FALSE)+
  #     facet_grid(country ~ .)+
  #     labs(
  #       title=paste("Total number of attendees by events"), x='', y='') +
  #     
  #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #     theme(legend.title = element_blank())+
  #     theme(legend.position = "none")+
  #     coord_flip()
  #   
  #   ggplotly(prtnr, tooltip = c("text"))
  #   
  # })
  #BY PARTNERS
  output$partnr_plotly <- renderPlotly({
    
    prtnr <- ggplot(parti_attendees2, aes(partner, total, 
                                          text = glue("Partner: {partner}
                                                       Total: {total}"))) +   
      
       geom_bar(
        stat = "identity", fill="#B56D4E", show.legend = FALSE)+
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by partners"), x='', y='') +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.title = element_blank())+
      theme(legend.position = "none")+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    
    ggplotly(prtnr, tooltip = c("text"))
    
  })
  
  output$partnr_plotly_NG <- renderPlotly({
    
    prtnr_NG <- ggplot(parti_attendees_NG, aes(partner, total, 
                                               text = glue("Partner: {partner}
                                                            Total: {total}"))) +   
      geom_bar(
        stat = "identity",fill="#B56D4E", show.legend = FALSE)+
      
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by partners"), x='', y='') +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.title = element_blank())+
      theme(legend.position = "none")+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    ggplotly(prtnr_NG, tooltip = c("text"))
    
  })
  
  output$partnr_plotly_TZ <- renderPlotly({
    
    prtnr_TZ <- ggplot(parti_attendees_TZ, aes(partner, total, 
                                               text = glue("Partner: {partner}
                                                            Total: {total}"))) +   
      geom_bar(
        stat = "identity", fill="#B56D4E", show.legend = FALSE)+
      facet_grid(country ~ .)+
      labs(
        title=paste("Total number of attendees by partners"), x='', y='') +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(legend.title = element_blank())+
      theme(legend.position = "none")+
      coord_flip()+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    
    ggplotly(prtnr_TZ, tooltip = c("text"))
    
  })
  
  
}

#enable both the following lines  WHEN POSTING TO THE SERVER. Comment out afterwards to be able to test the script locally
 # app <- shinyApp(ui,server)
 # runApp(app, host="0.0.0.0", port=4242)
shinyApp(ui = ui, server = server)