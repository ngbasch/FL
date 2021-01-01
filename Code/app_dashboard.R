###############################################################################
###   AUTHOR: Nathan Basch
###   DATE: 5/6/2002
###
###   DESCRIPTION: LOAD AND CLEAN USDA QUICKSTATS DATA; LOAD TIGRIS SHAPEFILE
###############################################################################

### Set Environment -------------------------

# Clear the environment 
rm(list=ls())

### Set Environment -------------------------
options(stringsAsFactors = F)
options(shiny.usecairo=TRUE)
#setwd("/users/nathanbasch/Documents/FL")

#Load libraries
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(googledrive)
library(rlang)
library(leaflet.extras)
library(shinyWidgets)
library(googledrive)
library(googlesheets4)
library(shinydashboard)
library(rintrojs)
library(shinyBS)
library(shinyjs)
library(tidyverse)
#library(DBI)

#INPUTS -------------
SIDEBAR_WIDTH = 300

df_names<-c("combined", "donors", "recipients")
u<-paste0("https://github.com/ngbasch/FL/raw/master/Intermediate/",df_names,".Rda")

#for (i in (1:length(df_names))){
#  load(url(u[i]))
#}

# combined_in<-read_csv("Intermediate/combined.csv")
# donors<-read_csv("Intermediate/donors.csv")
# recipients<-read_csv("Intermediate/recipients.csv")

colors<-c("#c95e4a","#5aad6a","#c55a9f", "#c9a944","#777acd","#7e7c34")
town_colors<-c("#4570b0", "#62bf4b","#ad5bd0","#a8b635", "#697ceb", "#d49f37","#6252b6", 
               "#51903a","#d143a0", "#5ec288","#df3f79","#42bec7","#de4452","#388661","#c2432b",
               "#7aa3e3","#e37632","#876db3","#adb064","#a24e99","#667228","#d48ad8","#8e6f2e", "#dd80a8",
               "#a25b2c", "#994974","#dc996b", "#af4056","#e17e7d", "#995258")


#Function to clean units --------------------------------------
addUnits <- function(n, dec = 0) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, dec), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, dec), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, dec), 'B'), # in billions
                                        paste0(round(n/1e12, dec),'T')
                                 ))))
  return(labels)
}

Tabulate<-function(df, level){
  df%>%
    mutate(var = case_when(
      level == "Daily" ~ date,
      level == "Weekly" ~ floor_date(date, "week"),
      level == "Monthly" ~ as.Date(paste(year(date), month(date), "01",sep = "-")),
      level == "Quarterly" ~ as.Date(paste(year(date), (ceiling(month(date)/3)-1)*3+1, "01", sep = "-")),
      level == "Annually" ~ as.Date(paste(year(date), "01", "01", sep = "-"))
    )
    )%>%
    group_by(var, type, direction)%>%
    summarise(lb = sum(lb))
}


#Set units and crop colors --------------------------------------
#units<-county_all%>%group_by(statisticcat_desc, unit_desc, commodity_desc)%>%summarise(t = max(value_num))
#classes<-county_all%>%distinct(commodity_desc, class_desc)

#' Main UI function for the application
#'
app_ui <- dashboardPage(
  dashboardHeader(title = "Foodlink Dashboard", titleWidth = SIDEBAR_WIDTH),
  dashboardSidebar(width = SIDEBAR_WIDTH,
                   
                   
                   # Grower name selection
                   #Control Panel:
                   # pickerInput("goods", "Goods", sort(unique(combined_in$type)),
                   #             multiple = TRUE,
                   #             selected = sort(unique(combined_in$type)),
                   #             options = list(`live-search` = TRUE, `actions-box` = TRUE)),
                   #hr(),
                   #hr(),
                   selectInput("trend", "Trend", c("Daily","Weekly", "Monthly", "Quarterly", "Annually"), selected = "Quarterly")
                   
                
  ),
  
  dashboardBody(
    useShinyjs(),
    introjsUI(),
    
    #includeCSS("inst/www/custom_styles.css"),
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton("grower_search_button",
                   label = "Summary",
                   icon = icon("fas fa-th-list pr-2"),
                   style = "success"),
          bsButton("us_map_button",
                   label = "Map",
                   icon = icon("map"),
                   style = "success"),
          bsButton("food_info_button",
                   label = "Food Trends",
                   icon = icon("fas fa-birthday-cake"),
                   style = "success"),
          bsButton("volunteer_info_button",
                   label = "Volunteers",
                   icon = icon("fas fa-users"),
                   style = "success")
          
        ))
    ),
    
    
    br(),
    br(),
    
    fluidRow(
      div(
        id = "summary_panel",
        column(width = 4, 
               box(
                 title = "This Fiscal Year",
                 width = NULL,
                 solidHeader = FALSE,
                 DT::dataTableOutput("summary_results")
               )),
        column(width = 8, 
               tabBox(
                 title = "Previous Fiscal Years",
                 width = NULL,
                 tabPanel("Donor vs Recipient", plotOutput("line_donor_recipient")),
                 tabPanel("Stacked Bar", plotOutput("total_stacked")),
                 tabPanel("Line", plotOutput("total_line"))
               )),
        column(width = 4, 
               box(
                 title = "Donor Org Table",
                 width = NULL,
                 solidHeader = FALSE,
                 DT::dataTableOutput("donor_org_table")
               )),
        column(width = 8,
               tabBox(
                 title = "Donor Org Bar",
                 width = NULL,
                 tabPanel("Stacked Bar (Proportion)", plotOutput("donor_org_stacked_perc")),
                 tabPanel("Stacked Bar", plotOutput("donor_org_stacked"))
               )),
        column(width = 4, 
               box(
                 title = "Lbs Delivered by City",
                 width = NULL,
                 solidHeader = FALSE,
                 DT::dataTableOutput("city_table")
               )),
        column(width = 8,
               tabBox(
                 title = "",
                 width = NULL,
                 tabPanel("Stacked Bar (Percent)", plotOutput("city_stacked_perc")),
                 tabPanel("Stacked Bar", plotOutput("city_stacked"))
                )),
        column(width = 12, 
               box(
                 title = "Top Recipients",
                 width = NULL,
                 solidHeader = FALSE,
                 DT::dataTableOutput("top_recipients_table")
               )),
        column(width = 12, 
               box(
                 title = "Top Donors",
                 width = NULL,
                 solidHeader = FALSE,
                 DT::dataTableOutput("top_donors_table")
               ))
        
        
      )),
    #MAP PANEL ------------------------------------------
    br(),
    br(),
        fluidRow(
          div(
            id = "us_map_panel",
            column(width = 12, 
              div(
                 tabBox(
                   height = 480,
                   width = NULL,
                   tabPanel(
                     title = "Map",
                     div(
                       style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                         selectizeInput("direction", "Facilities:", multiple = T, c("donor","recipient"), selected = c("donor", "recipient")),
                         selectizeInput("months_to_show", label = "Map Facilities Active in Previous", 
                                        choices = c("1 Month", "3 Months", "6 Months", "1 Year", "2 Years", "History"), selected = "1 Year", multiple = F),
                         awesomeCheckbox(inputId = "addmarkers", label = "Show Facility Markers", value = TRUE),
                         awesomeCheckbox(inputId = "addcircles", label = "Show Density Circles"),
                         awesomeCheckbox(inputId = "addheatmap", label = "Show Density Heatmap"),
                         size = "xs",
                         icon = icon("gear", class = "opt"), 
                         up = TRUE
                         )
                   ),
                   leafletOutput("map")
               ))
              )),
        column(width = 6,
               tabBox(
                 title = "Map Graph",
                 width = NULL,
                 tabPanel("Stacked Bar", plotOutput("time_series")),
                 tabPanel("Line", plotOutput("line"))
                 # tabPanel("Scatter", plotOutput("scatter")),
                 # tabPanel("Line", plotlyOutput("line"))
               )
        ),
        column(width = 6,
               box(
                 title = "Detailed Table",
                 width = 12,
                 status = "primary",
                 align = "right",
                 solidHeader = FALSE,
                 DT::dataTableOutput("results"),
                 downloadButton("download_results", "Download"),
               ))


      )),
    fluidRow(
      div(
        id = "food_info_panel",
        column(width = 12,
               tabBox(
                 title = "Map Graph",
                 width = NULL,
                 tabPanel("", plotOutput("food_bar"))
               )
      ))
  ))
)


#SERVER ---------------------------------------------------------------------------------------------------------------------------------------------------------------

DEFAULT_TABLE_OPTIONS <- list(scrollX="800px", scrollY="450px", columnDefs = list(list(className='dt-center', targets = "_all")))

#' Shiny app server function
#'
#' @param input - Shiny input object
#' @param output - Shiny output object
#'
#'
#'
app_server <- function(input, output, session){
  # DYNAMIC RENDER RULES ----------------------------------------------------
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Summary", "US Map", "Food Info", "Volunteers"),
                      label = "",
                      selected = x
    )
  }
  
  #observe(print(reactiveValuesToList(input)) )
  

  observeEvent(input$grower_search_button, {
    update_all("Summary")
  })
  observeEvent(input$us_map_button, {
    update_all("US Map")
  })
  observeEvent(input$food_info_button, {
    update_all("Food Info")
  })
  observeEvent(input$volunteer_info_button, {
    update_all("Volunteers")
  })
  
  # hide the underlying selectInput in sidebar for better design
  observeEvent("", {
    hide("tab")
  })
  
  
  
  observeEvent("", {
    show("summary_panel")
    hide("us_map_panel")
    hide("food_info_panel")
    hide("volunteer_panel")
  }, once = TRUE)
  
  observeEvent(input$grower_search_button, {
    show("summary_panel")
    hide("us_map_panel")
    hide("food_info_panel")
    hide("volunteer_panel")
  })
  observeEvent(input$us_map_button, {
    hide("summary_panel")
    show("us_map_panel")
    hide("food_info_panel")
    hide("volunteer_panel")
  })
  observeEvent(input$food_info_button, {
    hide("summary_panel")
    hide("us_map_panel")
    show("food_info_panel")
    hide("volunteer_panel")
  })
  observeEvent(input$volunteer_info_button, {
    hide("summary_panel")
    hide("us_map_panel")
    hide("food_info_panel")
    show("volunteer_panel")
  })
  
  
  observe({print(input$tabID)})
  
  
  #Define Reactives --------------------------------------------
  
  
  combined_reactive<- reactive({
  months_to_include<-case_when(input$months_to_show == "1 Month" ~ 1, 
                               input$months_to_show == "3 Months" ~ 3,
                               input$months_to_show == "6 Months" ~ 6,
                               input$months_to_show == "1 Year" ~ 12,
                               input$months_to_show == "2 Years" ~ 24,
                               input$months_to_show == "History" ~ 1e5
                               )
    
    
    combined_in%>%
      filter(date >= (Sys.Date() %m-% months(months_to_include)))
    
  })
  

  #Reactive expression for the data subsetted to what the user selected
  locations_reactive<-reactive({
    req(input$us_map_button)
    
    #Calculate weekly Lbs
    weekly_Lbs<-
      combined_in %>%
      mutate(week = floor_date(date, "week"))%>%
      group_by(week, type, name, direction)%>%
      summarise(week_lb = sum(lb, na.rm=T) )%>%
      group_by(type, name, direction)%>%
      summarise(weekly_lb = mean(week_lb))%>%
      arrange(type)%>%
      group_by(name, direction)%>%
      summarise(weekly_label = paste(paste0(type,": ", round(weekly_lb,1), "lb / wk"), collapse = "<br>"),
                total_weekly_lbs = sum(weekly_lb))
    
    
    
    locations<-donors%>%select("agency" = donor, lon, lat)%>%mutate(label = "donor", color = "blue")%>%
      bind_rows(
        recipients%>%
          select("agency" = recipient_agency, lon, lat)%>%
          mutate(label = "recipient", color = "red")
      )%>%
      filter(!is.na(lon))%>%
      mutate(agency = tolower(agency))%>%
      #NOTE - doing an inner join here will remove any recipients/donors who don't exist in the transactional data
      inner_join (weekly_Lbs%>%select(-direction), by = c("agency" = "name"))%>%
      mutate(weekly_label= ifelse(is.na(weekly_label), "", weekly_label))
    
    
    locations%>%
      filter(label %in% input$direction)
  })
  
  
  
  # This reactive expression represents the palette function,
  # mypal <- reactive({
  #    range = df_reactive()[["value_num"]]
  #    colorNumeric(palette = c("#fffdd0", "#FF8C00", "#006400"), domain = range, na.color = "grey")
  #  })
  
  
  
  # Create the map
  

  output$map <- renderLeaflet({
  req(input$us_map_button)
#  req(locations_reactive)
    temp<-locations_reactive()
    
    leafIcons <- icons(
      iconUrl = ifelse(temp$label == "recipient",
                       "https://image.flaticon.com/icons/svg/3208/3208275.svg",
                       "https://image.flaticon.com/icons/svg/1361/1361511.svg"
      ),
      iconWidth = 30, iconHeight = 30
      #iconAnchorX = 22, iconAnchorY = 94
    )
    
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      setView(lng = -71.1, lat = 42.4, zoom = 11)%>%
      addMarkers(data = temp,~lon, ~lat, layerId =~agency, icon = leafIcons,
                 popup = ~paste0('<strong>',as.character(label), " - ",as.character(agency), '</strong>',
                                 "<br><br>", weekly_label))
    
    
    
  })
  

  # Add points to the map
  observe({
    req(locations_reactive)

    temp<-locations_reactive()
    
    leafIcons <- icons(
      iconUrl = ifelse(temp$label == "recipient",
                       "https://image.flaticon.com/icons/svg/3208/3208275.svg",
                       "https://image.flaticon.com/icons/svg/1361/1361511.svg"
      ),
      iconWidth = 30, iconHeight = 30
      #iconAnchorX = 22, iconAnchorY = 94
    )
    
    leafletProxy("map")%>%clearMarkers()
    
    #print(input$addmarkers)
    #print(input$tabID)
    #print(input$)
    #print(input$us_map_panel)
    

    if (input$addmarkers == FALSE)
      return()
    
    isolate({
      leafletProxy("map", data = temp) %>%
        addMarkers(data = temp,~lon, ~lat, layerId =~agency, icon = leafIcons,
                   popup = ~paste0('<strong>',as.character(label), " - ",as.character(agency), '</strong>',
                                   "<br><br>", weekly_label))
    })
    
  })
  

  #Add heat density points
  observe({
    req(locations_reactive)

    temp<-locations_reactive()
    
    
    leafletProxy("map") %>% clearShapes()
    
    if (input$addcircles == FALSE)
      return()
    
    radius = temp[["total_weekly_lbs"]] / median(temp[["total_weekly_lbs"]],na.rm=T) * 400
    
    
    isolate({
      leafletProxy("map", data = temp)%>%
        addCircles(~lon, ~lat, color =  ~ color, opacity = 0.5, stroke = FALSE,radius = radius, fillOpacity = 0.2)
      
    })
  })
  
  #Add heatmap density
  observe({
    req(locations_reactive)

    temp<-locations_reactive()
    
    
    leafletProxy("map") %>% clearHeatmap()
    
    if (input$addheatmap == FALSE)
      return()
    
    isolate({
      leafletProxy("map", data = temp)%>%
        addHeatmap(lng = ~lon, lat = ~lat, intensity = ~total_weekly_lbs, max = 0.5, blur = 55, radius = 30)      
    })
  })
  
  results_selection <- reactive({
    req(input$map_marker_click)

    
    combined_in%>%
      filter(name == input$map_marker_click$id)
    
    
  })

  output$results <-   DT::renderDataTable({

    results_selection()%>%
      arrange(desc(date))%>%
      DT::datatable(rownames = FALSE, selection = "single")
    
  })
  
  observeEvent(input$map_marker_click, {
    
    output$time_series <- renderPlot({
      req(nrow(results_selection())>0)
      click <- input$map_marker_click
      
      gg_df<-
        results_selection()
      
      gg<-gg_df%>%
        Tabulate(input$trend)%>%
        #  mutate(direction =fct_reorder(direction, -lb))%>%
        ggplot(aes(x = var, y = lb, fill = type))+
        scale_fill_manual(values = colors)+
        geom_col()+
        labs(x = "", y = paste0(input$trend," Lbs"), title = click$id, fill = "")+
        facet_wrap(~direction)+
        theme_bw()
      
      gg
    })
    
    output$line <- renderPlot({
      req(nrow(results_selection())>0)
      click <- input$map_marker_click
      
      gg_df<-
        results_selection()
      
      gg<-gg_df%>%
        Tabulate(input$trend)%>%
        #  mutate(direction =fct_reorder(direction, -lb))%>%
        ggplot(aes(x = var, y = lb, colour = type))+
        scale_colour_manual(values = colors)+
        geom_line()+
        labs(x = "", y = paste0(input$trend," Lbs"), title = click$id, colour = "")+
        geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
        facet_wrap(~direction)+
        theme_bw()
      
      gg
    })
    
    
  })
  
  #SUMMARY SECTION OF APP -----------------------------------------------------
  output$summary_results <-   DT::renderDataTable({
    df<-combined_in%>%
      mutate(var = as.Date(paste(year(date), (ceiling(month(date)/3)-1)*3+1, "01", sep = "-")))%>%
      group_by(var, direction)%>%
      summarise(lbs = round(sum(lb,na.rm=T)),
                orgs = n_distinct(name))%>%
      mutate(FY = ifelse(month(var) == 1, year(var), year(var)+1))%>%
      group_by(direction)%>%
      filter(FY == max(FY))%>%
      gather(variable, val, lbs:orgs)%>%
      mutate(var = paste0(month.abb[month(var)], "-", month.abb[month(var)+2]),
             variable = paste0(direction, " ", variable))%>%
      group_by(variable, direction)%>%
      mutate(YTD = sum(val))%>%
      spread(var, val)%>%
      ungroup()%>%
      select(-direction)%>%
      relocate(YTD, .after = last_col())%>%
      rename(" " = variable)
      
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, " "),
          th(class = 'dt-center', colspan = ncol(df)-1, paste0("Fiscal Year ",unique(df$FY))),
        ),
        tr(
          lapply(setdiff(colnames(df), c("FY", " ")), th)
        )
      )
    ))
    
    
    df%>%
      select(-FY)%>%
      DT::datatable(options = list(dom = 't',columnDefs = list(list(className = 'dt-center'))),
                    rownames = FALSE, container = sketch, selection = "single")%>%
      formatStyle(
        columns = ncol(df)-2,
        backgroundColor = 'yellow'
      )
    
  })
  
  
  output$top_recipients_table<- DT::renderDataTable({
    
    combined_in%>%
      group_by(id,name)%>%
      filter(direction == "recipient")%>%
      mutate(quarter = as.Date(paste(year(date), (ceiling(month(date)/3)-1)*3+1, "01", sep = "-")),
             label = paste0(month.abb[month(quarter)], "-", month.abb[month(quarter)+2]))%>%
      group_by(id,name, quarter, label)%>%
      summarise(lb  = sum (lb))%>%
      group_by(id, name)%>%
      mutate(previous_avg_lb = round(mean(lb[quarter != max(quarter,na.rm=T)])))%>%
      ungroup()%>%
      filter(quarter == max(quarter))%>%
      arrange(-lb)%>%
      mutate(`Percent Change` = round((lb - previous_avg_lb) / previous_avg_lb,3))%>%
      select(id, name, "Quarter" = label, lb, "Previous Quarter Avg" = previous_avg_lb, `Percent Change`)%>%
      DT::datatable(rownames = FALSE, selection = "single")
    
      
    
  })
  
  output$top_donors_table<- DT::renderDataTable({
    
    combined_in%>%
      group_by(id,name)%>%
      filter(direction == "donor")%>%
      mutate(quarter = as.Date(paste(year(date), (ceiling(month(date)/3)-1)*3+1, "01", sep = "-")),
             label = paste0(month.abb[month(quarter)], "-", month.abb[month(quarter)+2]))%>%
      group_by(id,name, quarter, label)%>%
      summarise(lb  = sum (lb))%>%
      group_by(id, name)%>%
      mutate(previous_avg_lb = round(mean(lb[quarter != max(quarter,na.rm=T)])))%>%
      ungroup()%>%
      filter(quarter == max(quarter))%>%
      arrange(-lb)%>%
      mutate(`Percent Change` = round((lb - previous_avg_lb) / previous_avg_lb,3))%>%
      select(id, name, "Quarter" = label, lb, "Previous Quarter Avg" = previous_avg_lb, `Percent Change`)%>%
      DT::datatable(rownames = FALSE, selection = "single")
    
    
    
  })
  
  
  output$line_donor_recipient <- renderPlot({
    
    combined_in%>%
      Tabulate(input$trend)%>%
      group_by(var,direction)%>%
      summarise(lb = sum(lb))%>%
      #  mutate(direction =fct_reorder(direction, -lb))%>%
      ggplot(aes(x = var, y = lb, colour = direction))+
      geom_line(size = 2)+
      scale_y_continuous(label = addUnits)+
      labs(x = "", y = paste0(input$trend," Lbs"), title = paste0("Historical Comparison (", input$trend,")"), colour = "")+
      theme_bw()
    
  })
  
  

  
  output$total_stacked <- renderPlot({

    combined_in%>%
      Tabulate(input$trend)%>%
      #  mutate(direction =fct_reorder(direction, -lb))%>%
      ggplot(aes(x = var, y = lb, fill = type))+
      scale_fill_manual(values = colors)+
      geom_col()+
      scale_y_continuous(label = addUnits)+
      labs(x = "", y = paste0(input$trend," Lbs"), title = paste0("Historical Comparison (", input$trend,")"), fill = "")+
      facet_wrap(~direction)+
      theme_bw()
    
  })
  
  output$total_line <- renderPlot({
  
    combined_in%>%
      Tabulate(input$trend)%>%
      #  mutate(direction =fct_reorder(direction, -lb))%>%
      ggplot(aes(x = var, y = lb, colour = type))+
      scale_colour_manual(values = colors)+
      geom_line()+
      scale_y_continuous(label = addUnits)+
      labs(x = "", y = paste0(input$trend," Lbs"), title = paste0("Historical Comparison (", input$trend,")"), colour = "")+
      facet_wrap(~direction)+
      theme_bw()
  })

  
  #BY DONOR ORG TYPE ------------------------------
  
  donor_org_reactive<-reactive({
   combined_in%>%
      filter(direction == "donor")%>%
      mutate(quarter = as.Date(paste(year(date), (ceiling(month(date)/3)-1)*3+1, "01", sep = "-")))%>%
      left_join(donors%>%select(id, donor, org_type), by = "id")%>%
      group_by(org_type, quarter)%>%
      summarise(lb = sum(lb,na.rm=T))%>%
      mutate(org_type = ifelse(is.na(org_type), "missing", tolower(org_type)))%>%
      group_by(quarter)%>%
      mutate(perc = lb / sum(lb))
    
  })
  
  output$donor_org_table <-   DT::renderDataTable({
    req(donor_org_reactive)
    
    donor_org_reactive()%>%
      group_by(org_type)%>%
      mutate(label = paste0(month.abb[month(quarter)], "-", month.abb[month(quarter)+2]))%>%
      ungroup()%>%
      filter(quarter >= max(quarter) %m-% months(3))%>%
      group_by(org_type)%>%
      mutate(`Percent Change` = round(( lb[quarter == max(quarter)] - lb[quarter == min(quarter)]) / lb[quarter == min(quarter)], 3))%>%
      select(org_type, label, lb, `Percent Change`)%>%
      spread(label, lb)%>%
      relocate(`Percent Change`, .after = last_col())%>%
      DT::datatable(rownames = FALSE, selection = "single", options = list(dom = 't'))
    
  })
  
  output$donor_org_stacked <- renderPlot({
    req(donor_org_reactive)
    
    donor_org_reactive()%>%
      ggplot(aes(x = quarter, y = lb, fill = org_type))+
      geom_col()+
      scale_fill_manual(values = town_colors)+
      labs(x = "", y = "Proportion of Lbs", title = "Recipient Lbs, by Town", colour = "")+
      theme_bw()
  }) 
  
  
  
  output$donor_org_stacked_perc <- renderPlot({
    req(donor_org_reactive)
    
    donor_org_reactive()%>%
      ggplot(aes(x = quarter, y = perc, fill = org_type))+
      geom_col()+
      scale_fill_manual(values = town_colors)+
      labs(x = "", y = "Proportion of Lbs", title = "Recipient Lbs, by Town", colour = "")+
      theme_bw()
  }) 
  
  
  
  #BY Recipient CITY -------------------------------
      
    city_reactive<-reactive({
        combined_in%>%
          filter(direction == "recipient")%>%
          mutate(quarter = as.Date(paste(year(date), (ceiling(month(date)/3)-1)*3+1, "01", sep = "-")))%>%
          left_join(recipients%>%select(id, currently_active, acronym, recipient_agency, town), by = "id")%>%
          group_by(town, quarter)%>%
          summarise(lb = sum(lb,na.rm=T))%>%
          mutate(town = ifelse(is.na(town), "Missing", town))%>%
          group_by(quarter)%>%
          mutate(perc = lb / sum(lb))
      
    })
    

      
  output$city_table <-   DT::renderDataTable({
    req(city_reactive)
    
    city_reactive()%>%
      group_by(town)%>%
      mutate(label = paste0(month.abb[month(quarter)], "-", month.abb[month(quarter)+2]))%>%
      ungroup()%>%
      filter(quarter >= max(quarter) %m-% months(3))%>%
      group_by(town)%>%
      mutate(`Percent Change` = round(( lb[quarter == max(quarter)] - lb[quarter == min(quarter)]) / lb[quarter == min(quarter)], 3))%>%
      select(town, label, lb, `Percent Change`)%>%
      spread(label, lb)%>%
      relocate(`Percent Change`, .after = last_col())%>%
      DT::datatable(rownames = FALSE, selection = "single")
      
  })
    
  output$city_stacked <- renderPlot({
    req(city_reactive)
    
    city_reactive()%>%
        ggplot(aes(x = quarter, y = lb, fill = town))+
        geom_col()+
        scale_fill_manual(values = town_colors)+
        labs(x = "", y = "Proportion of Lbs", title = "Recipient Lbs, by Town", colour = "")+
        theme_bw()
    }) 
    
    
    
  output$city_stacked_perc <- renderPlot({
    req(city_reactive)
    
    city_reactive()%>%
      ggplot(aes(x = quarter, y = perc, fill = town))+
      geom_col()+
      scale_fill_manual(values = town_colors)+
      labs(x = "", y = "Proportion of Lbs", title = "Recipient Lbs, by Town", colour = "")+
      theme_bw()
  }) 
  
  #FOOD SECTION OF APP -----------------------------------------------------
  
  output$food_bar <- renderPlot({
    combined_in%>%
      Tabulate(input$trend)%>%
      group_by(direction, var)%>%
      mutate(perc_lb = lb/sum(lb))%>%
      ggplot(aes(x = var, y = perc_lb, fill = type))+
      geom_col()+
      scale_fill_manual(values = colors)+
      labs(x = "", y = "Proportion of Lbs", title = "Recipient Lbs, by Food Type", colour = "")+
      facet_wrap(~direction) + 
      theme_bw()
  }) 
  
    

  
  
  
}

shinyApp(ui = app_ui, server = app_server, options = list(launch.browser=T))
