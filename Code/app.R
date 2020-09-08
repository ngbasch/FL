###############################################################################
###   AUTHOR: Nathan Basch
###   DATE: 5/6/2002
###
###   DESCRIPTION: SHINY APP TO VIEW USDA QUICKSTATS DATA
###############################################################################

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

#Read Data ---------------------------------------------------
#API KEY: AIzaSyBcX7SpGnEGxE9ePGuCEt5R9M-ycq0dpuM
#register_google(key = "AIzaSyBcX7SpGnEGxE9ePGuCEt5R9M-ycq0dpuM")
#key<-"1M5998XCn6OHJ5bDVikpLfnJAlASLN6V"


df_names<-c("combined", "donors", "recipients")
u<-paste0("https://github.com/ngbasch/FL/raw/master/Intermediate/",df_names,".Rda")

for (i in (1:length(df_names))){
  load(url(u[i]))
}

#combined_in<-read_csv("Intermediate/combined.csv")
#donors<-read_csv("Intermediate/donors.csv")
#recipients<-read_csv("Intermediate/recipients.csv")

colors<-c("#c95e4a","#5aad6a","#c55a9f", "#c9a944","#777acd","#7e7c34")

#Clean data -----------------------------------------------------


#Calculate weekly boxes
weekly_boxes<-
  combined_in%>%
  mutate(week = floor_date(date, "week"))%>%
  group_by(week, type, name, direction)%>%
  summarise(week_bx = sum(bx, na.rm=T) )%>%
  group_by(type, name, direction)%>%
  summarise(weekly_bx = mean(week_bx))%>%
  arrange(type)%>%
  group_by(name, direction)%>%
  summarise(weekly_label = paste(paste0(type,": ", round(weekly_bx,1), "bx / wk"), collapse = "<br>"),
            total_weekly_boxes = sum(weekly_bx))
  
  

locations<-donors%>%select("agency" = donor, lon, lat)%>%mutate(label = "donor", color = "blue")%>%
  bind_rows(
    recipients%>%
      select("agency" = recipient_agency, lon, lat)%>%
      mutate(label = "recipient", color = "red")
    )%>%
  filter(!is.na(lon))%>%
  mutate(agency = tolower(agency))%>%
  left_join (weekly_boxes%>%select(-direction), by = c("agency" = "name"))%>%
  mutate(weekly_label= ifelse(is.na(weekly_label), "", weekly_label))

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
      level == "Annually" ~ as.Date(paste(year(date), "01", "01", sep = "-"))
    )
    )%>%
    group_by(var, type, direction)%>%
    summarise(bx = sum(bx))
}


#Set units and crop colors --------------------------------------
#units<-county_all%>%group_by(statisticcat_desc, unit_desc, commodity_desc)%>%summarise(t = max(value_num))
#classes<-county_all%>%distinct(commodity_desc, class_desc)

#APP -----------------------------------------------------------------------------------------------------
ui<- navbarPage("Food Link", id="nav",
                
                tabPanel("Interactive Map",
                         div(class="outer",
                             tags$head(
                               # Include custom CSS
                               includeCSS("https://raw.githubusercontent.com/ngbasch/indigo-case/master/styling.css")
                             ),
                             #Interactive Map UI -----------------------------------------------------------------------
                             leafletOutput("map", width="100%", height="100%"),
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = "auto", height = "auto",
                                           
                                           #Control Panel:
                                           selectizeInput("direction", "Facilities:", multiple = T, c("donor","recipient"), selected = c("donor", "recipient")),
                                          # pickerInput("goods", "Goods", sort(unique(combined_in$type)),
                                          #             multiple = TRUE,
                                          #             selected = sort(unique(combined_in$type)),
                                          #             options = list(`live-search` = TRUE, `actions-box` = TRUE)),
                                           awesomeCheckbox(inputId = "addmarkers", label = "Show Facility Markers", value = TRUE),
                                           awesomeCheckbox(inputId = "addcircles", label = "Show Density Circles"),
                                           awesomeCheckbox(inputId = "addheatmap", label = "Show Density Heatmap"),
                                           selectInput("trend", "Trend", c("Daily","Weekly", "Monthly", "Annually"), selected = "Monthly"),
                                            plotlyOutput("time_series")
                                           #textOutput("Summary Stats"),
                                           #plotOutput("temporalPlot", height = 200)
                             ),
                             #plotlyOutput("time_series"),
                             #Footnote
                             tags$div(id="cite",
                                      'App by Nathan Basch')
                         ))
                #National Trends UI -----------------------------------------------------------
                # tabPanel("Time Trends",
                #          sidebarLayout(
                #            #Panel Choices
                #            sidebarPanel(
                #              sliderTextInput("year_nat", "Years", 
                #                              choices = as.character(seq(1990,2019,1)),
                #                              selected = c("1990", "2019")),
                #              pickerInput("crop_nat", 
                #                          "Crop", 
                #                          multiple = TRUE,
                #                          choices = sort(unique(county_all$commodity_desc)),
                #                          selected = "CORN",
                #                          options = list(`live-search` = TRUE,
                #                                         `actions-box` = TRUE)),
                #              uiOutput("variety_nat"),
                #              selectInput("metric_nat", "Metric", sort(unique(county_all$statisticcat_desc)), selected = "PRODUCTION"),
                #              width = 2
                #            ),
                #            #Plotly Output
                #            mainPanel(
                #              fluidRow(column(12, plotlyOutput("national_plot", height = '600px')))
                #            )
                #          )
                # ),
                # #County Table UI -----------------------------------------------------------
                # tabPanel("All Counties Table",
                #          uiOutput("link"),
                #          hr(),
                #          DT::dataTableOutput("county_table")
                # )
)



server<-  function(input, output, session) {
  
  #Interactive Map Server ----------------------------------------------------------------
  #Update input to display the correct varieties based on crop 
  # observeEvent(input$crop,{
  #   updateSelectizeInput(session = session, inputId = 'variety',
  #                        choices = county_all%>%
  #                          filter(commodity_desc==input$crop,year == input$year)%>%
  #                          pull(class_desc)%>%unique()%>%sort(),
  #                        selected = "ALL CLASSES")
  # })
  
  #Only allow user to choose years where data exists for a given crop (Cotton data does not exist in 2019) 
  # observeEvent(input$crop,{
  #   years= county_all%>%filter(commodity_desc==input$crop)%>%pull(year)%>%unique()%>%sort()
  #   
  #   updateSelectizeInput(session = session, inputId = 'year',
  #                        choices = years,
  #                        selected = input$year
  #   )
  # })
  
  #Only allow user to choose crops where data exists for a given year
  # observeEvent(input$year,{
  #   crops= county_all%>%filter(year==input$year)%>%pull(commodity_desc)%>%unique()%>%sort()
  #   
  #   updateSelectizeInput(session = session, inputId = 'crop',
  #                        choices = crops,
  #                        selected = input$crop)
  #   
  # })
  
  #For Logs, print user input:
  #observe({print(c(input$year, input$crop,input$variety, input$metric))})
  
  #Reactive expression for the data subsetted to what the user selected
  locations_reactive<-reactive({

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
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")%>%
      setView(lng = -71.1, lat = 42.4, zoom = 11)
    
    
  })
  
  
  #Heatmap code
  bins <- c(0,1,2,3, 4, 5)
  pal <- colorBin("Spectral", domain = locations$total_weekly_boxes, bins = bins, na.color = "transparent")

  leaflet(locations%>%filter(label == "donor")) %>%
    addProviderTiles("CartoDB.Positron")%>%
    setView(lng = -71.1, lat = 42.4, zoom = 11)%>%
    addHeatmap(lng = ~lon, lat = ~lat, intensity = ~total_weekly_boxes, max = 0.5, blur = 55, radius = 20)%>%
    addMarkers(~lon, ~lat, layerId =~agency,# icon = leafIcons,
               popup = ~paste0('<strong>',as.character(label), " - ",as.character(agency), '</strong>',
                               "<br><br>", weekly_label), options = markerOptions(opacity = 0.8)
    )%>%
    addLegend(pal = pal, values = ~total_weekly_boxes,
              title="Heat map legend")
  
  
    
  

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
    
    if (input$addmarkers == FALSE)
      return()
    
    isolate({
      leafletProxy("map", data = temp) %>%
      addMarkers(data = temp,~lon, ~lat, layerId =~agency, icon = leafIcons,
                 popup = ~paste0('<strong>',as.character(label), " - ",as.character(agency), '</strong>',
                                 "<br><br>", weekly_label)
                 )
    })
  })
  
  # clearShapes()%>%
  #   clearHeatmap()%>%
    
  
  #Add heat density points
  observe({
    req(locations_reactive)
    temp<-locations_reactive()
    
    
    leafletProxy("map") %>% clearShapes()

    if (input$addcircles == FALSE)
      return()
    
    radius = temp[["total_weekly_boxes"]] / median(temp[["total_weekly_boxes"]],na.rm=T) * 400
    
    
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
        addHeatmap(lng = ~lon, lat = ~lat, intensity = ~total_weekly_boxes, max = 0.5, blur = 55, radius = 30)      
    })
  })
  
  
  
  
  
  observeEvent(input$map_marker_click, {
  
    click <- input$map_marker_click
    
    
    output$time_series <- renderPlotly({
    direction_in = locations %>% filter(agency == click$id)%>%pull(label)
    direction_in = ifelse(direction_in == "donor", "Received", "Distributed")
      
      
      
    gg_df<-
      combined_in%>%
        filter(name == click$id)
    ggtitle<-click$id
    
    if(nrow(gg_df) == 0){ 
      gg_df<-combined_in%>%
        filter(direction == direction_in)
      
    ggtitle<-paste0("All Food Link: ", direction_in)
      
    }
    
    gg<-gg_df%>%
        Tabulate(input$trend)%>%
        #  mutate(direction =fct_reorder(direction, -bx))%>%
          ggplot(aes(x = var, y = bx, fill = type))+
          scale_fill_manual(values = colors)+
          geom_col()+
          labs(x = "", y = paste0(input$trend," Boxes"), title = ggtitle, fill = "")+
          geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
          facet_wrap(~direction)+
          theme_bw()
      
    ggplotly(gg)
    })
  
  })
  
  
  
  
  #Create plot of national totals on the "Interactive Map" tab
  # output$temporalPlot <- renderPlot({
  #   req(national_df)
  #   
  #   unit<-units%>%filter(statisticcat_desc == input$metric, commodity_desc == input$crop)%>%pull(unit_desc)
  #   
  #   gg<-national_df()%>%
  #     mutate(highlight = ifelse(year == input$year, "Y","N"))%>%
  #     ggplot(aes(x = year, y = value_num, fill = highlight))+
  #     geom_col()+
  #     scale_fill_manual( values = c( "Y"="tomato", "N"="gray" ), guide = FALSE )+
  #     scale_y_continuous(label = addUnits)+
  #     labs(x = "Year", y = paste0(input$metric, " (", unit,")"), 
  #          title = "National Total")+
  #     theme_minimal()
  #   
  #   return(gg)
  #   
  # })
  
  # National Trends Server ----------------------------------------------------------
  
  
}

shinyApp(ui, server,  options = list(launch.browser=T))
