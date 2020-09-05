###############################################################################
###   AUTHOR: Nathan Basch
###   DATE: 5/6/2002
###
###   DESCRIPTION: SHINY APP TO VIEW USDA QUICKSTATS DATA
###############################################################################

### Set Environment -------------------------
options(stringsAsFactors = F)
options(shiny.usecairo=TRUE)

#Load libraries
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(tigris)
library(DT)
library(ggmap)

#Read Data ---------------------------------------------------
#API KEY: AIzaSyBcX7SpGnEGxE9ePGuCEt5R9M-ycq0dpuM
register_google(key = "AIzaSyBcX7SpGnEGxE9ePGuCEt5R9M-ycq0dpuM")


donors<-read_excel("Input/Complete Crosswalk.xlsx", sheet = "Food Donors")%>%
  clean_names()%>%
  filter(!is.na(donor))%>%
  mutate_geocode(donor, source = "google")

recipients<-read_excel("Input/Complete Crosswalk.xlsx", sheet = "Recipent Agencies")%>%
  clean_names()%>%
  filter(!is.na(recipient_agency))%>%
  mutate(full_address = ifelse(is.na(delivery_address),recipient_agency,paste0(delivery_address, ", ", town,", MA")))%>%
  mutate_geocode(full_address, source = "google")


#Write to CSV ----------------------------------------------------
write.csv(donors, "Intermediate/donors.csv", row.names = FALSE, na = "")
write.csv(recipients, "Intermediate/recipients.csv", row.names = FALSE, na = "")
