###############################################################################
###   AUTHOR: Nathan Basch
###   DATE: 5/6/2002
###
###   DESCRIPTION: LOAD AND CLEAN USDA QUICKSTATS DATA; LOAD TIGRIS SHAPEFILE
###############################################################################

### Set Environment -------------------------

# Clear the environment 
rm(list=ls())

# Set working directory
path <- "C:\\Users\\nated\\Documents\\Documents_NB\\Projects\\Food Link"
options(stringsAsFactors = F)
setwd(path)

# Load packages
library(tidyverse)
library(janitor)
library(readxl)
library(rjson)
library(plotly)
library(tigris)
library(maps)
library(leaflet)
library(rmapshaper)
library(scales)
library(lubridate)
library(gridExtra)

#Define function ---------------------------------------------------
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


#Import Data ---------------------------------------------------------------------
combined<-read_csv("Intermediate/combined.csv")



#Plot -----------------------------------------------------------------------
colors<-c("#c95e4a","#5aad6a","#c55a9f", "#c9a944","#777acd","#7e7c34")

#Plots of Type ------------------------------------------------

#Total
combined%>%
  #filter(direction == "Distributed")%>%
  group_by(type, direction)%>%
  summarise(bx = sum(bx))%>%
  mutate(direction =fct_reorder(direction, bx))%>%
  ggplot(aes(x = direction, y = bx, fill = type))+
  scale_fill_manual(values = colors)+
  geom_col()+
  labs(x = "", y = "Boxes", title = "Total Boxes: 2020")+
  theme_bw()


#Month
combined%>%
  mutate(month = as.Date(paste(year(date), month(date), "01",sep = "-")))%>%
  #filter(direction == "Distributed")%>%
  group_by(month, type, direction)%>%
  summarise(bx = sum(bx))%>%
  mutate(direction =fct_reorder(direction, -bx))%>%
  ggplot(aes(x = month, y = bx, fill = type))+
  scale_fill_manual(values = colors)+
  geom_col()+
  labs(x = "", y = "Monthly Boxes", title = "Boxes per Month")+
  geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
  facet_wrap(~direction)+
  theme_bw()

#Week
combined%>%
  Tabulate("Weekly")%>%
#  mutate(direction =fct_reorder(direction, -bx))%>%
  ggplot(aes(x = var, y = bx, fill = type))+
  scale_fill_manual(values = colors)+
  geom_col()+
  labs(x = "", y = "Weekly Boxes Distributed", title = "Boxes Distributed per Week")+
  geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
  facet_wrap(~direction)+
  theme_bw()

#Daily (bar)

gg_data%>%
  ggplot(aes(x = date, y = bx, fill = type))+
  scale_fill_manual(values = colors)+
  geom_col()+
  labs(x = "", y = "Daily Boxes Distributed", title = "Boxes Distributed per Day")+
  geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
  theme_bw()



#Daily (line)
combined%>%
  group_by(date, type, direction)%>%
  filter(direction == "Distributed")%>%
  summarise(bx = sum(bx))%>%
  ggplot(aes(x = date, y = bx, colour = type, linetype = direction))+
  geom_line(size = 1)+
  geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
  labs(x = "", y = "Daily Boxes Distributed", title = "Boxes Distributed per Day")+
  facet_wrap(~type, scales = "free")+
  scale_colour_manual(values = colors)+
  theme_bw()

#By Type
gg_data%>%
  mutate(week_date = floor_date(date, "week"))%>%
  group_by(week_date, type)%>%
  summarise(bx = sum(bx))%>%
  ggplot(aes(x = week_date, y = bx, colour = type))+
  geom_line(size = 1)+
  geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
  labs(x = "", y = "Weekly Boxes Distributed", title = "Boxes Distributed per Week")+
  scale_colour_manual(values = colors)+
  theme_bw()

#By Recipient
# 
# ggplotly(gg_data%>%
#   mutate(week_date = floor_date(date, "week"))%>%
#   group_by(week_date, recipient)%>%
#   summarise(bx = sum(bx))%>%
#   ggplot(aes(x = week_date, y = bx, colour = recipient))+
#   geom_line(size = 1)+
#   geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
#   labs(x = "", y = "Weekly Boxes Distributed", title = "Boxes Distributed per Week")+
#   #scale_colour_manual(values = colors)+
#   theme_bw())


#Pre vs Post COVID -----------------------------------------------------------
#Pre/Post Covid
combined%>%
  group_by(date, covid, direction)%>%
  summarise(bx = sum(bx))%>%
  group_by(covid, direction)%>%
  summarise(bx = mean(bx))%>%
  ggplot(aes(x = fct_reorder(direction, -bx), y = bx, fill = fct_reorder(covid, bx)))+
  scale_fill_manual(values = colors)+
  geom_col(position = "dodge")+
  labs(x = "", y = "Average Daily Boxes", title = "Boxes Distributed Pre/Post Covid", fill = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#By Food type
combined%>%
  group_by(date, covid, type, direction)%>%
  summarise(bx = sum(bx))%>%
  group_by(covid, type, direction)%>%
  summarise(bx = mean(bx))%>%
  ggplot(aes(x = type, y = bx, fill = fct_reorder(covid, bx)))+
  scale_fill_manual(values = colors)+
  geom_col(position = "dodge")+
  labs(x = "", y = "Average Daily Boxes", title = "Boxes Distributed Pre/Post Covid", fill = "")+
  facet_wrap(~fct_reorder(direction, -bx))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#V2
combined%>%
  group_by(date, covid, type, direction)%>%
  summarise(bx = sum(bx))%>%
  group_by(covid, type, direction)%>%
  summarise(bx = mean(bx))%>%
  ggplot(aes(x = fct_reorder(direction, -bx), y = bx, fill = fct_reorder(covid, bx)))+
  scale_fill_manual(values = colors)+
  geom_col(position = "dodge")+
  labs(x = "", y = "Average Daily Boxes", title = "Boxes Received/Distributed Pre/Post Covid", fill = "")+
  facet_grid(~type, switch = "x",space = "free_x", scales = "free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")



#Look at specific super markets --------------------------------------------------

combined%>%
  #mutate(week_date = floor_date(date, "week"))%>%
  filter(org_type == "Large Supermarket")%>%
  group_by(date, org_detail)%>%
  summarise(bx = sum(bx))%>%
  ggplot(aes(x = date, y = bx, colour = org_detail))+
  scale_colour_manual(values = colors)+
  geom_line(size = 1)+
  labs(x = "", y = "Monthly Boxes Received", title = "Boxes Received Weekly")+
  geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
  theme_bw()

#Look at whole foods specifically as an example:
combined%>%
  mutate(week_date = floor_date(date, "week"))%>%
  filter(org_detail == "Whole Foods")%>%
  group_by(week_date, name)%>%
  summarise(bx = sum(bx))%>%
  ggplot(aes(x = week_date, y = bx, fill = name))+
  scale_fill_manual(values = colors)+
  geom_col()+
  labs(x = "", y = "Monthly Boxes Received", title = "Whole Foods Boxes Received Weekly")+
  geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
  theme_bw()




#New chart ideas-------------------------------

#Show Distributed and Received on y and x axis to see correlation
#Maybe show before and after COVID?
combined%>%
  Tabulate("Weekly")%>%
  spread(direction, bx)%>%
  #  mutate(direction =fct_reorder(direction, -bx))%>%
  ggplot(aes(x = Received, y = Distributed, colour = type))+
  scale_color_manual(values = colors)+
  geom_point()+
  labs(x = "Weekly Boxes Received", y = "Weekly Boxes Distributed", title = "Boxes Distributed per Week")+
  #geom_vline(xintercept=as.Date("2020-03-10"), linetype="dashed", color = "grey", size = 1)+
  facet_wrap(~type)+
  theme_bw()

#Violin plots to show distribution of # of boxes for different types
combined%>%
  Tabulate("Daily")%>%
  #Remove outlier for now
  filter(bx<600)%>%
  ggplot(aes(x = direction, y = bx, fill = type))+
  geom_violin()+
  geom_boxplot(width = 0.2)+
  facet_wrap(~type, scales = "free")+
  theme_bw()


#Show daily number of pickups and drop offs per day, week, etc.(this would not be by type, but instead combined)
#maybe show 7 day rolling average on top?
combined%>%
  distinct(name, date, direction)%>%
  group_by(date,direction)%>%
  tally()%>%
  group_by(direction)%>%
  mutate(rolling = rollmean(n, 7, na.pad=TRUE, align = "right"))%>%
  ggplot()+
  geom_line(aes(x = date, y = n, colour = direction), size = 0.7, alpha = 0.2)+
  geom_line(aes(x = date, y = rolling, colour = direction), size = 1)+
  labs(x = "", y = "Daily Pickups and Drop offs")+
  theme_bw()
  
combined%>%
  group_by(date,direction)%>%
  summarise(bx = sum(bx))%>%
  group_by(direction)%>%
  mutate(rolling = rollmean(bx, 7, na.pad=TRUE, align = "right"))%>%
  ggplot()+
  geom_line(aes(x = date, y = bx, colour = direction), size = 0.7, alpha = 0.2)+
  geom_line(aes(x = date, y = rolling, colour = direction), size = 1)+
  labs(x = "", y = "Daily Boxes")+
  theme_bw()

# where does the food come from? Stacked bar weekly showing categories (NAs are "other")
combined%>%
  mutate(week = floor_date(date, "week"))%>%
  group_by(week, org_type, direction)%>%
  summarise(bx = sum(bx))%>%
  ungroup()%>%
  mutate(org_type = ifelse(is.na(org_type),"Other",org_type))%>%
  filter(direction == "Received")%>%
  ggplot(aes(x = week, y = bx, fill = org_type))+
  geom_col()+
  scale_fill_manual(values = c(colors, "orange"))+
  theme_bw()



#Shiny App -- 

#TAB: MAin dashboard
#Distributed and Recieved are automatically selected but you can just focus on one if they'd like
#Level of Tabulation in data
#STacked bar vs line chart (Line chart can be Facetted or on same axis)
# If bar is chosen, allow user to choose what to fill in colors (Default = Type, but could also be org_type)
#Choose specific Type (including ALL combined, which is not chosen by default)
#Choose specific Donors/Recipients  (Defualt = ALL). This should be dependenet on whether Recipient and/or Donor are chosen.


#Tab: COVID Checks
#Pre-Post Covid analysis (# of pickups, amoutn of food, outliers, etc.)
#Maybe return a table with the biggest single day donations, new recipients


#Tab: Donors
### Choose list of donors to compare (can be individual donors or donor categories)
### Choose level of analysis (Daily, Weekly, Monthly, etc.)
## Have an analysis/table of like Boxes of food Per Pickup, # of Pickups per week, Standard deviation of weekly pickups.
#Can we show a timeline of Donors (when they started to get on board, and how they've ramped up?)


#Tab: Recipients

#Tab: Foodlink operations
### Be able to search by volunteer.
### 

