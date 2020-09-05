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
path <- "/Users/nathanbasch/Documents/FL"
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
ReadDistributed<-function(month){
  #Notes:
  #Added in "Total" column to April 2020 tab
  #Deleted column 2 (empty) of May 2020 tab
  #Changed "other" to "Total" in May 2020.
  
  print(month)
  
  skip_var = ifelse(month == "January", 0, 1)
  
  raw<-read_excel("Input/Distributed/2020 Distribution Tracking sheet.xlsx", sheet = month,  col_names = FALSE, skip = skip_var)%>%
    clean_names()%>%
    mutate(r = 1:n(),
           #Delete 11 rows after the "Total" cell in column 1
           delete = r[ifelse(is.na(x1), FALSE, x1 == "Total")]+11,
           month = match(month,month.name),
           type = ifelse(is.na(x2), "Waste",x2)) %>%
    filter(r<delete)%>%
    fill(x1)%>%
    filter(x1!= "Total")%>%
    mutate(day = gsub("\\..*","",x1),
           date = ifelse(grepl("date|Agency|Acronym", day), NA, paste("2020", 
                                                   str_pad(month, side = "left", width = 2, pad = "0"), 
                                                   str_pad(day, side = "left", width = 2, pad = "0"), sep = "-")),
           date = as.Date(date))%>%
    select(r, date, type, everything(), -delete, -x1, -x2, -month, -day )
  
    vars<-
      raw%>%
      select(r,x3:length(.))%>%
      filter(r<=2)%>%
      gather(var, val,x3:length(.))%>%
      filter(!is.na(val))%>%
      spread(r, val)%>%
      rename("recipient"=`1`, "recipient_nickname"=`2`)
  
    clean<-raw%>%
      filter(r>=3)%>%
      select(-r)%>%
      gather(var, bx,x3:length(.))%>%
      mutate(bx = as.numeric(bx))%>%
      left_join(vars, by = "var")%>%
      filter(!is.na(bx), bx != 0)%>%
      select(-var)%>%
      arrange(date, recipient)%>%
      select(date, recipient, recipient_nickname, type, bx)
  
  
    #Looks like totals match. Weird that it doesn't look like they're counting totals
    test_total<-
      clean%>%
      filter(recipient != "Total")%>%
      group_by(type, date)%>%
      summarise(bx = sum(bx))%>%
      left_join(clean%>%filter(recipient == "Total"), by = c("type", "date"))%>%
      mutate(dif = bx.x - bx.y)
    
    test_recipient<-
      clean%>%
      filter(recipient != "Total")%>%
      group_by(type)%>%
      summarise(bx = sum(bx))
    
    return(clean)
}

ReadReceived<- function(filename){
  print(filename)
  
  raw<-read_excel(paste0("Input/Received/", filename))%>%
    select(Id:Donor)%>%
    gather(type, bx, `Bread and Bakery`: Compost)%>%
    clean_names()%>%
    mutate(date = as.Date(date))%>%
    select(date, id, donor, type, bx)%>%
    arrange(date, donor)%>%
    filter(bx!=0, !is.na(date))
    
}
  

#Import Data ---------------------------------------------------------------------

#Recipient Data --------------------------------------
#Population served sheet
pop_served<-read_excel("Input/New Agency_Community summary.xlsx", sheet = "Population Served 531")%>%clean_names()%>%
  select(site, "org_type"=type, community, "org_detail" = population, "individuals"=number_of_indviduals)%>%
  mutate(site = tolower(site),
         org_type = tolower(org_type))%>%
  #Why are there two broadway towers?
  filter(!is.na(site), !duplicated(site))

#Relevant Months
ms<- c("January", "February", "March", "April", "May")

#Distribution Tracking sheets
dist_raw<-lapply(ms,ReadDistributed)%>%bind_rows()%>%
  mutate(recipient = tolower(recipient))%>%
  left_join(pop_served, by = c("recipient" = "site"))

#Mising matches w/ the population served file. Some of these are clear which is meant to be, others not.
missing_orgs<-dist_raw%>%filter(is.na(org_type), recipient != "total")%>%group_by(recipient)%>%summarise(bx = sum(bx))

#Donor Data ---------------------------------------
file_names<-list.files("Input/Robot Data")
file_names<-file_names[!grepl("~", file_names)]


donor_raw<-lapply(file_names, ReadReceived)%>%bind_rows()%>%
  #Categorize Supermarkets versus other organizations
  mutate(donor = tolower(donor),
         org_type = case_when (
                    #Check these for categorization and accuracy
                    grepl("school|arlington eats", donor) ~ "School/Program",
                    grepl("stonyfield", donor)   ~ "Farm",
                    grepl(paste("bake", "wicked bagels", "great harvest", "dunkin'", "patisserie", "panera", sep = "|"), donor) ~ "Restaurant/Bakery",
                    donor %in%  c("bonafede produce", "fairfoods", "arrow farms","sysco", "kettle cuisine", "disilva", "disilvia") ~ "Wholesale",
                    grepl(paste("agastinos", "russo", sep = "|"), donor) ~ "Local supermarket",
                    grepl("wegman|whole foods|trader joe|stop & shop|stop and shop|costco", donor) ~ "Large Supermarket"
                              
                               ),
         org_detail = case_when(
           grepl("wegman", donor) ~ "Wegmans",
           grepl("whole foods", donor) ~ "Whole Foods",
           grepl("trader joe", donor) ~ "Trader Joe's",
           grepl("stop & shop|stop and shop", donor) ~ "Stop and Shop",
           grepl("costco", donor) ~ "Costco"
          )
        )
#What do the totals look like for different org types
donor_raw%>%group_by(org_type)%>%summarise(sum(bx))

missing_donor<-donor_raw%>%filter(is.na(org_type))%>%group_by(donor)%>%summarise(bx = sum(bx))
#Combine data-----------------------------------------------------------------------------------------------

combined<-
  dist_raw%>%
  rename("name" = recipient, "name2" = recipient_nickname)%>%
  mutate(direction = "Distributed")%>%
  bind_rows(donor_raw%>%
              mutate(name2 = NA,
                     type = ifelse(grepl("Bakery", type), "Bakery",
                                   ifelse(grepl("Meat", type), "Meat/Protein", type)),
                     direction = "Received")%>%
                     select(-id)%>%
                      rename("name" = donor))%>%
  filter(type != "Total", type != "Waste", type != "Compost", name != "Total")%>%
  #Make name and name2 lowercase
  mutate(name = tolower(name),
         name2 = tolower(name2),
         covid = ifelse(date>="2020-03-10", "Post-Covid","Pre-Covid"))%>%
  filter(name!="total")
  #select(-community, -org_detail, -individuals)

combined_simple<-
  combined%>%
  select(date, name, type, bx, direction)%>%
  arrange(date)

#Categorize Donors/Recipients ---------------------------------



#Plot -----------------------------------------------------------------------
colors<-c("#c95e4a","#5aad6a","#c55a9f", "#c9a944","#777acd","#7e7c34")

gg_data<-dist_raw%>%
  filter(type != "Total", type != "Waste", recipient != "Total")



#Plots of Type ------------------------------------------------

#Total
combined%>%
  filter(name!="total")%>%
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
  filter(name!="total")%>%
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
  filter(name!="total")%>%
  mutate(week_date = floor_date(date, "week"))%>%
  #filter(direction == "Distributed")%>%
  group_by(week_date, type, direction)%>%
  summarise(bx = sum(bx))%>%
  ungroup()%>%
  mutate(direction =fct_reorder(direction, -bx))%>%
  ggplot(aes(x = week_date, y = bx, fill = type))+
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




#Save data --------------------------------------------------------------------------

#Write to R Data compressed file
# save(county_all, file = "Intermediate/county_all.Rda")
# save(national, file = "Intermediate/national.Rda")
# save(counties_tigris, file = "Intermediate/sp_counties.Rda")
# 
# #Write two FDA tables to CSV
# write_csv(national, "Intermediate/national.csv")
 write.csv(d, "Intermediate/donors.csv", row.names = FALSE, na = "")
 write.csv(combined_simple, "Intermediate/combined.csv", row.names = FALSE, na = "")
 write.csv(missing_orgs, "Intermediate/missing_recipients.csv", row.names = FALSE, na = "")
