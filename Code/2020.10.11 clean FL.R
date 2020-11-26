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
ReadDistributed<-function(month, path, year){
  #Notes:
  #Added in "Total" column to April 2020 tab
  #Deleted column 2 (empty) of May 2020 tab
  #Changed "other" to "Total" in May 2020.
  
  print(month)
  
  #skip_var = ifelse(month == "January", 0, 1)
  
  raw<-read_excel(path, sheet = month,  col_names = FALSE, skip = 1)%>%
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
           date = ifelse(grepl("date|Agency|Acronym", day) |nchar(day)>2, NA, paste(year, 
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
  
  
    return(clean)
}

ReadDistributedFlipped<-function(month, path, year){
  #Notes:
  #Added in "Total" column to April 2020 tab
  #Deleted column 2 (empty) of May 2020 tab
  #Changed "other" to "Total" in May 2020.
  
  print(month)
  
  #skip_var = ifelse(month == "January", 0, 1)
  
  raw<-read_excel(path, sheet = month,  col_names = FALSE)%>%
    clean_names()%>%
    rename("recipient_nickname" = x1, "recipient" = x2)%>%
    mutate(r = 1:n())%>%
    select(r, everything())%>%
    gather(var, val,x3:length(.))
  
  
  days<- raw%>%filter(r ==1)%>%
    fill (val)%>%
    mutate(date = ifelse(grepl("dated|grand|totals", tolower(val)), NA, 
                         paste(year, str_pad(match(month,month.name), side = "left", width = 2, pad = "0"),
                                     str_pad(val, side = "left", width = 2, pad = "0"), sep = "-")),
           date = as.Date(date))
  
  type <- raw%>%filter(r==2)%>%rename("type" = val)
  
  clean<-
    raw%>%
    left_join(days%>%select(var,date))%>%
    left_join(type%>%select(var, type))%>%
    filter(r!=1, r!=2)%>%
    select(date, recipient, recipient_nickname, type, "bx" = val)%>%
    mutate(bx = as.numeric(bx))%>%
    filter(!is.na(bx), bx!= 0, !is.na(date), recipient != "Hashtotal")%>%
    arrange(date, recipient)
  
  return(clean)
}


ReadReceived<- function(filename){
  print(filename)
  
  raw<-read_excel(paste0("Input/Received/", filename))%>%
    #clean_names()%>%
    select(contains("id"):contains("donor"))%>%
    gather(type, bx, `Bread and Bakery`: Compost)%>%
    clean_names()%>%
    mutate(date = as.Date(date))%>%
    select(date, id, donor, type, bx)%>%
    arrange(date, donor)%>%
    filter(bx!=0, !is.na(date))
    
}
  

#Import Data ---------------------------------------------------------------------
load("Intermediate/donors.Rda")
load("Intermediate/recipients.Rda")



#Crosswalk -----------------------------------------------------
acronym<-
  recipients%>%
  mutate(id = 1:n())%>%
  select(id, recipient_agency, "also_known_as" = acronym)%>%
  separate_rows(also_known_as, sep = ",")

recipients_long<-
  recipients%>%
  mutate(id = 1:n())%>%
  select(id, recipient_agency, also_known_as)%>%
  separate_rows(also_known_as, sep = ",")%>%
  bind_rows(.)%>%
  arrange(id)%>%
  mutate(also_known_as = trimws(also_known_as))%>%
  distinct(.keep_all=T)%>%
  gather(name_type, recipient, recipient_agency:also_known_as)%>%
  mutate(recipient = tolower(recipient))%>%
  filter(!duplicated(recipient),!is.na(recipient))



#Recipient Data --------------------------------------

#Relevant Months
ms_2019_v1<- c("January", "February", "March", "April", "May","July")
ms_2019_v2<- c("August", "September", "October", "November","December")
ms_2020<- c("January", "February", "March", "April", "May", "June", "July", "September")

#Distribution Tracking sheets
dist_raw_2020<-lapply(ms_2020,ReadDistributed, path = "Input/Distributed/2020 Distribution Tracking sheet v2.xlsx", year = 2020)%>%bind_rows()


dist_raw_2019_v1<-lapply(ms_2019_v1,ReadDistributedFlipped, path = "Input/Distributed/2019 Distribution Tracking_NB.xlsx", year = 2019)%>%bind_rows()

dist_raw_2019_v2<-lapply(ms_2019_v2,ReadDistributed, path = "Input/Distributed/2019 Distribution Tracking_NB.xlsx", year = 2019)%>%bind_rows()
  

all_dist<-dist_raw_2019_v1%>%bind_rows(dist_raw_2019_v2)%>%bind_rows(dist_raw_2020)%>%
  mutate(recipient = tolower(recipient),
         recipient_nickname = tolower(recipient_nickname))%>%
  filter(recipient!= "total", recipient!="totals", type != "Total")%>%
  mutate(type = ifelse(grepl("perish", type), "Non-perishables", type),
         type = ifelse(grepl("Protein", type), "Meat/Protein", type))
  


tabyl(all_dist$type)

recipient_names<-distinct(all_dist, recipient, recipient_nickname)%>%mutate(r_id = 1:n())%>%
  separate_rows(recipient_nickname, sep = ",")%>%
  mutate(recipient_nickname = trimws(recipient_nickname))%>%
  gather(ntype, t, recipient:recipient_nickname)%>%
  mutate(t = tolower(t))%>%
  left_join(recipients_long, by = c("t" = "recipient"))%>%
  arrange(r_id)

missing_recipients<-
  recipient_names%>%
  group_by(r_id)%>%
  filter(sum(!is.na(id))==0)%>%
  filter(ntype == "recipient")%>%
  ungroup()%>%
  select("recipient" = t)%>%
  left_join(distinct(all_dist, recipient, recipient_nickname))


#Mising matches w/ the population served file. Some of these are clear which is meant to be, others not.
#missing_orgs<-dist_raw%>%filter(is.na(org_type), recipient != "total")%>%group_by(recipient)%>%summarise(bx = sum(bx))

#Donor Data ---------------------------------------
file_names<-list.files("Input/Received", recursive = TRUE)
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
        )%>%
  select(-id)


donor_names<-
  donor_raw%>%
  distinct(donor)%>%
  left_join(donors%>%select(donor)%>%mutate(n = 1:n()))

missing_donors<-
  donor_names%>%
  filter(is.na(n))%>%
  select(-n)


#What do the totals look like for different org types
donor_raw%>%group_by(org_type)%>%summarise(sum(bx))

missing_donor<-donor_raw%>%filter(is.na(org_type))%>%group_by(donor)%>%summarise(bx = sum(bx))
#Combine data-----------------------------------------------------------------------------------------------

combined<-
  all_dist%>%
  select(date, "name" = recipient, "nickname" = recipient_nickname, type, bx )%>%
  mutate(direction = "recipient")%>%
  bind_rows(donor_raw%>%select(date, "name" = donor, type, bx)%>%mutate(direction = "donor"))%>%
  arrange(date)
  

#Save data --------------------------------------------------------------------------

#Write to R Data compressed file
# save(county_all, file = "Intermediate/county_all.Rda")
# save(national, file = "Intermediate/national.Rda")
# save(counties_tigris, file = "Intermediate/sp_counties.Rda")
# 
# #Write two FDA tables to CSV
# write_csv(national, "Intermediate/national.csv")
# write.csv(d, "Intermediate/donors.csv", row.names = FALSE, na = "")
# write.csv(combined_simple, "Intermediate/combined.csv", row.names = FALSE, na = "")
# write.csv(missing_orgs, "Intermediate/missing_recipients.csv", row.names = FALSE, na = "")
 write.csv(missing_recipients, "/Users/nathanbasch/Documents/FL/Intermediate/missing_recipients.csv", row.names = FALSE, na = "")
 write.csv(missing_donors, "/Users/nathanbasch/Documents/FL/Intermediate/missing_donors.csv", row.names = FALSE, na = "")
 write.csv(combined, "/Users/nathanbasch/Documents/FL/Intermediate/combined.csv", row.names = FALSE, na = "")
 