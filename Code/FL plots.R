

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



