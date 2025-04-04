library(duckdb)
library(tidyverse)
library(dplyr)
library(sf)
library(tidycensus)

# Load the data
con <-dbConnect(duckdb::duckdb(), "data/raw_data.duckdb")


FARS2022<- dbGetQuery(con, "SELECT * FROM person2022")
FARS2021<- dbGetQuery(con, "SELECT * FROM person2021")
FARS2019<- dbGetQuery(con, "SELECT * FROM person2019")
FARS2018<- dbGetQuery(con, "SELECT * FROM person2018")
FARS2017<- dbGetQuery(con, "SELECT * FROM person2017")

dbDisconnect(con, shutdown = TRUE)


FARS2021<- FARS2021%>%
  filter(INJ_SEV==4)

FARS2022<- FARS2022%>%
  filter(INJ_SEV==4)

FARS2019<- FARS2019%>%
  filter(INJ_SEV==4)

FARS2018<- FARS2018%>%
  filter(INJ_SEV==4)

FARS2017<- FARS2017%>%
  filter(INJ_SEV==4)

FARS2022<- FARS2022%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2022$STATE, FARS2022$COUNTY) )

FARS2021<- FARS2021%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2021$STATE, FARS2021$COUNTY) )

FARS2019<- FARS2019%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2019$STATE, FARS2019$COUNTY) )

FARS2018<- FARS2018%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2018$STATE, FARS2018$COUNTY) )

FARS2017<- FARS2017%>%
  mutate(FIPS= sprintf("%02d%03d", FARS2017$STATE, FARS2017$COUNTY) )


FARS2022<- FARS2022%>%
  select(FIPS, PER_TYP, AGE)

FARS2021<- FARS2021%>%
  select(FIPS, PER_TYP, AGE)

FARS2019<- FARS2019%>%
  select(FIPS, PER_TYP, AGE)

FARS2018<- FARS2018%>%
  select(FIPS, PER_TYP, AGE)

FARS2017<- FARS2017%>%
  select(FIPS, PER_TYP, AGE)


complete<- rbind(FARS2022, FARS2021, FARS2019, FARS2018, FARS2017)

complete<- complete%>%
  mutate(age_group= case_when(
    AGE<18 ~ 1,
    AGE>=18 & AGE<200 ~ 2
  ))%>%
  filter(!is.na(age_group))

complete<- complete%>%
  select(-AGE)

complete<- complete%>%
  group_by(FIPS, age_group, PER_TYP)%>%
  summarise(count=n())


library(readxl)

cbsa<- read_excel("data/cbsa_list.xlsx") 

cbsa<- cbsa%>%
  rename(state= `FIPS State Code`,
         county= `FIPS County Code`)%>%
  rename(cbsa= `CBSA Code`,
         cbsa_name= `CBSA Title`)

top_30<-read.csv("data/together.csv")

cbsa_top30<- cbsa%>%
  filter(cbsa %in% top_30$cbsa_code)%>%
  select(cbsa, cbsa_name, state, county)

cbsa_top30$state<- as.numeric(cbsa_top30$state)
cbsa_top30$county<- as.numeric(cbsa_top30$county)

cbsa_top30$FIPS<-sprintf("%02d%03d",cbsa_top30$state, cbsa_top30$county)

complete<- complete%>%
  filter(FIPS %in% cbsa_top30$FIPS)

full<- left_join(cbsa_top30,complete, by="FIPS")

full<-full%>%
  group_by(cbsa, cbsa_name, age_group, PER_TYP)%>%
  summarise(count=sum(count))


full<-full %>%
  filter(PER_TYP<7)

children<- full%>%
  filter(age_group==1)

adults<-full%>%
  group_by(cbsa, cbsa_name, PER_TYP)%>%
  summarise(count=sum(count))

tot_pop2019<-get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= c(tot_pop= "B01001_001"),
  year= 2019,
  survey= "acs1"
)

tot_pop2019<- tot_pop2019%>%
  filter(GEOID %in% adults$cbsa)

tot2018<-get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= c(tot_pop= "B01001_001"),
  year= 2018,
  survey= "acs1"
)%>%
  filter(GEOID %in% adults$cbsa)

tot2017<-get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= c(tot_pop= "B01001_001"),
  year= 2017,
  survey= "acs1"
)%>%
  filter(GEOID %in% adults$cbsa)

tot2021<-get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= c(tot_pop= "B01001_001"),
  year= 2021,
  survey= "acs1"
)%>%
  filter(GEOID %in% adults$cbsa)

tot2022<-get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= c(tot_pop= "B01001_001"),
  year= 2022,
  survey= "acs1"
)%>%
  filter(GEOID %in% adults$cbsa)

pop<- rbind(tot_pop2019, tot2018, tot2017, tot2021, tot2022)

pop<-pop%>%
  group_by(GEOID)%>%
  summarise(tot_pop=sum(estimate))

adults<-left_join(adults, pop, by=c("cbsa"="GEOID"))

adults<-adults%>%
  mutate(rate= count/tot_pop*100000)

write.csv(adults, "data/adults_ratebyusergroup.csv")

walk<- adults%>%
  filter(PER_TYP==5)

children_2021<- get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= "B09001_001E",
  year= 2021,
  survey= "acs1"
)%>%
  filter(GEOID %in% children$cbsa)

children_2022<- get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= "B09001_001E",
  year= 2022,
  survey= "acs1"
)%>%
  filter(GEOID %in% children$cbsa)

children_2019<- get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= "B09001_001E",
  year= 2019,
  survey= "acs1"
)%>%
  filter(GEOID %in% children$cbsa)

children_2018<- get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= "B09001_001E",
  year= 2018,
  survey= "acs1"
)%>%
  filter(GEOID %in% children$cbsa)

children_2017<- get_acs(
  geography= "Metropolitan Statistical Area/Micropolitan Statistical Area",
  variables= "B09001_001E",
  year= 2017,
  survey= "acs1"
)%>%
  filter(GEOID %in% children$cbsa)

children_pop<- rbind(children_2022, children_2021, children_2019, children_2018, children_2017)

children_pop<-children_pop%>%
  group_by(GEOID)%>%
  summarise(child_pop=sum(estimate))

children<-left_join(children, children_pop, by=c("cbsa"="GEOID"))

children<-children%>%
  mutate(rate= count/child_pop*100000)

write.csv(children, "data/children_ratebyusergroup.csv")

walk_children<- children%>%
  filter(PER_TYP==5)
walk_children<-walk_children%>%
  select(cbsa, rate)%>%
  rename(walk_children=rate)

children_adults<- left_join(walk_children, walk, by="cbsa")
library(ggrepel)
ggplot(children_adults, aes(x=walk_children, y=rate,label=cbsa_name.x))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text_repel(size = 3, box.padding = 0.5, point.padding = 0.3)+
  labs(
    title="CBSA walking fatality rate",
    subtitle = "Top 30 most populated, 2017 - 2022",
    x="Children walking Fatality Rate (per 100,000)",
    y="Total walking Fatality Rate (per 100,000)"
  )+
  theme_minimal(base_size=12)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 10),  # Smaller legend title
    legend.text = element_text(size = 8),   # Smaller legend text
    legend.key.size = unit(0.6, "lines") 
  )

model1<-lm(rate~walk_children, data=children_adults)
summary(model1)

children_percentage<-children%>%
  group_by(cbsa, cbsa_name, PER_TYP)%>%
  summarise(count=sum(count))%>%
  mutate(percentage= count/sum(count)*100)

adults_percentage<-adults%>%
  group_by(cbsa, cbsa_name, PER_TYP)%>%
  summarise(count=sum(count))%>%
  mutate(percentage= count/sum(count)*100)

longer<- left_join(children_percentage, adults_percentage, by=c("cbsa"="cbsa", "PER_TYP"="PER_TYP"))%>%
  select(cbsa,cbsa_name.x, PER_TYP, percentage.x, percentage.y)%>%
  rename(children=percentage.x,
         adults=percentage.y)

write.csv(longer, "data/percentage.csv")

walk2<- longer%>%
  filter(PER_TYP==5)

ggplot(walk2, aes(x=children, y=adults, label=cbsa_name.x))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text_repel(size = 3, box.padding = 0.5, point.padding = 0.3)+
  labs(
    title="CBSA walking fatality percentage",
    subtitle = "Top 30 most populated, 2017 - 2022",
    x="Children walking Fatality percentage (%)",
    y="Total walking Fatality percentage (%)"
  )+
  theme_minimal(base_size=12)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 10),  # Smaller legend title
    legend.text = element_text(size = 8),   # Smaller legend text
    legend.key.size = unit(0.6, "lines") 
  )
longer<- longer%>%
  filter(PER_TYP!=3)

longer<- longer%>%
  mutate(mode= case_when(
    PER_TYP==1 ~ "Driver",
    PER_TYP==2 ~ "Passenger",
    PER_TYP==5 ~ "Walking",
    PER_TYP==6 ~ "Biking"
  ))
ggplot(longer, aes(x=children, y=adults, label=cbsa_name.x))+
  geom_point()+
  facet_wrap(~mode)+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text_repel(size = 3, box.padding = 0.5, point.padding = 0.3)+
  labs(
    title="CBSA walking fatality percentage",
    subtitle = "Top 30 most populated, 2017 - 2022",
    x="Children walking Fatality percentage (%)",
    y="Total walking Fatality percentage (%)"
  )+
  theme_minimal(base_size=12)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 10),  # Smaller legend title
    legend.text = element_text(size = 8),   # Smaller legend text
    legend.key.size = unit(0.6, "lines") 
  )

longer2<- left_join(children, adults, by=c("cbsa"="cbsa", "PER_TYP"="PER_TYP"))%>%
  select(cbsa,cbsa_name.x, PER_TYP, rate.x, rate.y)%>%
  rename(children=rate.x,
         adults=rate.y)

longer2<- longer2%>%
  filter(PER_TYP!=3)
longer2<- longer2%>%
  mutate(mode= case_when(
    PER_TYP==1 ~ "Driver",
    PER_TYP==2 ~ "Passenger",
    PER_TYP==5 ~ "Walking",
    PER_TYP==6 ~ "Biking"
  ))
ggplot(longer2, aes(x=children, y=adults, label=cbsa_name.x))+
  geom_point()+
  facet_wrap(~mode)+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text_repel(size = 3, box.padding = 0.5, point.padding = 0.3)+
  labs(
    title="CBSA fatality rate by mode",
    subtitle = "Top 30 most populated, 2017 - 2022",
    x="Children Fatality Rate (per 100,000)",
    y="Total Fatality Rate (per 100,000)"
  )+
  theme_minimal(base_size=12)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 10),  # Smaller legend title
    legend.text = element_text(size = 8),   # Smaller legend text
    legend.key.size = unit(0.6, "lines") 
  )