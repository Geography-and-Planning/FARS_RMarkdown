library(tidyverse)
county<-youth_msa_county_wide_modified%>%
  filter(Main=="Main")

county<-county%>%
  select(cbsa_code, rate_total)%>%
  rename(county_rate= rate_total)

master_full<-left_join(master_fatality_cbsa30, county, by="cbsa_code")

master_full<-master_full%>%
  select(-child_pop,-rate_children,-fatality_children,-fatality_overall,-tot_pop)

write.csv(master_full, "master_full.csv", row.names = FALSE)
