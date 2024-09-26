#Filters
source("./02_Scripts/000_Init.R")

library(tidygeocoder)
library(dplyr)


#####################
##### Rossmore ##########
#####################
walnut <- read_csv("walnut_creek_Roosmore_addresses.csv")





EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) 
  # filter(str_detect(mail_city, "ROSS|Ross"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
  #        party_category == "Democratic",
  #        voted_in_2020_general == "Yes"
  # ) 


# Assuming your data frame is named 'df' with columns: 'Street', 'City', 'State', 'Zip'
# Create a full address column
df <- EBRPD_district_2_voter_data_2 %>%
  mutate(mail_zip = substr(mail_zip, 1, 5))%>%
  #select(-full_address) %>% 
  #only for mapping
  mutate(mail_street = gsub("\\s+\\d+$", "", mail_street)) %>%
  mutate(full_address = paste(house_number, mail_street, mail_state, mail_zip, sep = ", ")) %>% 
  filter(!str_detect(mail_street, "PO BOX"), 
         mail_state == "CA" ) %>% 
  distinct(full_address, .keep_all = TRUE) %>% 
  filter(full_address %in% walnut$full_address) %>% 
  arrange(mail_street, mail_zip) %>% 
  select(full_address,apartment_number, house_number, party, name_first,name_last,last_voted,precinct_name,voted_vs_opportunities_group) %>% 
  arrange(house_number, full_address, apartment_number)

write.csv(df, "Rossmoore_clean_distinct.csv")
write.csv(EBRPD_district_2_voter_data_2, "Rossmoore_all_info.csv")