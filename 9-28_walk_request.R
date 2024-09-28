#Filters
source("./02_Scripts/000_Init.R")

library(tidygeocoder)
library(dplyr)


#####################
##### Trestle Glen Road. All adresses Up to Vallent (where it gets too steep to walk) ##########
#####################

EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(str_detect(mail_street, "TRESTLE GLEN"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
         party_category == "Democratic",
         voted_in_2020_general == "Yes"
  ) 


# Assuming your data frame is named 'df' with columns: 'Street', 'City', 'State', 'Zip'
# Create a full address column
df <- EBRPD_district_2_voter_data_2 %>%
  mutate(mail_zip = substr(mail_zip, 1, 5))%>%
  #select(-full_address) %>% 
  #only for mapping
  mutate(mail_street = gsub("\\s+\\d+$", "", mail_street)) %>%
  mutate(full_address = paste( mail_street, mail_state, mail_zip, sep = ", ")) %>% 
  filter(!str_detect(mail_street, "PO BOX"),
         mail_state == "CA" ,
         house_number < 1700) %>%
  distinct(full_address, .keep_all = TRUE) %>% 
  arrange(mail_street, mail_zip) %>% 
  select(full_address,apartment_number, house_number, party, name_first,name_last, email, phone_1, voted_vs_opportunities_group, voted_in_2020_general ,precinct_name) %>% 
  arrange(house_number, full_address, apartment_number)

geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude) 

write.csv(df, "TRESTLE GLEN_clean_distinct.csv")
write.csv(EBRPD_district_2_voter_data_2, "TRESTLE GLEN_all_info.csv")

######### MAP ------------------------
dot_map <- leaflet(data = geocoded_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    radius = 2, 
    color = "blue", 
    fillOpacity = 0.6,
    label = ~full_address,  # Add label to show full address on hover
    labelOptions = labelOptions(
      noHide = FALSE,
      direction = "auto"
    )
  )
dot_map
# Save the map to an HTML file
library(htmlwidgets)
saveWidget(dot_map, file = "dot_TRESTLE_GLEN_Hover.html")

########################
########################

#####################
##### Shafter off college all. All the way to MacArthur(Manila/Lawton and Chabot too) ##########
#####################

EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(str_detect(mail_street, "Shafter|SHAFTER|Chabot|CHABOT|LAWTON|Lawton|Manila|MANILA"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
         party_category == "Democratic",
         voted_in_2020_general == "Yes"
  ) 


# Assuming your data frame is named 'df' with columns: 'Street', 'City', 'State', 'Zip'
# Create a full address column
df <- EBRPD_district_2_voter_data_2 %>%
  mutate(mail_zip = substr(mail_zip, 1, 5),
         street_name = str_remove_all(mail_street, "[0-9]"))%>%
  #select(-full_address) %>% 
  #only for mapping
  mutate(mail_street = gsub("\\s+\\d+$", "", mail_street)) %>%
  mutate(full_address = paste( mail_street, mail_state, mail_zip, sep = ", ")) %>% 
  filter(!str_detect(mail_street, "PO BOX"),
         mail_state == "CA" ) %>%
  distinct(full_address, .keep_all = TRUE)%>% 
  arrange(street_name, house_number, apartment_number) %>% 
 # arrange(mail_street, mail_zip) %>% 
  select(full_address,apartment_number, house_number, party, name_first,name_last, email, phone_1, voted_vs_opportunities_group, voted_in_2020_general ,precinct_name)

geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude) 
saveRDS(geocoded_data ,"Shafter_clean.rds")

write.csv(df, "Shafter_clean_distinct.csv")
write.csv(EBRPD_district_2_voter_data_2, "Shafter_all_info.csv")

######### MAP ------------------------
dot_map <- leaflet(data = geocoded_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    radius = 2, 
    color = "blue", 
    fillOpacity = 0.6,
    label = ~full_address,  # Add label to show full address on hover
    labelOptions = labelOptions(
      noHide = FALSE,
      direction = "auto"
    )
  )
dot_map
# Save the map to an HTML file
library(htmlwidgets)
saveWidget(dot_map, file = "dot_Shafter_Hover.html")

################
#####################
##### 3. highland Ave between Oakland ave and Moraga ave ##########
#####################

EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(str_detect(mail_street, "highland|HIGHLAND"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
         party_category == "Democratic",
         voted_in_2020_general == "Yes"
  ) 


# Assuming your data frame is named 'df' with columns: 'Street', 'City', 'State', 'Zip'
# Create a full address column
df <- EBRPD_district_2_voter_data_2 %>%
  mutate(mail_zip = substr(mail_zip, 1, 5),
         street_name = str_remove_all(mail_street, "[0-9]"))%>%
  #select(-full_address) %>% 
  #only for mapping
  mutate(mail_street = gsub("\\s+\\d+$", "", mail_street)) %>%
  mutate(full_address = paste( mail_street, mail_state, mail_zip, sep = ", ")) %>% 
  filter(!str_detect(mail_street, "PO BOX"),
         mail_state == "CA" ) %>%
  distinct(full_address, .keep_all = TRUE)%>% 
  arrange(street_name, house_number, apartment_number) %>% 
  # arrange(mail_street, mail_zip) %>% 
  select(full_address,apartment_number, house_number, party, name_first,name_last, email, phone_1, voted_vs_opportunities_group, voted_in_2020_general ,precinct_name)

geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude) 
saveRDS(geocoded_data ,"Highland_geo.rds")
geocoded_data <- geocoded_data %>% 
  filter(house_number != 301,
         full_address != "10 HIGHLAND WAY, CA, 94611")
write.csv(df, "highland_clean_distinct.csv")
write.csv(EBRPD_district_2_voter_data_2, "highland_all_info.csv")

######### MAP ------------------------
dot_map <- leaflet(data = geocoded_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    radius = 2, 
    color = "blue", 
    fillOpacity = 0.6,
    label = ~full_address,  # Add label to show full address on hover
    labelOptions = labelOptions(
      noHide = FALSE,
      direction = "auto"
    )
  )
dot_map
# Save the map to an HTML file
library(htmlwidgets)
saveWidget(dot_map, file = "dot_highland_Hover.html")


################
#####################
##### 4. lakeshore road (go past the merchants near lake)  then good flat houses rest of lakeshore. ##########
#####################

EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(str_detect(mail_street, "Lakeshore|LAKESHORE|Lakeside|LAKESIDE|BELLEVUE|Bellevue"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
         party_category == "Democratic",
         voted_in_2020_general == "Yes"
  ) 


# Assuming your data frame is named 'df' with columns: 'Street', 'City', 'State', 'Zip'
# Create a full address column
df <- EBRPD_district_2_voter_data_2 %>%
  mutate(mail_zip = substr(mail_zip, 1, 5),
         street_name = str_remove_all(mail_street, "[0-9]"))%>%
  #select(-full_address) %>% 
  #only for mapping
  mutate(mail_street = gsub("\\s+\\d+$", "", mail_street)) %>%
  mutate(full_address = paste( mail_street, mail_state, mail_zip, sep = ", ")) %>% 
  filter(!str_detect(mail_street, "PO BOX"),
         mail_state == "CA" ) %>%
  #distinct(full_address, .keep_all = TRUE)%>% 
  arrange(street_name, house_number, apartment_number) %>% 
  # arrange(mail_street, mail_zip) %>% 
  select(full_address,apartment_number, house_number, party, name_first,name_last, email, phone_1, voted_vs_opportunities_group, voted_in_2020_general ,precinct_name)

geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude) 
saveRDS(geocoded_data ,"lake_geo.rds")

geocoded_data <- readRDS("lake_geo.rds")

geocoded_data <- geocoded_data %>%
  filter(!str_detect(full_address,"374|3450|244|94611"))
write.csv(df, "lake_clean_distinct.csv")
write.csv(EBRPD_district_2_voter_data_2, "Lake_all_info.csv")

######### MAP ------------------------
dot_map <- leaflet(data = geocoded_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    radius = 2, 
    color = "blue", 
    fillOpacity = 0.6,
    label = ~full_address,  # Add label to show full address on hover
    labelOptions = labelOptions(
      noHide = FALSE,
      direction = "auto"
    )
  )
dot_map
# Save the map to an HTML file
library(htmlwidgets)
saveWidget(dot_map, file = "dot_Lake_Hover.html")
