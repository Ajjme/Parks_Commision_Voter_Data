#Filters
source("./02_Scripts/000_Init.R")
library(tidygeocoder)
library(dplyr)


#####################
##### Moraga ##########
#####################
desired_levels <- c( "80-90%", "90-100%") #"70-80%", "80-90%",

EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_all <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(str_detect(mail_zip, "94618|94611|94610")#,#"94618|94610|94611"94618 first,  94611 second 94610 third
         # party_category == "Democratic",
         # voted_in_2020_general == "Yes",
         # voted_in_2016_general == "Yes",
         #voted_vs_opportunities_group %in% desired_levels
         ) %>%
  select( "voter_id"                     ,                                           
          "most_recent_precinct"         ,                                           
          "name_prefix"                  ,                                           
          "name_last"                    ,                                           
          "name_first"                   ,                                           
          "house_number"                 ,                                           
          "apartment_number"             ,                                           
          "phone_1"                      ,                                           
          "phone_2"                      ,                                         
          "party"                        ,                                           
          "precinct_name"                ,                                           
          "mail_street"                  ,                                           
          "mail_city"                    ,                                           
          "mail_state"                   ,                                           
          "mail_zip"                     ,                                           
          "mail_country"                 ,                                           
          "email"                        ,                                         
          "voted_in_2024_primary"        ,                                           
          "voted_in_2020_general"        ,                                           
          "percent_voted_by_mail_group"  ,                                           
          "voted_vs_opportunities_group" ,                                         
          "voted_in_2016_general"          )%>%
  mutate(mail_zip = substr(mail_zip, 1, 5))%>%
  #select(-full_address) %>% 
  #only for mapping
  mutate(mail_street = gsub("\\s+\\d+$", "", mail_street)) %>%
  mutate(full_address = paste( apartment_number, mail_street, mail_state, mail_zip, sep = ", "))  %>%
  mutate(full_address = str_remove(full_address, "NA, |, NA|NA")) %>%  
  # filter(!str_detect(mail_street, "PO BOX"),
  #        mail_state == "CA" ) %>%
  # distinct(full_address, .keep_all = TRUE) %>% 
  arrange(mail_street, mail_zip)


write.csv(EBRPD_district_2_voter_data_all,"zips_9-21.csv")

summary_table <- EBRPD_district_2_voter_data_all %>%
  group_by(mail_zip) %>%
  summarise(
    num_voters = n(),
    num_phones = sum(!is.na(phone_1) | !is.na(phone_2))
  ) %>%
  arrange(desc(num_phones))

# Save the summary table to a CSV
write.csv(summary_table, "summary_table_94618.csv")

######################
####### Map ##########
######################
df <- EBRPD_district_2_voter_data_2 %>%
  mutate(mail_zip = substr(mail_zip, 1, 5))%>%
  #select(-full_address) %>% 
  #only for mapping
  mutate(mail_street = gsub("\\s+\\d+$", "", mail_street)) %>%
  mutate(full_address = paste(house_number, mail_street, mail_state, mail_zip, sep = ", ")) %>% 
  filter(!str_detect(mail_street, "PO BOX"), 
         mail_state == "CA" ) %>% 
  distinct(full_address, .keep_all = TRUE) %>% 
  arrange(mail_street, mail_zip) %>% 
  select(full_address,name_first,name_last,last_voted,precinct_name,voting_opportunities)

df_test <- head(df)
# Geocode the addresses
geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude) 

saveRDS(geocoded_data , file = "Laf_dem_2020_.6_unique.rds")

lafayette_lat_min <- 37.86
lafayette_lat_max <- 37.92
lafayette_long_min <- -122.16
lafayette_long_max <- -122.10

# Filter the data frame using dplyr
df <- geocoded_data
lafayette_df <- df %>%
  filter(latitude >= lafayette_lat_min, latitude <= lafayette_lat_max,
         longitude >= lafayette_long_min, longitude <= lafayette_long_max)

# Filter the data frame using dplyr
# Filter the data frame to only include rows within the Moraga boundaries

###----------------

dot_map <- leaflet(data = lafayette_df) %>%
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
# Save the map to an HTML file
library(htmlwidgets)
saveWidget(dot_map, file = "dot_Laf_Hover.html")

write.csv(lafayette_df, "Laf_addresses.csv")
