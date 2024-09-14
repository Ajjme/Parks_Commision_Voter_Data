#Filters
source("./02_Scripts/000_Init.R")
# upload combined file
first_mailer <- readRDS("first_mailer.rds")
library(tidygeocoder)
library(dplyr)


#####################
##### Moraga ##########
#####################

EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(str_detect(precinct_name, "Moraga"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
         party_category == "Democratic",
         voted_in_2020_general == "Yes") 


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
  arrange(mail_street, mail_zip) %>% 
  select(full_address,name_first,name_last,last_voted,precinct_name,voting_opportunities)

df_test <- head(df)
# Geocode the addresses
geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude) 

saveRDS(geocoded_data , file = "Moraga_dem_2020_unique.rds")

moraga_lat_min <- 37.81
moraga_lat_max <- 37.86
moraga_long_min <- -122.15
moraga_long_max <- -122.10

# Filter the data frame using dplyr
# Filter the data frame to only include rows within the Moraga boundaries
df <- geocoded_data
moraga_df <- df[df$latitude >= moraga_lat_min & df$latitude <= moraga_lat_max & 
                  df$longitude >= moraga_long_min & df$longitude <= moraga_long_max, ]

geocoded_data_filtered <- geocoded_data 

###-------------------
dot_map <- leaflet(data = moraga_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    radius = 2, 
    color = "blue", 
    fillOpacity = 0.6#,
   # clusterOptions = markerClusterOptions()
  )
dot_map <- leaflet(data = moraga_df) %>%
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
saveWidget(dot_map, file = "dot_Moraga_Hover.html")

write.csv(in_j_lat_long, "Moraga_addresses.csv")




#################################
############ Lafyette ###########
#################################

desired_levels <- c( "80-90%", "90-100%") #"70-80%", "80-90%",
EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(str_detect(precinct_name, "Lafayette"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
         party_category == "Democratic",
         voted_in_2020_general == "Yes",
         voted_in_2024_primary == "Yes",
         voted_vs_opportunities >= .6)


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


####################
#### walnut ########
####################

desired_levels <- c( "80-90%", "90-100%") #"70-80%", "80-90%",
EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(str_detect(precinct_name, "WalnutCreek"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
         party_category == "Democratic",
         voted_in_2020_general == "Yes",
         voted_in_2024_primary == "Yes",
         str_detect(most_recent_precinct, "SARA102|SARA104|SARA801|LFET108|WLCR129|WLCR130|WLCR127")
         )


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
  arrange(mail_street, mail_zip) %>% 
  select(full_address,name_first,name_last,last_voted,precinct_name,voting_opportunities)

df_test <- head(df)
# Geocode the addresses
geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude) 

saveRDS(geocoded_data , file = "wal_sara_2020_unique.rds")

walnut_creek_lat_min <- 37.88
walnut_creek_lat_max <- 37.95
walnut_creek_long_min <- -122.10
walnut_creek_long_max <- -122.03

# Filter the data frame using dplyr
df <- geocoded_data
walnut_creek_df <- df %>%
  filter(latitude >= walnut_creek_lat_min, latitude <= walnut_creek_lat_max,
         longitude >= walnut_creek_long_min, longitude <= walnut_creek_long_max)

# Filter the data frame using dplyr
# Filter the data frame to only include rows within the Moraga boundaries

###----------------

dot_map <- leaflet(data = walnut_creek_df) %>%
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
saveWidget(dot_map, file = "dot_walnut_creek_SARANAP_Hover.html")

write.csv(walnut_creek_df, "walnut_creek_SARANAP_addresses.csv")


###################

#Emeryville and berkely hills!

desired_levels <- c( "80-90%", "90-100%") #"70-80%", "80-90%",
EBRPD_district_2_voter_data <- readRDS("EBRPD_district_2_voter_data.rds")

EBRPD_district_2_voter_data_2 <- EBRPD_district_2_voter_data %>% 
  rename(party_category = Party_Category) %>% 
  mutate(voted_in_2016_general = case_when(x52_11_08_2016_2016_general_election_127_eligibility == "V" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "A" ~ "Yes",
                                           x52_11_08_2016_2016_general_election_127_eligibility == "N" ~ "No",
                                           TRUE ~ "not eligable")) %>% 
  filter(#str_detect(precinct_name, "WalnutCreek"), #saranapp (walnut Creek) (Lafayette) - Oakland Piedmont Onthclair
         str_detect(mail_city, "BER"),
                    #mail_city == "EMERYVILLE",
         party_category == "Democratic",
         voted_in_2020_general == "Yes",
         voted_in_2024_primary == "Yes"
         #str_detect(most_recent_precinct, "SARA102|SARA104|SARA801|LFET108|WLCR129|WLCR130|WLCR127")
  )


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
  arrange(mail_street, mail_zip) %>% 
  select(full_address,name_first,name_last,last_voted,precinct_name,voting_opportunities)

df_test <- head(df)
# Geocode the addresses
geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude) 

saveRDS(geocoded_data , file = "EMERYVILLE_2020_unique.rds")

emeryville_lat_min <- 37.82
emeryville_lat_max <- 37.85
emeryville_long_min <- -122.31
emeryville_long_max <- -122.27

# Filter the data frame using dplyr
df <- geocoded_data
emeryville_df <- df %>%
  filter(latitude >= emeryville_lat_min, latitude <= emeryville_lat_max,
         longitude >= emeryville_long_min, longitude <= emeryville_long_max)

# Filter the data frame using dplyr
# Filter the data frame to only include rows within the Moraga boundaries

###----------------

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


### Archive ----------------------
in_j_lat_long <- geocoded_data

test <- readRDS("002_dem_90_v_inperson_30.rds")
install.packages("ggplot2")
install.packages("sf")
library(ggplot2)
library(sf)

# Plot the points on a map
ggplot(data = geocoded_data ) +
  geom_point(aes(x = longitude, y = latitude), color = "red", size = 2) +
  borders("county") + # Add state borders
  theme_minimal() +
  coord_quickmap()+
  xlim(-123, -121) +  # Set x-axis limits to zoom in on the longitude range
  ylim(37, 38.5)      # Set y-axis limits to zoom in on the latitude range

# Convert your data frame to an sf object
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Plot the points
ggplot(data = df_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Residential Locations", x = "Longitude", y = "Latitude") +
  coord_sf()