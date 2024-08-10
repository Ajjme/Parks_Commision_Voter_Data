#Filters
source("./02_Scripts/000_Init.R")
# upload combined file
first_mailer <- readRDS("first_mailer.rds")
library(tidygeocoder)
library(dplyr)

# Assuming your data frame is named 'df' with columns: 'Street', 'City', 'State', 'Zip'
# Create a full address column
df <- first_mailer %>%
  mutate(mail_zip = substr(mail_zip, 1, 5))%>%
  select(-full_address) %>% 
  #only for mapping
  mutate(mail_street = gsub("\\s+\\d+$", "", mail_street)) %>%
  mutate(full_address = paste(mail_street, mail_state, mail_zip, sep = ", ")) %>% 
  filter(!str_detect(mail_street, "PO BOX"), 
           mail_state == "CA" )

df_test <- head(df)
# Geocode the addresses
geocoded_data <- df %>%
  geocode(address = full_address, method = 'osm', lat = latitude, long = longitude)

saveRDS(geocoded_data , file = "003_dem_90_v_inperson_30.rds")

in_j_lat_long <- geocoded_data %>% 
  filter(longitude > -123 & longitude < -122 & latitude > 37 & latitude < 38.5)

test <- readRDS("002_dem_90_v_inperson_30.rds")
install.packages("ggplot2")
install.packages("sf")
library(ggplot2)
library(sf)

# Plot the points on a map
ggplot(data = in_j_lat_long ) +
  geom_point(aes(x = longitude, y = latitude), color = "red", size = 2) +
  borders("county") + # Add state borders
  theme_minimal() +
  coord_quickmap()+
  xlim(-123, -122) +  # Set x-axis limits to zoom in on the longitude range
  ylim(37, 38.5)      # Set y-axis limits to zoom in on the latitude range

###-------------------
dot_map <- leaflet(data = in_j_lat_long) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    radius = 2, 
    color = "blue", 
    fillOpacity = 0.6#,
   # clusterOptions = markerClusterOptions()
  )

# Save the map to an HTML file
library(htmlwidgets)
saveWidget(dot_map, file = "dot_leaflet_map.html")
### ----------------------
# Convert your data frame to an sf object
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Plot the points
ggplot(data = df_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Residential Locations", x = "Longitude", y = "Latitude") +
  coord_sf()