#Interactive map of our voters colored by the chances that they vote

# 1 convert all addresses to Lat and Long
# use developed levels as color for the dots
#plot out where everyone is
# Install necessary packages if not already installed
# install.packages("sf")
# install.packages("ggplot2")

# Load the required libraries
library(sf)
library(ggplot2)
library(leaflet)

# Read the shapefile
shapefile_path <- "02_Scripts/CCC_Precincts072024.shp"  # Update with your actual file path
CCC_Precincts <- st_read(shapefile_path)

# Filter the shapefile for certain precincts (e.g., precinct_id is 101 or 102)
filtered_precincts <- CCC_Precincts %>% 
  filter(sDistrictID == "6000RP	2")

# Plot the shapefile using ggplot2
ggplot(data = CCC_Precincts) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Contra Costa County Precincts Map",
       x = "Longitude",
       y = "Latitude")

shapefile_path <- "02_Scripts/East_Bay_Regional_Park_District_Wards.shx"  # Update with your actual file path
Ala_Precincts <- st_read(shapefile_path)
shapeData <- spTransform(Ala_Precincts, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# Create a Leaflet map
leaflet_map <- leaflet() %>%
  addTiles() %>% 
  addPolygons(data = Ala_Precincts, weight = 3, color = "black")
# Display the map
leaflet_map

projection(Ala_Precincts)="+init=epsg:3091"

> bbox(spTransform(add,"+init=epsg:4326"))
min       max
x -89.57120 -81.96479
y  36.49706  39.14773