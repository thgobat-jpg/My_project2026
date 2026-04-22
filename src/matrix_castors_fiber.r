##But de ce fichier est de créer la matrice avec toute les données sur les espèces : 
###############################################################################
# FULLY COMMENTED SCRIPT
# COMBINING GBIF + iNaturalist OCCURRENCES
# WITH DATE FILTERING AND A MAP OF SWITZERLAND
###############################################################################

# =========================
# 1) PACKAGES
# =========================

# Install if necessary:
# install.packages(c("rgbif", "rnaturalearth", "ggplot2", "rinat",
#                    "raster", "dplyr", "sf"))
install.packages("rgbif")
library(rgbif)         # access to GBIF data
library(rnaturalearth) # country maps
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(dplyr)         # table manipulation
library(sf)            # modern spatial objects

# Disable spherical geometry for simpler spatial operations
sf_use_s2(FALSE)

###############################################################################
# 2) USER PARAMETERS
###############################################################################

# Species of interest
myspecies <- "Castor fiber"

# Maximum number of GBIF records to download
#gbif_limit <- 5000

# Time filtering period
date_start <- as.Date("2000-01-01")
date_end   <- as.Date("2026-12-31")

# Simplified geographic extent for Switzerland
xmin <- 6
xmax <- 11
ymin <- 46
ymax <- 48

###############################################################################
# 3) BASE MAP: SWITZERLAND
###############################################################################

# Download the outline of Switzerland
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)

# Simple visualization of the map
x11()
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  theme_classic()

###############################################################################
# 4) DOWNLOAD GBIF DATA
###############################################################################

# Download occurrences with coordinates

gbif_raw <- occ_data(
  scientificName = myspecies,
  country = "CH",
  hasCoordinate = TRUE,
  eventDate = "2000-01-01,2026-12-31",
  limit = 5000
)
# Extract the main data table
gbif_occ <- gbif_raw$data

# Quick inspection
head(gbif_occ)
names(gbif_occ)

# Select occurrences located in Switzerland
gbif_switzerland <- gbif_occ %>%
  filter(country == "Switzerland")

# Check number of records
nrow(gbif_switzerland)

# Quick base plot for checking
x11()
plot(
  gbif_switzerland$decimalLongitude,
  gbif_switzerland$decimalLatitude,
  pch = 16,
  col = "darkgreen",
  xlab = "Longitude",
  ylab = "Latitude",
  main = "GBIF occurrences in Switzerland"
)

# Map showing GBIF occurrences only
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_switzerland,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  theme_classic()

###############################################################################
# 5) FORMAT GBIF DATA
###############################################################################

# Keep only the useful columns
# eventDate may contain date + time; as.Date() keeps only the date
data_gbif <- data.frame(
  species   = gbif_switzerland$species,
  latitude  = gbif_switzerland$decimalLatitude,
  longitude = gbif_switzerland$decimalLongitude,
  date_obs  = as.Date(gbif_switzerland$eventDate),
  source    = "gbif"
)
View(data_gbif)

# Check structure
head(data_gbif)
str(data_gbif)

###############################################################################
# 6) DOWNLOAD iNaturalist DATA
###############################################################################

# Query iNaturalist for the same species in Switzerland
# place_id = "switzerland" usually works with rinat
inat_raw <- get_inat_obs(
  query = myspecies,
  place_id = "switzerland"
)

# Inspect the structure
head(inat_raw)
names(inat_raw)

# Map showing iNaturalist occurrences only
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  theme_classic()

###############################################################################
# 7) FORMAT iNaturalist DATA
###############################################################################

# In most rinat versions the observation date is stored in observed_on
# Convert it to Date format
data_inat <- data.frame(
  species   = inat_raw$scientific_name,
  latitude  = inat_raw$latitude,
  longitude = inat_raw$longitude,
  date_obs  = as.Date(inat_raw$observed_on),
  source    = "inat"
)
View(data_inat)

# Check structure
head(data_inat)
str(data_inat)

###############################################################################
# 8) MERGE THE TWO DATABASES
###############################################################################

# IMPORTANT:
# Here we want to STACK GBIF and iNaturalist observations.
# Therefore we use bind_rows() instead of merge().
matrix_full <- bind_rows(data_gbif, data_inat)

# Check results
head(matrix_full)
table(matrix_full$source, useNA = "ifany")
summary(matrix_full$date_obs)

###############################################################################
# 9) TIME FILTERING BETWEEN TWO DATES
###############################################################################

# Keep only observations within the selected time interval
matrix_full_date <- matrix_full %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
head(matrix_full_date)
summary(matrix_full_date$date_obs)
table(matrix_full_date$source)

###############################################################################
# 10) MAP OF COMBINED DATA
###############################################################################

ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_date,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  theme_classic()

###############################################################################
# 11) DEFINE A SIMPLE SPATIAL EXTENT
###############################################################################

################################################################################
##### Crop the background using coordinates

library(sf)

sf_use_s2(FALSE)

# Define the spatial extent
extent(Switzerland)
ext_Switzerland_cut <- as(raster::extent(6, 11, 46, 48), "SpatialPolygons")

# Crop Switzerland map to the defined extent
Switzerland_crop <- st_crop(Switzerland, ext_Switzerland_cut)

# Plot cropped map with occurrence points
ggplot(data = Switzerland_crop) +
  geom_sf() +
  geom_point(
    data = matrix_full,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  theme_classic()

################################################################################
################################################################################
##### Exclude points outside the specified spatial extent

# Convert occurrences to sf object
data_gbif_sf <- st_as_sf(matrix_full, coords = c("longitude", "latitude"), crs = 4326)

# Convert cropped Switzerland polygon to sf
Switzerland_crop_sf <- st_as_sf(Switzerland_crop)

# Identify points located inside the spatial extent
cur_data <- matrix_full[as.matrix(st_intersects(data_gbif_sf, Switzerland_crop_sf)),]

# Plot cropped Switzerland map with filtered points
x11()
ggplot(data = Switzerland_crop) +
  geom_sf() +
  geom_point(
    data = cur_data,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  theme_classic()

###############################################################################
# 14) OPTIONAL SAVE OF THE FINAL TABLE
###############################################################################

# Save filtered occurrence table
write.csv(
  cur_data,
  file = "Castor fiber.csv",
  row.names = FALSE
)