###############################################################################
# ADDING ECOSYSTEM DATA TO SPECIES OCCURRENCE COORDINATES
# Example workflow:
# - read an ecosystem raster
# - crop it to Switzerland
# - extract ecosystem values at species occurrence points
# - join the raster values with metadata
# - visualize the result
###############################################################################

#------------------------------------------------------------------------------
# 1) LOAD REQUIRED PACKAGES
#------------------------------------------------------------------------------

# raster: to read and manipulate raster files
# sf: to handle vector spatial data
# rnaturalearth: to download country boundaries
# ggplot2: to create graphs
install.packages(c(
  "rgbif",
  "rinat",
  "sf",
  "sp",
  "raster",
  "terra",
  "ggplot2",
  "dplyr",
  "rnaturalearth",
  "elevatr",
  "rayshader"
))
library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)

#------------------------------------------------------------------------------
# 2) LOAD THE ECOSYSTEM RASTER
#------------------------------------------------------------------------------

# Define the path to the GeoTIFF file
file_path <- "./data/WorldEcosystem.tif"

# Read the raster layer
# This raster contains ecosystem categories coded as numeric values
ecosystem_raster <- raster(file_path)

# Display basic information about the raster
print(ecosystem_raster)

# Optional: plot the full raster
#plot(ecosystem_raster, main = "Original Ecosystem Raster")

#------------------------------------------------------------------------------
# 3) LOAD THE BOUNDARY OF SWITZERLAND
#------------------------------------------------------------------------------

# Download the country boundary as an sf object
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)

# Plot the country boundary
plot(st_geometry(Switzerland), main = "Boundary of Switzerland")

#------------------------------------------------------------------------------
# 4) CROP AND MASK THE RASTER TO SWITZERLAND
#------------------------------------------------------------------------------

# crop() keeps only the rectangular extent around Switzerland
r2 <- crop(ecosystem_raster, extent(Switzerland))

# mask() keeps only the pixels that fall inside the country boundary
ecosystem_switzerland <- mask(r2, Switzerland)

# Plot the cropped and masked raster
plot(ecosystem_switzerland, main = "Ecosystem Raster Restricted to Switzerland")

#------------------------------------------------------------------------------
# 5) CONVERT SPECIES COORDINATES INTO SPATIAL POINTS
#------------------------------------------------------------------------------

# We assume that matrix_full is a data frame containing at least:
# - longitude
# - latitude
# - species

getwd()
Complete_matrix <- read.csv("data/matrix_mix.csv")
Complete_matrix

# Example structure:
#head(matrix_full)

# Convert the coordinate columns into spatial points
# The CRS used here is WGS84, which is the standard geographic coordinate system
spatial_points <- SpatialPoints(
  coords = Complete_matrix[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Add the occurrence points on top of the ecosystem map
plot(ecosystem_switzerland, main = "Species Occurrences on Ecosystem Map")
plot(spatial_points, add = TRUE, pch = 16, cex = 1.2)

#------------------------------------------------------------------------------
# 6) EXTRACT ECOSYSTEM VALUES AT EACH OCCURRENCE POINT
#------------------------------------------------------------------------------

# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values <- raster::extract(ecosystem_switzerland, spatial_points)

# Check the extracted values
head(eco_values)

#------------------------------------------------------------------------------
# 7) ADD THE EXTRACTED ECOSYSTEM VALUES TO THE ORIGINAL DATA FRAME
#------------------------------------------------------------------------------

# Create a new data frame by adding the extracted ecosystem values
matrix_full_eco <- data.frame(Complete_matrix, eco_values)

# Inspect the result


#------------------------------------------------------------------------------
# 8) LOAD THE ECOSYSTEM METADATA TABLE
#------------------------------------------------------------------------------

# This metadata table links the numeric raster code to descriptive ecosystem names
metadata_eco <- read.delim("./data/WorldEcosystem.metadata.tsv")

# Inspect the metadata table
head(metadata_eco)

#------------------------------------------------------------------------------
# 9) MERGE THE EXTRACTED VALUES WITH THE METADATA
#------------------------------------------------------------------------------

# Merge the occurrence table with the metadata table
# by.x = "eco_values" means the ecosystem code in our occurrence table
# by.y = "Value" means the corresponding code column in the metadata table
matrix_full_eco <- merge(
  matrix_full_eco,
  metadata_eco,
  by.x = "eco_values",
  by.y = "Value"
)

# Inspect the enriched table
head(matrix_full_eco)

#------------------------------------------------------------------------------
# 10) VISUALIZE THE NUMBER OF OBSERVATIONS PER CLIMATE CATEGORY AND SPECIES
#------------------------------------------------------------------------------

# Create a bar plot showing how many observations of each species
# are found in each climate category
p2 <- ggplot(matrix_full_eco, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Count of Observations of Each Species by Climate",
    x = "Climate category",
    y = "Number of observations"
  ) +
  theme_minimal()

# Display the plot
x11()
print(p2)

write.csv(matrix_full_eco, "data/matrix_full_eco", row.names = FALSE)