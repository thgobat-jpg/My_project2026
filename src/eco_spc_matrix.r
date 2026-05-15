
##The goal of this code is to add the ecosystem data to the occurences coorindates 

library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)

#------------------------------------------------------------------------------
# 2) LOAD THE ECOSYSTEM RASTER
#------------------------------------------------------------------------------

file_path <- "./data/WorldEcosystem.tif"

# Read the raster layer
# This raster contains ecosystem categories coded as numeric values
ecosystem_raster <- raster(file_path)

# Display basic information about the raster
print(ecosystem_raster)

# PLOT OF THE FULL RASTER 
x11()
plot(
  ecosystem_raster,
  main = "Global ecosystem raster",
  xlab = "Longitude",
  ylab = "Latitude",
  col = terrain.colors(50),
  axes = TRUE
)
#------------------------------------------------------------------------------
# 3) LOAD THE BOUNDARY OF SWITZERLAND
#------------------------------------------------------------------------------

# Download the country boundary as an sf object
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)
Sys.sleep(7)
# Plot the country boundary
plot(st_geometry(Switzerland), main = "Boundary of Switzerland")
Sys.sleep(7)

#------------------------------------------------------------------------------
# 4) CROP AND MASK THE RASTER TO SWITZERLAND
#------------------------------------------------------------------------------

# crop() keeps only the rectangular extent around Switzerland
r2 <- crop(ecosystem_raster, extent(Switzerland))

# mask() keeps only the pixels that fall inside the country boundary
ecosystem_switzerland <- mask(r2, Switzerland)

# Plot the cropped and masked raster
plot(ecosystem_switzerland, 
main = "Ecosystem Raster Restricted to Switzerland",
xlab="Longitude",
ylab="Latitude",
axes=TRUE)
Sys.sleep(7)
#------------------------------------------------------------------------------
# 5) CONVERT SPECIES COORDINATES INTO SPATIAL POINTS
#------------------------------------------------------------------------------



getwd()
Complete_matrix <- read.csv("data/matrix_mix.csv")
head(Complete_matrix)

# Convert the coordinate columns into spatial points
# The CRS used here is WGS84, which is the standard geographic coordinate system
spatial_points <- SpatialPoints(
  coords = Complete_matrix[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

# Add the occurrence points on top of the ecosystem map

x11()
plot(ecosystem_switzerland,
     main = "Species Occurrences on Ecosystem Map",
     xlab = "Longitude",
     ylab = "Latitude")

plot(
  spatial_points,
  add = TRUE,
  pch = 16,
  cex = 0.5,
  col = ifelse(
    Complete_matrix$species == "Castor fiber",
    "#009ACD",
    "#CD2626"
  )
)
legend(
  "topright",
  legend = c("Castor fiber", "Libellula depressa"),
  col = c("#009ACD", "#CD2626"),
  pch = 16
)
Sys.sleep(7)
#------------------------------------------------------------------------------
# 6) EXTRACT ECOSYSTEM VALUES AT EACH OCCURRENCE POINT
#------------------------------------------------------------------------------

# extract() retrieves the raster value at the location of each point
# Each point receives the ecosystem code of the raster cell where it falls
eco_values <- raster::extract(ecosystem_switzerland, spatial_points)

# Check the extracted values
#Modification of the axes cause x axe wasn't long enough. 
head(eco_values)
x11()
hist(eco_values,
     breaks = 20,
     col = "lightblue",
     border = "white",
     main = "Distribution of Ecosystem Codes",
     xlab = "Ecosystem code",
     ylab = "Frequency", 
     xlim = c(0, max(eco_values, na.rm = TRUE) + 10),
  axes = FALSE)
  axis(1, at = seq(0, 200, by = 25))
axis(2, las = 1)
box()
Sys.sleep(7)
#------------------------------------------------------------------------------
# 7) ADD THE EXTRACTED ECOSYSTEM VALUES TO THE ORIGINAL DATA FRAME
#------------------------------------------------------------------------------

# Create a new data frame by adding the extracted ecosystem values
matrix_full_eco <- data.frame(Complete_matrix, eco_values)
head(matrix_full_eco)
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
Sys.sleep(7)

top_eco <- matrix_full_eco %>%
  count(W_Ecosystm, species) %>%
  group_by(W_Ecosystm) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(total)) %>%
  slice_head(n = 20)

#Distribution of occurences per ecosystem type
p_top_eco <- ggplot(top_eco,
                    aes(x = reorder(W_Ecosystm, total),
                        y = n,
                        fill = species)) +
  geom_col(position = "dodge", color = "black", alpha = 0.8) +
  coord_flip() +
  labs(title = "Distribution of occurences per ecosystem type",
       x = "Ecosystem type",
       y = "Number of occurrences",
       fill = "Species") +
  theme_classic(base_size = 13)+
    theme(
    plot.title = element_text(hjust = 0.5)
  )

x11()
print(p_top_eco)
Sys.sleep(7)
