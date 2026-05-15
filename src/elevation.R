################################################################################
# EXTRACTING ELEVATION DATA IN SWITZERLAND AND VISUALIZATION
################################################################################

# =========================
# 1. Load required packages
# =========================
library(sf)        # modern spatial data handling (simple features)
library(elevatr)   # download elevation data
library(raster)    # raster data manipulation (maps)
library(ggplot2)   # data visualization

# Disable s2 geometry engine (can avoid issues in some spatial operations)
sf_use_s2(FALSE)


# =========================
# 2. Load Switzerland boundaries
# =========================
# Retrieve country borders from Natural Earth
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)


# =========================
# 3. Download elevation data
# =========================
# z controls resolution (higher = more detail but slower)
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)

# Quick visualization of the elevation raster
x11()
plot(
  elevation_switzerland,
  main = "Elevation map of Switzerland",
  xlab = "Longitude",
  ylab = "Latitude",
  col = terrain.colors(50),
  axes = TRUE
)
Sys.sleep(7)
# 3B. TOPOGRAPHIC VARIABLES
###############################################################################

# Slope (in degrees)
slope <- terrain(
  elevation_switzerland,
  opt = "slope",
  unit = "degrees"
)

# Aspect (orientation of the slope)
aspect <- terrain(
  elevation_switzerland,
  opt = "aspect",
  unit = "degrees"
)

x11()
plot(slope,main = "Slope of Switzerland",
xlab = "Longitude",
ylab = "Latitude",
col = terrain.colors(50),
axes = TRUE
)
Sys.sleep(7)

x11()
plot(
  aspect,
  main = "Aspect of Switzerland",
  xlab = "Longitude",
  ylab = "Latitude",
  col = hcl.colors(50, "Spectral"),
  axes = TRUE
)
Sys.sleep(7)

# =========================
# 4. Prepare sampling points
# =========================
# We assume your dataset contains:
# - longitude
# - latitude

# Convert coordinates into a spatial object (SpatialPoints format)

matrix_1 <- read.csv("data/matrix_afterclimate.csv")
head(matrix_1)

spatial_points <- SpatialPoints(
  coords = matrix_1[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)


# =========================
# 5. Extract elevation values
# =========================
# Extract raster values at each point location
elevation <- raster::extract(elevation_switzerland, spatial_points)

###############################################################################
# 5B. Extract topographic variables
###############################################################################

slope_values <- raster::extract(
  slope,
  spatial_points
)

aspect_values <- raster::extract(
  aspect,
  spatial_points
)

# =========================
# 6. Add elevation to the dataset
# =========================
matrix_full_eco_elev <- data.frame(
  matrix_1,

  elevation = elevation,

  slope_deg = slope_values,

  aspect_deg = aspect_values
)

#Categorisation of aspect and add to the dataset
matrix_full_eco_elev$aspect_class <- cut(
  matrix_full_eco_elev$aspect_deg,
  breaks = c(0, 45, 135, 225, 315, 360),
  labels = c("North", "East", "South", "West", "North"),
  include.lowest = TRUE
)

# =========================
# 7. Visualization: elevation distribution
# =========================
# Compare elevation distributions between species 


p_elevation <- ggplot(matrix_full_eco_elev,
                      aes(x = species, y = elevation, fill = species)) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(width = 0.08, size = 0.8, alpha = 0.25) +
  labs(title = "Elevation distribution of species occurrences",
       x = "Species",
       y = "Elevation (m)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

x11()
print(p_elevation)
Sys.sleep(7)

#Visualisation of the density occurence across temperature and elevation gradient 


p_density=ggplot(matrix_full_eco_elev,
       aes(x = t_mean_c,
           y = elevation)) +

  geom_density_2d(color = "darkorange2") +

  facet_wrap(~ species) +

  labs(
    title = "Density of occurrences across temperature and elevation gradients",
    x = "Mean temperature (°C)",
    y = "Elevation (m)"
  ) +

  theme_classic()

  x11()
print(p_density)


#Slope distribution per species 
p_slope <- ggplot(matrix_full_eco_elev,
                  aes(x = species, y = slope_deg, fill = species)) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(width = 0.08, size = 0.8, alpha = 0.25) +
  labs(title = "Slope distribution of species occurrences",
       x = "Species",
       y = "Slope (degrees)") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

x11()
print(p_slope)
Sys.sleep(7)

#Slope x elevation per species 
p_elev_slope <- ggplot(matrix_full_eco_elev,
                       aes(x = elevation, y = slope_deg, color = species)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_density_2d(linewidth = 0.8) +
  labs(title = "Topographic space occupied by species",
       x = "Elevation (m)",
       y = "Slope (degrees)",
       color = "Species") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

x11()
print(p_elev_slope)
Sys.sleep(7)

## Occurence of species per slope orientation
p_aspect_class <- ggplot(matrix_full_eco_elev,
                         aes(x = aspect_class,
                             fill = species)) +

  geom_bar(position = "dodge",
           color = "black",
           alpha = 0.8) +

  labs(
    title = "Slope orientation associated with species occurrences",
    x = "Slope orientation",
    y = "Number of occurrences",
    fill = "Species"
  ) +

  theme_classic(base_size = 13) +

  theme(
    plot.title = element_text(hjust = 0.5)
  )

x11()
print(p_aspect_class)
Sys.sleep(7)
##
write.csv(matrix_full_eco_elev, "data/matrix_full_clim_eco_elev.csv", row.names = FALSE)