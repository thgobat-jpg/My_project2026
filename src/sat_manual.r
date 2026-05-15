################################################################################
# SIMPLE MANUAL WORKFLOW:
# EXPLORE MODIS PRODUCTS, DOWNLOAD NDVI MANUALLY, READ IT IN R,
# EXTRACT VALUES AT POINT LOCATIONS, AND ADD THEM TO THE DATA TABLE
################################################################################

# ==============================================================================
# 1. Load required packages
# ==============================================================================

#install.packages('luna', repos='https://rspatial.r-universe.dev')
#install.packages("remotes")
#remotes::install_github("ropensci/MODIStsp")
#install.packages("appeears")
#install.packages("plotly")
library(luna)
library(MODIStsp)

library(appeears)
library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
# ------------------------------------------------------------------------------
# Optional installation
#install.packages(c("appeears", "terra", "sf", "rnaturalearth", "ggplot2", "dplyr"))
# ------------------------------------------------------------------------------


# ==============================================================================
# 2. Explore available MODIS products
# ==============================================================================
# List all products available through AppEEARS
products <- rs_products()

# Display the first rows
head(products)


getProducts("^MOD|^MYD|^MCD")

#MOD = Terra satellite products

#MYD = Aqua satellite products

#MCD = Combined products (Terra + Aqua)

MODIStsp_get_prodlayers("M*D13Q1")

#product <- "MOD09A1" #surface spectral reflectance of Terra
product <- "MOD13Q1" # NDVI


# IMPORTANT:
# Look for the NDVI layer name in the printed list.
# This is the layer you will select manually in AppEEARS.
##Si tu veux mettre toute les couches NDVI et faire une moyenne pour éviter les sécheresses et les problèmes 
# ==============================================================================
# 4. Export the Switzerland polygon for manual upload in AppEEARS
# ==============================================================================
# This file can be uploaded directly in the AppEEARS web interface
# when creating an area request.

switzerland_sf <- ne_countries(
  scale = "medium",
  country = "Switzerland",
  returnclass = "sf"
)

dir.create(".data", showWarnings = FALSE)

st_write(
  switzerland_sf,
  "data/switzerland.geojson",
  delete_dsn = TRUE
)

plot(st_geometry(switzerland_sf), col = "lightgray", main = "Switzerland")

# ------------------------------------------------------------------------------
# MANUAL STEP IN APP EEARS
# ------------------------------------------------------------------------------
# 1. Open the AppEEARS website
# 2. Create an AREA request
# 3. Upload the file: .data/switzerland.geojson
# 4. Select product: MOD13Q1.061
# 5. Select layer: NDVI
# 6. Select the desired date range
# 7. Choose GeoTIFF as output format if available
# 8. Submit the task
# 9. Download the resulting NDVI raster manually
# 10. Save it in the folder: .data/appeears_manual_download
# ------------------------------------------------------------------------------

# ==============================================================================
# 5. Read the manually downloaded NDVI raster
# ==============================================================================

#Here only the data of the month July from the year 2017 to 2022 were downloaded. I only selected the month of July because i didn't think that having the NDVi data for the all month would make sense 
#and it would have been to much data to download. I also choose the years 2017 to 2022 because it was corresponding to the year of the data obtained for the elevation. 
manual_path <- "data/modis"

ndvi_files <- list.files(
  manual_path,
  pattern = "NDVI.*\\.tif$",
  full.names = TRUE
)

print(ndvi_files)
ndvi_stack <- rast(ndvi_files)

ndvi_mean <- mean(ndvi_stack, na.rm = TRUE)

windows()

plot(
  ndvi_mean,
  main = "Mean NDVI over the selected period (July 2017-2022)",
  xlab = "Longitude",
  ylab = "Latitude",
  col = hcl.colors(100, "YlGn"),
  axes = TRUE
)

# ==============================================================================
# 6. Clip the raster to the exact Switzerland border
# ==============================================================================
switzerland_vect <- vect(switzerland_sf)

# Reproject the Switzerland polygon to the raster CRS
switzerland_vect <- project(switzerland_vect, crs(ndvi_mean))

# Crop and mask
ndvi_switzerland <- crop(ndvi_mean, switzerland_vect)
ndvi_switzerland <- mask(ndvi_switzerland, switzerland_vect)

# Plot the clipped raster
windows()
plot(ndvi_switzerland, main = "NDVI raster clipped to Switzerland")
plot(switzerland_vect, add = TRUE, border = "black", lwd = 1)


# ==============================================================================
# 7. Convert the sampling table to spatial points
# ==============================================================================
# We assume your data frame is called matrix_full_eco
# and contains longitude and latitude columns.

Matrix_final = read.csv("data/matrix_full_clim_eco_elev.csv")
head(Matrix_final)

points_vect <- vect(
  Matrix_final,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Reproject the points to the raster CRS
points_vect <- project(points_vect, crs(ndvi_switzerland))

# Plot the points on top of the raster

windows()

plot(
  ndvi_switzerland,
  main = "Species occurrences over mean NDVI",
  xlab = "Longitude",
  ylab = "Latitude",
  col = hcl.colors(100, "YlGn")
)

plot(
  points_vect,
  add = TRUE,
  pch = 16,
  cex = 0.6,
  col = ifelse(Matrix_final$species == "Castor fiber", "#009ACD", "#CD2626")
)

par(xpd = TRUE)

legend(
  "topleft",
  legend = c("Castor fiber", "Libellula depressa"),
  col = c("#009ACD", "#CD2626"),
  pch = 16,
  pt.cex = 1.5,
  cex = 0.9,
  bg = "white",
  box.col = "black"
)
# ==============================================================================
# 8. Extract NDVI values at point locations
# ==============================================================================
ndvi_values <- terra::extract(ndvi_switzerland, points_vect)

# Check extracted values
head(ndvi_values)


# ==============================================================================
# 9. Add NDVI values to the original data frame
# ==============================================================================
# The first column returned by terra::extract() is usually the point ID
# and the second column contains the extracted raster value.
Matrix_final$NDVI <- ndvi_values[, 2]

# Check the updated table
head(Matrix_final)


# ==============================================================================
# 10. Plots
# ==============================================================================
summary(Matrix_final$NDVI)
###############################################################################################################
##NDVI with climate and weather condition
#Graphic showing the NDVI distribution per climate
p_ndvi_climate <- ggplot(Matrix_final, aes(x = NDVI, fill = Climate_Re)) +
  geom_density(alpha = 0.5, adjust = 3) +
  labs(title = "NDVI distribution by climate category",
       x = "Mean NDVI",
       y = "Density",
       fill = "Climate category") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

windows()
print(p_ndvi_climate)
Sys.sleep(7)

#Graphic showing the link between NDVI and temperatur
names(Matrix_final)
Matrix_final$temp_class <- cut(
  Matrix_final$t_mean_c,
  breaks = 12
)

ndvi_temp <- Matrix_final %>%
  group_by(temp_class) %>%
  summarise(
    mean_temp = mean(t_mean_c, na.rm = TRUE),
    mean_ndvi = mean(NDVI, na.rm = TRUE)
  )

p_ndvi_temp <- ggplot(ndvi_temp, aes(x = mean_temp, y = mean_ndvi)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Mean NDVI along the temperature gradient",
       x = "Mean temperature (°C)",
       y = "Mean NDVI") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

windows()
print(p_ndvi_temp)
Sys.sleep(7)

#Graphic showing the link between NDVI and precipitaion : 
Matrix_final$prec_class <- cut(Matrix_final$prec_mean_annual, breaks = 12)

ndvi_prec <- Matrix_final %>%
  group_by(prec_class) %>%
  summarise(mean_prec = mean(prec_mean_annual, na.rm = TRUE),
            mean_ndvi = mean(NDVI, na.rm = TRUE))

p_ndvi_prec <- ggplot(ndvi_prec, aes(x = mean_prec, y = mean_ndvi)) +
  geom_line(linewidth = 1.2, color = "steelblue4") +
  geom_point(size = 3, color = "steelblue4") +
  labs(title = "Mean NDVI along the precipitation gradient",
       x = "Mean annual precipitation",
       y = "Mean NDVI") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

windows()
print(p_ndvi_prec)
Sys.sleep(7)


##NDVI per species occurence
p_ndvi_species <- ggplot(Matrix_final, aes(x = species, y = NDVI, fill = species)) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(width = 0.08, size = 0.7, alpha = 0.2) +
  labs(title = "NDVI associated with species occurrences",
       x = "Species",
       y = "Mean NDVI") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

x11()
print(p_ndvi_species)
Sys.sleep(7)
#Same idea of the last graphic but this time on the switzerland map

  windows()

p_map_occ_ndvi <- ggplot() +
  geom_sf(data = switzerland_sf, fill = "grey95", color = "black") +
  geom_point(
    data = Matrix_final,
    aes(x = longitude, y = latitude, color = NDVI),
    size = 0.8,
    alpha = 0.8
  ) +
  facet_wrap(~ species) +
  scale_color_viridis_c(option = "C", limits = c(0, 1)) +
  labs(
    title = "NDVI values at species occurrence locations",
    subtitle = "Spatial comparison between Castor fiber and Libellula depressa",
    x = "Longitude",
    y = "Latitude",
    color = "NDVI"
  ) +
  theme_classic(base_size = 13)
Sys.sleep(7)
print(p_map_occ_ndvi)
###############################################################################################################

##Graph showing the species habitat preferences for the elevation and NDVI
p_ndvi_elev <- ggplot(Matrix_final, aes(x = NDVI, y = elevation, color = species)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.1) +
  facet_wrap(~ species) +
  labs(title = "Relationship between NDVI and elevation by species",
       x = "Mean NDVI",
       y = "Elevation (m)",
       color = "Species") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

windows()
print(p_ndvi_elev)
Sys.sleep(7)


##NDVI across climat categories 
p_ndvi_climate_species <- ggplot(Matrix_final,
                                 aes(x = Climate_Re, y = NDVI, fill = species)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
  labs(title = "NDVI across climate categories and species",
       x = "Climate category",
       y = "Mean NDVI",
       fill = "Species") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

windows()
print(p_ndvi_climate_species)
Sys.sleep(7)
###############################################################################################################
#Interactiv map (with a great help of Chatgpt)--> takes time to charge. 

ndvi_stack_switzerland <- crop(ndvi_stack, switzerland_vect)
ndvi_stack_switzerland <- mask(ndvi_stack_switzerland, switzerland_vect)

ndvi_stack_small <- aggregate(
  ndvi_stack_switzerland,
  fact = 5,
  fun = mean,
  na.rm = TRUE
)

file_names <- basename(ndvi_files)

years <- substr(gsub(".*doy", "", file_names), 1, 4)
doys  <- substr(gsub(".*doy", "", file_names), 5, 7)

dates <- as.Date(as.numeric(doys) - 1, origin = paste0(years, "-01-01"))

ndvi_df <- as.data.frame(ndvi_stack_small, xy = TRUE, na.rm = FALSE)

names(ndvi_df)[3:ncol(ndvi_df)] <- as.character(dates)

ndvi_long <- tidyr::pivot_longer(
  ndvi_df,
  cols = -c(x, y),
  names_to = "date",
  values_to = "NDVI"
)

ndvi_long <- ndvi_long[!is.na(ndvi_long$NDVI), ]

print(table(ndvi_long$date))

library(plotly)
library(dplyr)

# Make sure date is treated as text, not Date object
ndvi_long$date <- as.character(ndvi_long$date)

# Remove missing NDVI values
ndvi_long <- ndvi_long[!is.na(ndvi_long$NDVI), ]

# Check number of pixels per date
print(table(ndvi_long$date))

# Interactive animated map
p_ndvi_map <- plot_ly(
  data = ndvi_long,
  x = ~x,
  y = ~y,
  frame = ~date,
  type = "scatter",
  mode = "markers",
  marker = list(
    symbol = "square",
    size = 7,
    color = ~NDVI,
    colorscale = "Greens",
    cmin = 0,
    cmax = 1,
    colorbar = list(title = "NDVI")
  ),
  hoverinfo = "text",
  text = ~paste(
    "Date:", date,
    "<br>NDVI:", round(NDVI, 3),
    "<br>Longitude:", round(x, 3),
    "<br>Latitude:", round(y, 3)
  )
) %>%
  layout(
    title = "NDVI evolution in Switzerland",
    xaxis = list(
      title = "Longitude",
      range = c(5.8, 10.7)
    ),
    yaxis = list(
      title = "Latitude",
      range = c(45.6, 48.0),
      scaleanchor = "x"
    )
  ) %>%
  animation_opts(
    frame = 800,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Date: ")
  )

p_ndvi_map

write.csv(Matrix_final, "data/matrix.final.csv", row.names = FALSE)


