###############################################################################
# MACHINE LEARNING ANALYSIS
# Goal: predict Libellula depressa presence using environmental variables
# and distance to nearest Castor fiber occurrence.
###############################################################################
###############################################################################
# MACHINE LEARNING ANALYSIS
###############################################################################

# The goal of this analysis is to test whether the presence of
# Libellula depressa can be predicted using environmental variables
# and the distance to the nearest Castor fiber occurrence.
#
# Because GBIF and iNaturalist only provide presence data,
# true absence data are not available.
#
# Therefore, random background points are generated across Switzerland.
# These points do not represent true absences,
# but random available locations within the study area.
#
# The model then compares:
# - real Libellula depressa occurrences
# - and random background points
#
# using environmental variables and distance to Castor fiber.
#
# Finally, a Random Forest model is used to identify
# which variables are the most important
# for predicting Libellula depressa presence.
###############################################################################

# =========================
# 1) PACKAGES
# =========================

library(sf)
library(raster)
library(terra)
library(elevatr)
library(Rchelsa)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(rnaturalearth)
library(viridis)

sf_use_s2(FALSE)


###############################################################################
# 2) LOAD FINAL MATRIX
###############################################################################

# This matrix contains the real occurrence points and the environmental variables
# already extracted during the first part of the project.

matrix_final <- read.csv("data/matrix.final.csv")

head(matrix_final)
names(matrix_final)
table(matrix_final$species)


###############################################################################
# 3) SPLIT CASTOR AND LIBELLULA OCCURRENCES
###############################################################################

# Castor points are used to calculate the distance to nearest beaver occurrence.
# Libellula points are used as presence points for the machine learning model.

castor_points <- filter(matrix_final, species == "Castor fiber")
libellula_points <- filter(matrix_final, species == "Libellula depressa")

nrow(castor_points)
nrow(libellula_points)


###############################################################################
# 4) LOAD SWITZERLAND MAP
###############################################################################

# This polygon is used to create random background points inside Switzerland.

Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)

p_switzerland <- ggplot() +
  geom_sf(data = Switzerland, fill = "grey95", color = "black") +
  labs(title = "Switzerland polygon",
       x = "Longitude",
       y = "Latitude") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_switzerland)
Sys.sleep(7)


###############################################################################
# 5) CREATE RANDOM BACKGROUND POINTS
###############################################################################
#Besoin de plus d'explication de pouruqoi 
# GBIF and iNaturalist provide presence data only.
# Therefore, we create random background points.
# These points are not true absences, but they represent available space.

set.seed(123)

pseudo_geom <- st_sample(
  Switzerland,
  size = nrow(libellula_points),
  type = "random"
)

pseudo_sf <- st_sf(geometry = pseudo_geom)
pseudo_sf <- st_set_crs(pseudo_sf, 4326)

pseudo_coord <- st_coordinates(pseudo_sf)

pseudo_points <- data.frame(
  longitude = pseudo_coord[, 1],
  latitude = pseudo_coord[, 2]
)

pseudo_points$presence_class <- "background"

head(pseudo_points)


###############################################################################
# 6) VISUAL CONTROL OF PRESENCE AND BACKGROUND POINTS
###############################################################################

p_background_map <- ggplot() +
  geom_sf(data = Switzerland, fill = "grey95", color = "black") +
  geom_point(data = pseudo_points,
             aes(x = longitude, y = latitude, color = "Random points"),
             size = 0.8, alpha = 0.5) +
  geom_point(data = libellula_points,
             aes(x = longitude, y = latitude, color = "Libellula"),
             size = 0.8, alpha = 0.7) +
  scale_color_manual(values = c("Random points" = "grey50",
                                "Libellula" = "#CD2626")) +
  labs(title = "Libellula presences and random background points",
       x = "Longitude",
       y = "Latitude",
       color = "Point type") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_background_map)
Sys.sleep(7)


###############################################################################
# 7) DISTANCE TO NEAREST CASTOR FOR LIBELLULA AND BACKGROUND POINTS
###############################################################################

# Distances must be calculated in meters.
# Therefore, all points are projected to the Swiss coordinate system EPSG:2056.

castor_sf <- st_as_sf(
  castor_points,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

libellula_sf <- st_as_sf(
  libellula_points,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

pseudo_sf <- st_as_sf(
  pseudo_points,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

castor_sf_2056 <- st_transform(castor_sf, 2056)
libellula_sf_2056 <- st_transform(libellula_sf, 2056)
pseudo_sf_2056 <- st_transform(pseudo_sf, 2056)

nearest_castor_lib <- st_nearest_feature(libellula_sf_2056, castor_sf_2056)
nearest_castor_pseudo <- st_nearest_feature(pseudo_sf_2056, castor_sf_2056)

nearest_castor_lib_sf <- castor_sf_2056[nearest_castor_lib, ]
nearest_castor_pseudo_sf <- castor_sf_2056[nearest_castor_pseudo, ]

distance_lib <- st_distance(
  libellula_sf_2056,
  nearest_castor_lib_sf,
  by_element = TRUE
)

distance_pseudo <- st_distance(
  pseudo_sf_2056,
  nearest_castor_pseudo_sf,
  by_element = TRUE
)

libellula_points$distance_to_nearest_castor_m <- as.numeric(distance_lib)
pseudo_points$distance_to_nearest_castor_m <- as.numeric(distance_pseudo)

libellula_points$distance_to_nearest_castor_km <- libellula_points$distance_to_nearest_castor_m / 1000
pseudo_points$distance_to_nearest_castor_km <- pseudo_points$distance_to_nearest_castor_m / 1000

summary(libellula_points$distance_to_nearest_castor_km)
summary(pseudo_points$distance_to_nearest_castor_km)


###############################################################################
# 8) VISUAL CONTROL OF DISTANCE TO CASTOR
###############################################################################

distance_control <- data.frame(
  presence_class = c(rep("libellula", nrow(libellula_points)),
                     rep("background", nrow(pseudo_points))),
  distance_to_nearest_castor_km = c(libellula_points$distance_to_nearest_castor_km,
                                    pseudo_points$distance_to_nearest_castor_km)
)

p_distance_control <- ggplot(
  distance_control,
  aes(x = distance_to_nearest_castor_km,
      fill = presence_class)
) +
  geom_density(alpha = 0.45) +
  labs(title = "Distance to nearest Castor fiber occurrence",
       x = "Distance to nearest castor (km)",
       y = "Density",
       fill = "Point type") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_distance_control)
Sys.sleep(7)


###############################################################################
# 9) EXTRACT ELEVATION FOR BACKGROUND POINTS
###############################################################################

# We download the elevation raster again and extract elevation
# at the background point locations.

elevation_switzerland <- get_elev_raster(Switzerland, z = 8)

spatial_points_pseudo <- SpatialPoints(
  coords = pseudo_points[, c("longitude", "latitude")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

pseudo_elevation <- raster::extract(
  elevation_switzerland,
  spatial_points_pseudo
)

pseudo_points$elevation <- pseudo_elevation

elev_control <- rbind(
  data.frame(elevation = libellula_points$elevation,
             point_type = "Libellula"),
  data.frame(elevation = pseudo_points$elevation,
             point_type = "Random points")
)

p_elev_control <- ggplot(elev_control,
                         aes(x = elevation,
                             fill = point_type)) +
  geom_histogram(position = "identity",
                 bins = 30,
                 color = "black",
                 alpha = 0.5) +
  scale_fill_manual(values = c("Libellula" = "#CD2626",
                               "Random points" = "grey50")) +
  labs(title = "Elevation control",
       x = "Elevation (m)",
       y = "Number of points",
       fill = "Point type") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_elev_control)
Sys.sleep(7)


###############################################################################
# 10) EXTRACT ECOSYSTEM CODE FOR BACKGROUND POINTS
###############################################################################

ecosystem_raster <- raster("data/WorldEcosystem.tif")

Switzerland_sp <- as(Switzerland, "Spatial")

ecosystem_crop <- crop(
  ecosystem_raster,
  extent(Switzerland_sp)
)

ecosystem_switzerland <- mask(
  ecosystem_crop,
  Switzerland_sp
)

pseudo_eco_values <- raster::extract(
  ecosystem_switzerland,
  spatial_points_pseudo
)

pseudo_points$eco_values <- pseudo_eco_values

eco_control <- rbind(
  data.frame(eco_values = libellula_points$eco_values,
             point_type = "Libellula"),
  data.frame(eco_values = pseudo_points$eco_values,
             point_type = "Random points")
)

p_eco_control <- ggplot(
  eco_control,
  aes(x = factor(eco_values),
      fill = point_type)
) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Libellula" = "#CD2626",
                               "Random points" = "grey50")) +
  labs(title = "Ecosystem codes of points",
       x = "Ecosystem code",
       y = "Number of points",
       fill = "Point type") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

x11()
print(p_eco_control)
Sys.sleep(7)


###############################################################################
# 11) EXTRACT NDVI FOR BACKGROUND POINTS
###############################################################################

manual_path <- "data/modis"

ndvi_files <- list.files(
  manual_path,
  pattern = "NDVI.*\\.tif$",
  full.names = TRUE
)

print(ndvi_files)

ndvi_stack <- rast(ndvi_files)

ndvi_mean <- mean(ndvi_stack, na.rm = TRUE)

switzerland_vect <- vect(Switzerland)
switzerland_vect <- project(switzerland_vect, crs(ndvi_mean))

ndvi_switzerland <- crop(ndvi_mean, switzerland_vect)
ndvi_switzerland <- mask(ndvi_switzerland, switzerland_vect)

pseudo_vect <- vect(
  pseudo_points,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

pseudo_vect <- project(pseudo_vect, crs(ndvi_switzerland))

pseudo_ndvi_values <- terra::extract(
  ndvi_switzerland,
  pseudo_vect
)

pseudo_points$NDVI <- pseudo_ndvi_values[, 2]

summary(pseudo_points$NDVI)


###############################################################################
# 12) VISUAL CONTROL OF NDVI
###############################################################################

ndvi_control <- rbind(
  data.frame(NDVI = libellula_points$NDVI,
             point_type = "Libellula"),
  data.frame(NDVI = pseudo_points$NDVI,
             point_type = "Random points")
)

p_ndvi_control <- ggplot(
  ndvi_control,
  aes(x = NDVI,
      fill = point_type)
) +
  geom_density(alpha = 0.45) +
  scale_fill_manual(values = c("Libellula" = "#CD2626",
                               "Random points" = "grey50")) +
  labs(title = "NDVI control",
       x = "Mean NDVI",
       y = "Density",
       fill = "Point type") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_ndvi_control)
Sys.sleep(7)


###############################################################################
# 13) EXTRACT CHELSA CLIMATE FOR BACKGROUND POINTS
###############################################################################

coords_pseudo <- data.frame(
  longitude = pseudo_points$longitude,
  latitude = pseudo_points$latitude
)

t_r <- getChelsa(
  var = "tas",
  coords = coords_pseudo[, c("longitude", "latitude")],
  startdate = as.Date("2018-01-01"),
  enddate = as.Date("2021-12-15"),
  dataset = "chelsa-monthly"
)

names(t_r) <- make.names(names(t_r), unique = TRUE)

t_mat <- as.matrix(t_r[, names(t_r) != "time"])
t_mean_k <- colMeans(t_mat, na.rm = TRUE)
t_mean_c <- t_mean_k - 273.15

pseudo_points$tmax_mean_c <- as.numeric(t_mean_c)

prec_r <- getChelsa(
  var = "pr",
  coords = coords_pseudo[, c("longitude", "latitude")],
  startdate = as.Date("2018-01-01"),
  enddate = as.Date("2021-12-15"),
  dataset = "chelsa-monthly"
)

names(prec_r) <- make.names(names(prec_r), unique = TRUE)

prec_mat <- as.matrix(prec_r[, names(prec_r) != "time"])
prec_mean <- colMeans(prec_mat, na.rm = TRUE)

pseudo_points$prec_mean_annual <- as.numeric(prec_mean)

summary(pseudo_points$tmax_mean_c)
summary(pseudo_points$prec_mean_annual)

climate_control <- rbind(
  data.frame(variable = libellula_points$tmax_mean_c,
             point_type = "Libellula",
             climate = "Temperature"),
  data.frame(variable = pseudo_points$tmax_mean_c,
             point_type = "Random points",
             climate = "Temperature"),
  data.frame(variable = libellula_points$prec_mean_annual,
             point_type = "Libellula",
             climate = "Precipitation"),
  data.frame(variable = pseudo_points$prec_mean_annual,
             point_type = "Random points",
             climate = "Precipitation")
)

p_climate_control <- ggplot(
  climate_control,
  aes(x = variable,
      fill = point_type)
) +
  geom_density(alpha = 0.45) +
  facet_wrap(~ climate, scales = "free") +
  scale_fill_manual(values = c("Libellula" = "#CD2626",
                               "Random points" = "grey50")) +
  labs(title = "Climate control",
       x = "Value",
       y = "Density",
       fill = "Point type") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_climate_control)
Sys.sleep(7)


###############################################################################
# 14) BUILD MACHINE LEARNING DATASET
###############################################################################

# We now create the final table for the model.
# Libellula points are labelled as "libellula".
# Random background points are labelled as "background".

libellula_ml <- data.frame(
  presence_class = "libellula",
  distance_to_nearest_castor_km = libellula_points$distance_to_nearest_castor_km,
  elevation = libellula_points$elevation,
  NDVI = libellula_points$NDVI,
  tmax_mean_c = libellula_points$tmax_mean_c,
  prec_mean_annual = libellula_points$prec_mean_annual,
  eco_values = libellula_points$eco_values
)

pseudo_ml <- data.frame(
  presence_class = "background",
  distance_to_nearest_castor_km = pseudo_points$distance_to_nearest_castor_km,
  elevation = pseudo_points$elevation,
  NDVI = pseudo_points$NDVI,
  tmax_mean_c = pseudo_points$tmax_mean_c,
  prec_mean_annual = pseudo_points$prec_mean_annual,
  eco_values = pseudo_points$eco_values
)

ml_df <- rbind(libellula_ml, pseudo_ml)

ml_df <- na.omit(ml_df)

ml_df$presence_class <- as.factor(ml_df$presence_class)

head(ml_df)
table(ml_df$presence_class)


###############################################################################
# 15) VISUAL CONTROL OF FINAL ML DATASET
###############################################################################

p_ml_distance <- ggplot(
  ml_df,
  aes(x = distance_to_nearest_castor_km,
      fill = presence_class)
) +
  geom_density(alpha = 0.45) +
  labs(title = "Distance to Castor in the ML dataset",
       x = "Distance to nearest Castor fiber (km)",
       y = "Density",
       fill = "Class") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_ml_distance)
Sys.sleep(7)

p_ml_env <- ggplot(
  ml_df,
  aes(x = elevation,
      y = NDVI,
      color = presence_class)
) +
  geom_point(alpha = 0.35, size = 1) +
  labs(title = "Environmental space of Libellula and background points",
       x = "Elevation (m)",
       y = "Mean NDVI",
       color = "Class") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_ml_env)
Sys.sleep(7)

summary(pseudo_points$NDVI)
summary(libellula_points$NDVI)
table(is.na(pseudo_points$NDVI))
###############################################################################
# 16) TRAIN AND TEST SPLIT
###############################################################################

# We split the data into a training set and a testing set.
# The training set is used to build the model.
# The testing set is used to evaluate predictions.

set.seed(123)

train_index <- createDataPartition(
  y = ml_df$presence_class,
  p = 0.7,
  list = FALSE
)

train_df <- ml_df[train_index, ]
test_df <- ml_df[-train_index, ]

table(train_df$presence_class)
table(test_df$presence_class)


###############################################################################
# 17) TRAIN RANDOM FOREST MODEL
###############################################################################

# The model predicts whether a point is a Libellula presence
# or a background point.
# It uses environmental variables and distance to Castor fiber.

rf_libellula <- randomForest(
  presence_class ~ .,
  data = train_df,
  ntree = 500,
  importance = TRUE
)

print(rf_libellula)


###############################################################################
# 18) PREDICT ON TEST DATA
###############################################################################

pred_class <- predict(
  rf_libellula,
  newdata = test_df
)

head(pred_class)


###############################################################################
# 19) MODEL EVALUATION
###############################################################################

# The confusion matrix compares predicted classes with observed classes.

confusionMatrix(
  data = pred_class,
  reference = test_df$presence_class
)


###############################################################################
# 20) VARIABLE IMPORTANCE
###############################################################################

# This is one of the most important outputs.
# It shows which variables are most useful to predict Libellula presence.

importance_df <- as.data.frame(importance(rf_libellula))
importance_df$feature <- rownames(importance_df)

importance_df <- importance_df[
  order(importance_df$MeanDecreaseGini, decreasing = TRUE),
]

p_importance <- ggplot(
  importance_df,
  aes(x = reorder(feature, MeanDecreaseGini),
      y = MeanDecreaseGini)
) +
  geom_col(fill = "darkolivegreen3", color = "black") +
  coord_flip() +
  labs(title = "Variable importance for Libellula prediction",
       x = "Variable",
       y = "Mean decrease in Gini") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_importance)
Sys.sleep(7)


###############################################################################
# 21) PREDICTED PROBABILITIES
###############################################################################

# We also predict probabilities instead of only classes.

pred_prob <- predict(
  rf_libellula,
  newdata = test_df,
  type = "prob"
)

head(pred_prob)


###############################################################################
# 22) PROBABILITY CURVE ALONG DISTANCE TO CASTOR
###############################################################################

# This graph shows how predicted probability of Libellula presence
# changes along the distance to Castor gradient.
# Other variables are kept at their average values.

distance_curve <- data.frame(
  distance_to_nearest_castor_km = seq(
    min(ml_df$distance_to_nearest_castor_km),
    max(ml_df$distance_to_nearest_castor_km),
    length.out = 200
  )
)

distance_curve$elevation <- mean(ml_df$elevation, na.rm = TRUE)
distance_curve$NDVI <- mean(ml_df$NDVI, na.rm = TRUE)
distance_curve$tmax_mean_c <- mean(ml_df$tmax_mean_c, na.rm = TRUE)
distance_curve$prec_mean_annual <- mean(ml_df$prec_mean_annual, na.rm = TRUE)
distance_curve$eco_values <- round(mean(ml_df$eco_values, na.rm = TRUE))

prob_curve <- predict(
  rf_libellula,
  newdata = distance_curve,
  type = "prob"
)

distance_curve$prob_libellula <- prob_curve[, "libellula"]

p_prob_curve <- ggplot(
  distance_curve,
  aes(x = distance_to_nearest_castor_km,
      y = prob_libellula)
) +
  geom_line(linewidth = 1.2, color = "#CD2626") +
  labs(title = "Predicted probability of Libellula depressa",
       x = "Distance to nearest Castor fiber (km)",
       y = "Predicted probability") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_prob_curve)
Sys.sleep(7)


###############################################################################
# 23) SAVE MACHINE LEARNING DATASET
###############################################################################

write.csv(
  ml_df,
  "data/libellula_ml_dataset_with_background.csv",
  row.names = FALSE
)

###############################################################################
# 24) Comments on the model 
###############################################################################

#The Random Forest model gave very strong results for predicting Libellula depressa presence. The model used 500 decision trees and produced a very low training error rate of only 0.37%. 
#In the test dataset, all 1348 background points were correctly classified and 1392 out of 1404 Libellula occurrences were correctly predicted. Only 12 classification errors were observed, leading to an overall accuracy of 99.56%. 
#These results show that the environmental variables used in the model strongly differentiate suitable habitats for Libellula depressa from random locations in Switzerland.

#The variable importance analysis showed that temperature was the most important factor explaining Libellula depressa presence. However, distance to the nearest Castor fiber occurrence was also highly important, even more important than elevation, NDVI or precipitation. 
#The probability curve also showed that predicted probability of Libellula presence was generally higher close to Castor fiber occurrences and decreased with increasing distance. This suggests that habitats associated with beaver presence may favour Libellula depressa occurrence.

#However, some limitations must be considered. The model compares real Libellula occurrences with randomly generated background points across Switzerland. Some of these random points are located in clearly unsuitable habitats such as high mountains or very cold environments, which probably increases model performance. 
#Therefore, the very high accuracy should be interpreted carefully. In addition, the observed relationship between Libellula depressa and Castor fiber does not necessarily indicate a direct ecological interaction, as both species may simply share similar environmental preferences.