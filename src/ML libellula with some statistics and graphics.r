###############################################################################
# MACHINE LEARNING ANALYSIS
# Goal: predict Libellula depressa presence using environmental variables and distance to nearest Castor fiber occurrence.
###############################################################################
###############################################################################
# MACHINE LEARNING ANALYSIS
###############################################################################

# The goal of this analysis is to test whether the presence of Libellula depressa can be predicted using environmental variables
# and the distance to the nearest Castor fiber occurrence.
#
# Because GBIF and iNaturalist only provide presence data,true absence data are not available.
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
#which variables are the most important for predicting Libellula depressa presence.
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
# 2) LOAD FINAL MATRIX WITH CASTOR DISTANCE
###############################################################################

# This matrix contains the real species occurrences, environmental variables,
# and the distance to the nearest Castor fiber occurrence.

matrix_final <- read.csv("data/matrix_final_with_castor_distance.csv")

head(matrix_final)
names(matrix_final)
table(matrix_final$species)

# If the distance is only in meters, we create the distance in kilometers.
if(!"distance_to_nearest_castor_km" %in% names(matrix_final)) {
  matrix_final$distance_to_nearest_castor_km <-
    matrix_final$distance_to_nearest_castor_m / 1000
}


###############################################################################
# 3) SPLIT CASTOR AND LIBELLULA OCCURRENCES
###############################################################################

# Castor points are used to calculate the distance to nearest beaver occurrence.
# Libellula points are used as presence points for the machine learning model.

castor_points <- filter(matrix_final, species == "Castor fiber")
libellula_points <- filter(matrix_final, species == "Libellula depressa")

nrow(castor_points)
nrow(libellula_points)

names(matrix_final)

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
# 7) DISTANCE TO NEAREST CASTOR FOR BACKGROUND POINTS
###############################################################################

# The distance to Castor fiber is already present for the real Libellula depressa occurrences because it was created in a previous script.
#
# However, random background points were created inside this script.
# Therefore, we still need to calculate the distance from each random point to the nearest Castor fiber occurrence.

castor_sf <- st_as_sf(
  castor_points,
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
pseudo_sf_2056 <- st_transform(pseudo_sf, 2056)

nearest_castor_pseudo <- st_nearest_feature(
  pseudo_sf_2056,
  castor_sf_2056
)

nearest_castor_pseudo_sf <- castor_sf_2056[
  nearest_castor_pseudo,
]

distance_pseudo <- st_distance(
  pseudo_sf_2056,
  nearest_castor_pseudo_sf,
  by_element = TRUE
)

pseudo_points$distance_to_nearest_castor_m <- as.numeric(distance_pseudo)

pseudo_points$distance_to_nearest_castor_km <-
  pseudo_points$distance_to_nearest_castor_m / 1000

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

pseudo_points$t_mean_c <- as.numeric(t_mean_c)

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

summary(pseudo_points$t_mean_c)
summary(pseudo_points$prec_mean_annual)

climate_control <- rbind(
  data.frame(variable = libellula_points$t_mean_c,
             point_type = "Libellula",
             climate = "Temperature"),
  data.frame(variable = pseudo_points$t_mean_c,
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
  t_mean_c = libellula_points$t_mean_c,
  prec_mean_annual = libellula_points$prec_mean_annual,
  eco_values = libellula_points$eco_values
)

pseudo_ml <- data.frame(
  presence_class = "background",
  distance_to_nearest_castor_km = pseudo_points$distance_to_nearest_castor_km,
  elevation = pseudo_points$elevation,
  NDVI = pseudo_points$NDVI,
  t_mean_c = pseudo_points$t_mean_c,
  prec_mean_annual = pseudo_points$prec_mean_annual,
  eco_values = pseudo_points$eco_values
)

ml_df <- rbind(libellula_ml, pseudo_ml)

ml_df <- na.omit(ml_df)

ml_df$presence_class <- as.factor(ml_df$presence_class)

head(ml_df)
table(ml_df$presence_class)

names(libellula_points)
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
distance_curve$t_mean_c <- mean(ml_df$t_mean_c, na.rm = TRUE)
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
# 24) SIMPLE STATISTICAL ANALYSES
# Goal: test whether Libellula depressa occurrences are closer to Castor fiber
# than random background points.
###############################################################################


###############################################################################
#  LOAD MACHINE LEARNING DATASET
###############################################################################

ml_df <- read.csv("data/libellula_ml_dataset_with_background.csv")

head(ml_df)
names(ml_df)
table(ml_df$presence_class)

###############################################################################
# SIMPLE STATISTICAL ANALYSIS
###############################################################################

# Here I separate the two groups to make the code easier to understand.

libellula_distance <- ml_df[ml_df$presence_class == "libellula", ]

background_distance <- ml_df[ml_df$presence_class == "background", ]


# Median distance to Castor fiber for Libellula points

median(
  libellula_distance$distance_to_nearest_castor_km,
  na.rm = TRUE
)


# Median distance to Castor fiber for random background points

median(
  background_distance$distance_to_nearest_castor_km,
  na.rm = TRUE
)


# Wilcoxon test comparing the two distance distributions

wilcox_distance <- wilcox.test(
  libellula_distance$distance_to_nearest_castor_km,
  background_distance$distance_to_nearest_castor_km
)

wilcox_distance

summary(libellula_distance$distance_to_nearest_castor_km)
table(
  libellula_distance$distance_to_nearest_castor_km == 0
)

###############################################################################
# CORRELATION HEATMAP
###############################################################################

# This heatmap shows correlations between the variables used in the model.
# It helps to see if some variables are strongly related to each other.

cor_df <- ml_df[
  ,
  c("distance_to_nearest_castor_km",
    "elevation",
    "NDVI",
    "t_mean_c",
    "prec_mean_annual")
]

cor_df <- na.omit(cor_df)

cor_matrix <- cor(
  cor_df,
  method = "spearman"
)

print(cor_matrix)

cor_table <- as.data.frame(
  as.table(cor_matrix)
)

names(cor_table) <- c(
  "Variable_1",
  "Variable_2",
  "Correlation"
)

p_cor_heatmap <- ggplot(
  cor_table,
  aes(x = Variable_1,
      y = Variable_2,
      fill = Correlation)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(Correlation, 2)),
    size = 3
  ) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(-1, 1)
  ) +
  labs(title = "Correlation between model variables",
       x = "",
       y = "",
       fill = "Spearman\ncorrelation") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

x11()
print(p_cor_heatmap)
Sys.sleep(7)

# The Wilcoxon test showed a significant difference between Libellula depressa observations and the random background points (W = 4353899, p < 2.2e-16). 
# The median distance to the nearest Castor fiber occurrence was 0 km for Libellula depressa and 6.76 km for background points, suggesting that dragonflies are generally observed  close to beavers. 

# The correlation analysis showed that distance to the nearest Castor fiber was positively correlated with elevation (r = 0.64) and precipitation (r = 0.68), and negatively correlated with temperature (r = -0.59). 
# As expected, Temperature and elevation have a strong negative correlation of almost one (-0.96)

###############################################################################
# COORDINATE OVERLAP BETWEEN BOTH SPECIES
###############################################################################

castor_coord <- unique(castor_points[, c("longitude", "latitude")])
libellula_coord <- unique(libellula_points[, c("longitude", "latitude")])

shared_coord <- merge(castor_coord, libellula_coord)

coord_overlap <- data.frame(
  category = c("Shared coordinates", "Castor only", "Libellula only"),
  count = c(
    nrow(shared_coord),
    nrow(castor_coord) - nrow(shared_coord),
    nrow(libellula_coord) - nrow(shared_coord)
  )
)

print(coord_overlap)

p_coord_overlap <- ggplot(
  coord_overlap,
  aes(x = category, y = count, fill = category)
) +
  geom_col(color = "black", alpha = 0.8) +
  geom_text(aes(label = count), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c(
    "Shared coordinates" = "darkolivegreen3",
    "Castor only" = "#009ACD",
    "Libellula only" = "#CD2626"
  )) +
  labs(title = "Unique coordinates shared by both species",
       x = "",
       y = "Number of unique coordinates") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

x11()
print(p_coord_overlap)
Sys.sleep(7)


###############################################################################
# 25) Conclusion : 
###############################################################################

#The Random Forest model gave good results for predicting Libellula depressa presence. The model used 500 decision trees and produced an overall classification accuracy of 80.4%.
#In the test dataset, 216 out of 267 background points were correctly classified and 223 out of 279 Libellula occurrences were correctly predicted. A total of 107 classification errors were observed.
#These results show that the environmental variables used in the model can differentiate Libellula depressa occurrences from random locations in Switzerland.

#The variable importance analysis showed that distance to the nearest Castor fiber occurrence was the most important factor explaining Libellula depressa presence. Temperature, elevation and precipitation were also important variables, while NDVI and ecosystem type had a smaller contribution.
#The Wilcoxon test also showed that Libellula depressa occurrences were significantly closer to Castor fiber occurrences than random background points. The median distance was 2.5 km for Libellula depressa compared to 6.3 km for random locations.

#However, some limitations must be considered. The model compares real Libellula occurrences with randomly generated background points across Switzerland. Some of these random points are located in clearly unsuitable habitats such as high mountains or very cold environments, which probably increases model performance.
#Therefore, the model accuracy should be interpreted carefully.

#Another limitation comes from the occurrence data. Although duplicated coordinates were removed before the analyses, many unique coordinates were still shared by both species. A total of 302 coordinates were common to Castor fiber and Libellula depressa.
#Therefore, the relationship between Libellula depressa and Castor fiber may partly result from both species being recorded in similar habitats rather than from a direct ecological relationship.

#Overall, the results suggest that Libellula depressa and Castor fiber are often found in similar environments and that Libellula depressa occurrences tend to be located closer to Castor fiber occurrences than expected by chance.
#However, these results do not prove that beavers directly influence the presence of the dragonfly.


