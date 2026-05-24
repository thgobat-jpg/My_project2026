###My project part 2###
#My question for this project was to understand if the presence of the beaver could influence the presence of Libellula depressa. 

#Packages loading : 
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(plotly)
library(rnaturalearth)

sf_use_s2(FALSE)

matrix_final=read.csv("data/matrix.final.csv")
head(matrix_final)
names(matrix_final)

################################################################################################################################
## 1. Graphical determination of the clinatic niches for each species : 
################################################################################################################################

#1.1 : Temperarure x annual precipitation 
p_clim <- ggplot(matrix_final,
aes(x = tmax_mean_c,
    y = prec_mean_annual,
    color = species)) +

geom_point(alpha = 0.3, size = 1) +

geom_smooth(method = "loess",
            se = TRUE,
            linewidth = 1) +

labs(title = "Climatic niche of the two species",
     x = "Mean maximum temperature (°C)",
     y = "Mean annual precipitation") +

theme_classic(base_size = 13) +

theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_clim)

#1.2 : Temperature distribution 
p_temp <- ggplot(matrix_final,
aes(x = tmax_mean_c,
    fill = species)) +

geom_density(alpha = 0.45) +

labs(title = "Temperature niche distribution",
     x = "Mean maximum temperature (°C)",
     y = "Density") +

theme_classic(base_size = 13) +

theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_temp)

#1.3 : Precipitation distribution : 
p_prec <- ggplot(matrix_final,
aes(x = prec_mean_annual,
    fill = species)) +

geom_density(alpha = 0.45) +

labs(title = "Precipitation niche distribution",
     x = "Mean annual precipitation",
     y = "Density") +

theme_classic(base_size = 13) +

theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_prec)

#1.4 : altitude distribution : nul on voit même pas quelle altitude est mieux...
p_elev <- ggplot(matrix_final,
aes(x = species,
    y = elevation,
    color = species,
    fill = species)) +

geom_boxplot(
  width = 0.22,
  alpha = 0.25,
  outlier.shape = NA
) +

geom_jitter(
  width = 0.12,
  alpha = 0.12,
  size = 0.7
) +

stat_summary(
  fun = mean,
  geom = "point",
  shape = 18,
  size = 4,
  color = "black"
) +

labs(
  title = "Elevation associated with species occurrences",
  x = "Species",
  y = "Elevation (m)"
) +

theme_classic(base_size = 13) +

theme(
  plot.title = element_text(hjust = 0.5, face = "bold"),
  legend.position = "none"
)

x11()
print(p_elev)

#1.5 Landcover preferences 
p_landcover <- ggplot(matrix_final,
aes(x = Landcover,
    fill = species)) +

geom_bar(position = "dodge") +

labs(title = "Landcover categories associated with species",
     x = "Landcover",
     y = "Number of occurrences") +

theme_classic(base_size = 13) +

theme(plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1))

x11()
print(p_landcover)

#1.6 Ecosystem preferences : 
p_ecosystem <- ggplot(matrix_final,
aes(x = W_Ecosystm,
    fill = species)) +

geom_bar(position = "dodge") +

labs(title = "Ecosystem types associated with species",
     x = "Ecosystem type",
     y = "Number of occurrences") +

theme_classic(base_size = 13) +

theme(plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 55, hjust = 1))

x11()
print(p_ecosystem)

#1.7 Altitude x NDVI : 
p_ndvi_alt <- ggplot(matrix_final,
aes(x = elevation,
    y = NDVI,
    color = species)) +

geom_point(alpha = 0.3, size = 1) +

geom_smooth(method = "loess",
            se = TRUE,
            linewidth = 1) +

facet_wrap(~ species) +

labs(title = "NDVI along the elevation gradient",
     x = "Elevation (m)",
     y = "Mean NDVI") +

theme_classic(base_size = 13) +

theme(plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none")

x11()
print(p_ndvi_alt)

#1.8 First interpretation : 
#The analyses showed that the two species share many environmental conditions in Switzerland.

#However, Libellula depressa seemed to live in a larger variety of environments than Castor fiber.

#The dragonfly was found across a larger range of temperature, precipitation and elevation. 
#One of the biggest difference is that Libellula depressa was still present at high elevation, even with a lower NDVi index

#In contrast, Castor fiber was mainly found at lower elevation and in areas with high vegetation cover. It showed also that Beaver are far less present in moutain areas

#Landcover analyses showed that Castor fiber was often associated with croplands and lowland areas, while Libellula depressa was also common in forests and grasslands.

#These results are interesting for the main question of the project.
#Even if the two species do not live in exactly the same habitats, they still overlap in many places.

#Therefore, habitats created by Castor fiber, such as ponds and wetlands, could help create suitable conditions for Libellula depressa.

################################################################################################################################
## 2. Création of a variable called "Distance to beaver"
################################################################################################################################

#2.1 splitting of the two species : 
castor_points <- filter(
  matrix_final,
  species == "Castor fiber"
)

libellula_points <- filter(
  matrix_final,
  species == "Libellula depressa"
)

View(castor_points)
nrow(castor_points)
nrow(libellula_points)

#2.2 Conversion to spatial object : 
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

#2.3 Projection to swiss metrics 
castor_sf_2056 <- st_transform(
  castor_sf,
  2056
)

libellula_sf_2056 <- st_transform(
  libellula_sf,
  2056
)

#2.4 Find the nearest beaver for each libellula point 
nearest_castor_id <- st_nearest_feature(
  libellula_sf_2056,
  castor_sf_2056
)

nearest_castor <- castor_sf_2056[
  nearest_castor_id,
]
View(nearest_castor)

#2.5 Distance computation : 
distance_values <- st_distance(
  libellula_sf_2056,
  nearest_castor,
  by_element = TRUE
)

libellula_points$distance_to_nearest_castor_m <- as.numeric(
  distance_values
)

summary(
  libellula_points$distance_to_nearest_castor_m
)

#2.6 Adding the distance to the final matrix 
castor_points$distance_to_nearest_castor_m <- 0

matrix_final_castor_distance <- bind_rows(
  castor_points,
  libellula_points
)

head(matrix_final_castor_distance)
View(matrix_final_castor_distance)
table(matrix_final_castor_distance$species)
#2.7 Visual control
switzerland <- ne_countries(
  country = "Switzerland",
  scale = "medium",
  returnclass = "sf"
)

p_map_distance <- ggplot() +

  geom_sf(
    data = switzerland,
    fill = "grey95",
    color = "black"
  ) +

  geom_point(
    data = castor_points,
    aes(x = longitude, y = latitude),
    color = "#009ACD",
    size = 1,
    alpha = 0.6
  ) +

  geom_point(
    data = libellula_points,
    aes(
      x = longitude,
      y = latitude,
      color = distance_to_nearest_castor_m
    ),
    size = 1,
    alpha = 0.7
  ) +

  scale_color_viridis_c() +

  labs(
    title = "Distance from Libellula depressa to nearest Castor fiber",
    x = "Longitude",
    y = "Latitude",
    color = "Distance (m)"
  ) +

  theme_classic(base_size = 13) +

  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

x11()
print(p_map_distance)

#2.8 Histogram of density of nearest localisation
libellula_points$distance_to_nearest_castor_km <-
  libellula_points$distance_to_nearest_castor_m / 1000

p_distance_hist_km <- ggplot(
  libellula_points,
  aes(x = distance_to_nearest_castor_km)
) +

  geom_histogram(
    bins = 40,
    fill = "darkolivegreen3",
    color = "black",
    alpha = 0.8
  ) +

  labs(
    title = "Distance to nearest Castor fiber occurrence",
    x = "Distance to nearest castor (km)",
    y = "Number of Libellula occurrences"
  ) +

  theme_classic(base_size = 13) +

  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

x11()
print(p_distance_hist_km)

#Save of the matrix 
write.csv(
  matrix_final_castor_distance,
  "data/matrix_final_with_castor_distance.csv",
  row.names = FALSE
)