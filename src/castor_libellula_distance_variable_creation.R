################################################################################################################################
##Creation of the distance from beaver variable : 
################################################################################################################################

#The PCA made before showed that both species occur under similar environmental conditions in Swirtzerland. 
#However, This does not indicate if the presence of beaver could favorise the presence of Libellula depressa. 
#To answer this question, another variable has been added to the matrix : The calculated distance to each occurence point to the beaver. 
#Thus, this will help to determine if Libellula depressa tends to colonise the habitat created or occupied by the beaver. 

#Packages loading : 
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(plotly)
library(rnaturalearth)

sf_use_s2(FALSE)


################################################################################################################################
## 1. Création of a variable called "Distance to beaver"
################################################################################################################################

matrix_final <- read.csv("data/matrix.final.csv")

#1.1 splitting of the two species : 
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

#1.2 Conversion to spatial object : 
castor_unique <- castor_points[
  !duplicated(castor_points[, c("longitude", "latitude")]),
]

castor_sf <- st_as_sf(
  castor_unique,
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

#1.3 Projection to swiss metrics 
castor_sf_2056 <- st_transform(
  castor_sf,
  2056
)

libellula_sf_2056 <- st_transform(
  libellula_sf,
  2056
)

#1.4 Find the nearest beaver for each libellula point 
nearest_castor_id <- st_nearest_feature(
  libellula_sf_2056,
  castor_sf_2056
)

nearest_castor <- castor_sf_2056[
  nearest_castor_id,
]
View(nearest_castor)

#1.5 Distance computation : 
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

#1.6 Adding the distance to the final matrix 
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

#1.8 Histogram of density of nearest localisation
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

names(matrix_final_castor_distance)

