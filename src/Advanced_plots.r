###############################################################################
#1 : INTERACTIVE LEAFLET MAP
###############################################################################

library(leaflet)
library(ggplot2)
library(plotly)

# Put the two species in one data frame for leaflet

map_points <- rbind(
  castor_points[, c("species", "longitude", "latitude")],
  libellula_points[, c("species", "longitude", "latitude")]
)

# Colors by species

species_cols <- c(
  "Castor fiber" = "#009ACD",
  "Libellula depressa" = "#CD2626"
)

pal_species <- colorFactor(
  palette = species_cols,
  domain = map_points$species
)

# Popup text

map_points$popup <- paste(
  "Species:", map_points$species,
  "<br>Longitude:", round(map_points$longitude, 4),
  "<br>Latitude:", round(map_points$latitude, 4)
)

# Interactive map, same logic as the professor script

p_leaflet_map <- leaflet(map_points) %>%
  addTiles() %>%
   setView(
    lng = 8.2,
    lat = 46.8,
    zoom = 7
  ) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 1.5,
    color = "black",
    weight = 1,
    fillColor = ~pal_species(species),
    fillOpacity = 0.7,
    popup = ~popup,
    group = ~species
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_species,
    values = ~species,
    title = "Species"
  ) %>%
  addLayersControl(
    overlayGroups = unique(map_points$species),
    options = layersControlOptions(collapsed = FALSE)
  )

p_leaflet_map

Sys.sleep(7)

###############################################################################
#2 : INTERACTIVE 3D PLOTLY SCATTERPLOT
###############################################################################

# This 3D plot only uses Libellula depressa occurrences.
# The variables were chosen according to the Random Forest results.
# Distance to Castor fiber, elevation and precipitation are used as the three axes, while NDVI is shown with a color gradient.
# Temperature hasn't been used  because it's strongly correlated with elevation, which would make the 3D plot less informative.

libellula_3d <- libellula_points[
  ,
  c("distance_to_nearest_castor_km",
    "elevation",
    "prec_mean_annual",
    "NDVI")
]

libellula_3d <- na.omit(libellula_3d)


libellula_3d$tooltip <- paste(
  "Species: Libellula depressa",
  "<br>Distance to Castor:", round(libellula_3d$distance_to_nearest_castor_km, 2), "km",
  "<br>Elevation:", round(libellula_3d$elevation, 0), "m",
  "<br>Precipitation:", round(libellula_3d$prec_mean_annual, 0),
  "<br>NDVI:", round(libellula_3d$NDVI, 3)
)


p_plotly_3d <- plot_ly(
  data = libellula_3d,
  x = ~distance_to_nearest_castor_km,
  y = ~elevation,
  z = ~prec_mean_annual,
  color = ~NDVI,
  text = ~tooltip,
  hoverinfo = "text",
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 3,
    opacity = 0.55
  )
) %>%
  layout(
    title = "Interactive 3D environmental space of Libellula depressa",
    scene = list(
      xaxis = list(title = "Distance to Castor fiber (km)"),
      yaxis = list(title = "Elevation (m)"),
      zaxis = list(title = "Annual precipitation")
    )
  )

# Display the plot

p_plotly_3d

Sys.sleep(7)

