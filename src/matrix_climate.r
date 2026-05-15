
# =========================
# 1) PACKAGES
# =========================
#install.packages("remotes")
#remotes::install_git("https://gitlabext.wsl.ch/karger/rchelsa.git")
library(Rchelsa)
library(terra)
library(dplyr)
library(ggplot2)

# =========================
# 2) STARTING DATASET
# =========================
#Loading of the matrix created with my species, their occurences and the ecosystem data 

species_df <- read.csv("data/matrix_full_eco")
species_df$occurrence_id <- 1:nrow(species_df)
# Display the initial dataset
head(species_df)

# =========================
# 3) CREATE A SPATIAL OBJECT
# =========================
# CHELSA requires coordinates. We therefore create a spatial vector
# from the longitude and latitude columns.

pts_v <- terra::vect(
  species_df,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Extract simple coordinates as a standard data frame
coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
  rename(
    longitude = x,
    latitude = y
  ) %>%
  mutate(occurrence_id = species_df$occurrence_id)

head(coords_df)

# =========================
# 4) EXTRACT MONTHLY Tmax FOR 2018
# =========================
# CHELSA variable naming:
# - tas    = near-surface air temperature
# - tasmin = minimum near-surface air temperature
# - tasmax = maximum near-surface air temperature
# - pr     = precipitation
#
# Temperature values are often returned in Kelvin.
# Conversion to Celsius: °C = K - 273.15

##I choose here the data between 2018 and 2021 because taking more than that was too heavy for my computer. 
t_r <- getChelsa(
  var       = "tas",
  coords    = coords_df %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2021-12-15"),
  dataset   = "chelsa-monthly"
)

#Here i had to define unique value cause some points had the same localisation. 
names(t_r) <- make.names(names(t_r), unique = TRUE)

# Remove the time column with dplyr, then convert to matrix
t_mat <- t_r %>%
  dplyr::select(-time) %>%
  as.matrix()

# Calculate the mean across the 12 months for each point
# colMeans() works by column, and here each column corresponds to one point
t_mean_k <- colMeans(t_mat, na.rm = TRUE)

# Convert Kelvin to Celsius
t_mean_c <- t_mean_k - 273.15

# Create a table containing the new climate variable
t_df <- data.frame(
  occurrence_id = species_df$occurrence_id,
  t_mean_c = as.numeric(t_mean_c)
)

head(t_df)

# =========================
# 5) EXTRACT MONTHLY PRECIPITATION FOR 2018
# =========================

prec_r <- getChelsa(
  var       = "pr",
  coords    = coords_df %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2018-01-01"),
  enddate   = as.Date("2021-12-15"),
  dataset   = "chelsa-monthly"
)
names(prec_r) <- make.names(names(prec_r), unique = TRUE)
# Remove the time column with dplyr, then convert to matrix
prec_mat <- prec_r %>%
  dplyr::select(-time) %>%
  as.matrix()

# Calculate the mean across the 12 months for each point
prec_mean <- colMeans(prec_mat, na.rm = TRUE)

# Create a table containing the precipitation variable
prec_df <- data.frame(
  occurrence_id = species_df$occurrence_id,
  prec_mean_annual = as.numeric(prec_mean)
)

head(prec_df)

# =========================
# 6) JOIN THE NEW CLIMATE VARIABLES
#    TO THE ORIGINAL DATASET
# =========================
# This is the key teaching point:
# we start from an existing dataset and add new columns
# extracted from an external source.

species_climate_df <- species_df %>%
  left_join(t_df, by = "occurrence_id") %>%
  left_join(prec_df, by = "occurrence_id")

head(species_climate_df)

# =========================
# 7) CHECK THE RESULT
# =========================

dim(species_df)           # original dimensions
dim(species_climate_df)   # enriched dimensions
names(species_climate_df) # column names after enrichment

# =========================
# 8) PLOTS
# =========================
#Plot to check the temperature mean assoociated with the species occurences
p_temp <- ggplot(species_climate_df,
                 aes(x = species,
                     y = t_mean_c,
                     fill = species)) +

  geom_violin(alpha = 0.5,
              color = NA) +

  geom_boxplot(width = 0.15,
               outlier.shape = NA,
               alpha = 0.8) +

  labs(
    title = "Temperature associated with species occurrences",
    x = "Species",
    y = "Mean temperature (°C)"
  ) +

  theme_classic(base_size = 13) +

  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

x11()
print(p_temp)
Sys.sleep(7)

#Plot to check the temperature mean associated with the species occurences
p_prec <- ggplot(species_climate_df,
                 aes(x = species,
                     y = prec_mean_annual,
                     fill = species)) +

  geom_violin(alpha = 0.5,
              color = NA) +

  geom_boxplot(width = 0.15,
               outlier.shape = NA,
               alpha = 0.8) +

  labs(
    title = "Precipitation associated with species occurrences",
    x = "Species",
    y = "Mean annual precipitation"
  ) +

  theme_classic(base_size = 13) +

  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

x11()
print(p_prec)
Sys.sleep(7)

#Graph to compare the climatic preference per species
 p_climate_space <- ggplot(species_climate_df,
                          aes(x = t_mean_c,
                              y = prec_mean_annual,
                              color = species)) +

  geom_point(alpha = 0.25,
             size = 1) +

  geom_density_2d(linewidth = 1) +

  labs(
    title = "Climatic space occupied by species",
    x = "Mean temperature (°C)",
    y = "Mean annual precipitation",
    color = "Species"
  ) +

  theme_classic(base_size = 13) +

  theme(
    plot.title = element_text(hjust = 0.5)
  )

x11()
print(p_climate_space)
Sys.sleep(7)

#Temperature distribution across climatic categories 

p_temp_climate <- ggplot(species_climate_df,
                         aes(x = Climate_Re,
                             y = t_mean_c,
                             fill = species)) +

  geom_boxplot(alpha = 0.8) +

  labs(
    title = "Temperature distribution across climate categories",
    x = "Climate category",
    y = "Mean temperature (°C)",
    fill = "Species"
  ) +

  theme_classic(base_size = 13) +

  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

x11()
print(p_temp_climate)
Sys.sleep(7)


#Precipitation distribution across climatic categories 
p_prec_climate <- ggplot(species_climate_df,
                         aes(x = Climate_Re,
                             y = prec_mean_annual,
                             fill = species)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Precipitation distribution across climate categories",
       x = "Climate category",
       y = "Mean annual precipitation",
       fill = "Species") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

x11()
print(p_prec_climate)
Sys.sleep(7)


write.csv(species_climate_df, "data/matrix_afterclimate.csv", row.names = FALSE)





