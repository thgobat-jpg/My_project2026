##The goal of this script is to build a matrix with the occurence of the two target species (Castor fiber and Libellula depressa)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)

castor <- read.csv("data/Castor fiber.csv")
libellula <- read.csv("data/Libellula depressa.csv")
unique(castor$species)

names(castor)
View(castor)
names(libellula)

matrix_mix <- bind_rows(castor, libellula)

nrow(castor)
nrow(libellula)
nrow(matrix_mix)
unique(matrix_mix$species)

head(matrix_mix)
table(matrix_mix$species)
str(matrix_mix)

##Visualisation of the data from both species in switzerland : 
switzerland <- ne_countries(
  country = "Switzerland",
  scale = "medium",
  returnclass = "sf"
)
occ_sf <- st_as_sf(
  matrix_mix,
  coords = c("longitude", "latitude"),
  crs = 4326
)

x11()
ggplot() +
  geom_sf(data = switzerland, fill = "grey95", color = "black") +
  geom_sf(data = occ_sf, aes(color = species), size = 1.5, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Occurence of Castor fiber and Libellula depressa in Switzerland",
    color = "Species",
    x = "Longitude",y = "Latitude")

#Graphic summarizing the number of occurences per species and per origine 
occ_table <- table(matrix_mix$species, matrix_mix$source)
head(occ_table)

p_occ_source <- ggplot(matrix_mix,
                       aes(x = species, fill = source)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.8) +
  labs(title = "Number of occurrences per species and source",
       x = "Species",
       y = "Number of occurrences",
       fill = "Data source") +
  theme_classic(base_size = 13)

x11()
print(p_occ_source)
Sys.sleep(7)

#Temporal distribution of occurence per species
matrix_mix$date_obs <- as.Date(matrix_mix$date_obs)
matrix_mix$year_obs <- as.numeric(format(matrix_mix$date_obs, "%Y"))

p_time_species <- ggplot(matrix_mix,
                         aes(x = year_obs, fill = species)) +
  geom_histogram(binwidth = 1,
                 color = "black",
                 alpha = 0.75,
                 position = "identity") +
  labs(title = "Temporal distribution of occurrences by species",
       x = "Observation year",
       y = "Number of occurrences",
       fill = "Species") +
  theme_classic(base_size = 13)

x11()
print(p_time_species)
Sys.sleep(7)

write.csv(matrix_mix, "data/matrix_mix.csv", row.names = FALSE)