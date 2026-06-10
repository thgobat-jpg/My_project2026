###############################################################################
# PCA BIPLOT
# Goal: compare the environmental niches of Castor fiber and Libellula depressa
###############################################################################

# =========================
# 1) PACKAGES
# =========================

# ggplot2 is used for graphics.
# dplyr is used for simple data manipulation.
# factoextra is used to make clean PCA graphics.

library(ggplot2)
library(dplyr)
library(factoextra)


###############################################################################
# 2) LOAD FINAL MATRIX
###############################################################################

# This matrix contains the species occurrences and the environmental variables
# created during the first part of the project.

matrix_full <- read.csv("data/matrix.final.csv")

# Quick checks.
head(matrix_full)
names(matrix_full)
table(matrix_full$species)


###############################################################################
# 3) SELECT VARIABLES FOR PCA
###############################################################################

# A PCA can only use numerical variables.
# Here we select environmental variables that describe climate, topography
# and vegetation productivity.

pca_df <- matrix_full[, c("species",
                          "elevation",
                          "NDVI",
                          "t_mean_c",
                          "prec_mean_annual")]

# Remove rows with missing values because PCA cannot use NA values.

pca_df <- na.omit(pca_df)

# Check the cleaned dataset.

head(pca_df)
summary(pca_df)
table(pca_df$species)


###############################################################################
# 4) PREPARE NUMERICAL VARIABLES
###############################################################################

# The species column is not used to calculate the PCA.
# It is only kept later to color the points in the graph.

pca_variables <- pca_df[, c("elevation",
                            "NDVI",
                            "t_mean_c",
                            "prec_mean_annual")]

# Check that all selected columns are numerical.

str(pca_variables)


###############################################################################
# 5) RUN PCA
###############################################################################

# prcomp() calculates the PCA.
# center = TRUE centers each variable around its mean.
# scale. = TRUE standardizes variables so they can be compared fairly.
# This is important because elevation, NDVI, temperature and precipitation
# have different units and ranges.

pca_result <- prcomp(
  pca_variables,
  center = TRUE,
  scale. = TRUE
)

# Show how much variance is explained by each PCA axis.

summary(pca_result)


###############################################################################
# 6) EXTRACT PCA SCORES
###############################################################################

# pca_result$x contains the coordinates of each occurrence in the PCA space.
# Each row is one occurrence.

pca_scores <- as.data.frame(pca_result$x)

# Add the species information again for coloring the points.

pca_scores$species <- pca_df$species

head(pca_scores)


###############################################################################
# 7) CREATE AXIS LABELS WITH EXPLAINED VARIANCE
###############################################################################

# This calculates the percentage of variance explained by each PCA axis.

var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

pc1_lab <- paste0("PC1 (", round(var_explained[1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(var_explained[2], 1), "%)")


###############################################################################
# 8) PCA POINTS WITH ELLIPSES
###############################################################################

# This first plot shows the position of each occurrence in the PCA space.
# Points close together have similar environmental conditions.
# Ellipses summarize the environmental space occupied by each species.

p_pca_points <- ggplot(pca_scores,
                       aes(x = PC1,
                           y = PC2,
                           color = species,
                           fill = species)) +
  stat_ellipse(geom = "polygon",
               alpha = 0.15,
               color = NA) +
  stat_ellipse(linewidth = 1) +
  geom_point(size = 1.2,
             alpha = 0.45) +
  labs(title = "Environmental niche comparison of the two species",
       x = pc1_lab,
       y = pc2_lab,
       color = "Species",
       fill = "Species") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_pca_points)
Sys.sleep(7)


###############################################################################
# 9) PCA BIPLOT
###############################################################################

# This biplot shows both:
# - the occurrences of the two species
# - the environmental variables as arrows
#
# The arrows indicate which variables structure the PCA space.
# Occurrences located in the direction of an arrow are associated
# with higher values of that variable.

p_pca_biplot <- fviz_pca_biplot(
  pca_result,
  geom.ind = "point",
  habillage = pca_df$species,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  label = "var",
  col.var = "black",
  repel = TRUE,
  pointsize = 1.4,
  alpha.ind = 0.45
) +
  labs(title = "PCA biplot of environmental niches",
       x = pc1_lab,
       y = pc2_lab,
       color = "Species",
       fill = "Species") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_pca_biplot)
Sys.sleep(7)


###############################################################################
# 10) VARIABLE CONTRIBUTION TO PCA AXES
###############################################################################

# This plot shows which environmental variables contribute most
# to the PCA axes.
# It helps interpret the PCA biplot.

p_var_contrib <- fviz_contrib(
  pca_result,
  choice = "var",
  axes = 1,
  top = 4
) +
  labs(title = "Variable contribution to PC1") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_var_contrib)
Sys.sleep(7)

p_var_contrib2 <- fviz_contrib(
  pca_result,
  choice = "var",
  axes = 2,
  top = 4
) +
  labs(title = "Variable contribution to PC2") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

x11()
print(p_var_contrib2)
Sys.sleep(7)



###############################################################################
# 11) INTERPRETATION
###############################################################################
#The PCA explained 86.7% of the total variation, with the PC1 explaining 65.3% and PC1 21.4%. 
#As shown on the graphic PC1 contribution, we can that this axis is mainly associated with elevation and t_mean while the mean precipitation was also high. 
#Meaning that this axis mainly represent a gradient opposing high elevation and high precipitation areas to warmer and lower elevation areas. 
#PC2 was mainly driven by NDVI. 
#The niches of the two species are clearly overlapping, meaning that both species occur under similar environmental conditions 
#However, the distribution of Libellula depressa is much wider, showing that the species can occupy a broader niche than the Beaver. 
#In this case, we can see here that Libellula can also live at a higher altitude than the beaver and is abble to colonise more rainy areas. 
###############################################################################