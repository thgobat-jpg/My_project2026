#Part 1 : Intermediate project 
source("./src/matrix_libellula_depressa.r")
source("./src/matrix_castors_fiber.r")
source("./src/Complete_matrix.r")
source("./src/eco_spc_matrix.r")
source("./src/matrix_climate.r")
source("./src/elevation.R")
source("./src/sat_manual.r")


#Part 2 : Final project 
#2.1 Description of the research question 
source("./src/project_question.r")
#2.2 Creation of PCA to determine the habitat preferences for both species
source("./src/PCA_biplot.r")
#2.3 Creation of the variable "Distance to nearest beaver"
#Visualisation of ecological variables preferences for both species and creation of the variable 
source("./src/castor_libellula_distance_variable_creation.r") 
#2.4 Machine learning to determine if the presence of Beaver influence the prescence of Libellula
#Machine learning model, graphical visualisation of results, statistics, heatmap 
#Conclusion of the research question 
source("./src/ML libellula with some statistics and graphics.r")
#2.5 Advanced plots 
#With interactive map with species occurences and interctive 3D graphical with explanatory variables of Libellula occurences
source("./src/Advanced_plots.r")