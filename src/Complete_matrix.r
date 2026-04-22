library(dplyr)
library(readr)


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

matrix_mix
write.csv(matrix_mix, "data/matrix_mix.csv", row.names = FALSE)