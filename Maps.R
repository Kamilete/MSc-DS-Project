# Download data:
#
setwd("C:/Users/juang/Documents/Camilo/MSc Data Science/Research Project/Data")
fem <- read.csv("Femicides.csv")
#
# Change character variables as factors:
#
for (i in 1:ncol(fem)) {
  if (class(fem[,i]) == "character") {
    fem[,i] <- as.factor(fem[,i])
  }
}
#
# Exclude cases with file or preclusion due to atypicality or non-existence
#
fem_2015 <- subset(fem, ATIPICIDAD_INEXISTENCIA != "SI")
#
# Subset data >= 2017 & <= 2019
#
fem_2015 <- subset(fem_2015, ANIO_HECHO >= 2017 & ANIO_HECHO <= 2019)
#  
# Summaries:
#
summary(fem)
summary(fem_2015)
#
# Tables:
#
table(fem_2015$HOMICIDIO_DOLOSO_CONSUMADO, fem_2015$HECHO)
table(fem_2015$HOMICIDIO_DOLOSO_CONSUMADO, fem_2015$ESTADO_NOTICIA)
table(fem_2015$ETAPA, fem_2015$ESTADO_NOTICIA)
#
# Convert First letter of every word to Uppercase
#
library(stringr)
fem_2015$MUNICIPIO <- str_to_title(fem_2015$MUNICIPIO)
fem_2015$DEPARTAMENTO <- str_to_title(fem_2015$DEPARTAMENTO)
#
# Set DepMun column as a union between municipality and department
#
fem_2015$DepMun <- paste(fem_2015$DEPARTAMENTO, fem_2015$MUNICIPIO, sep = " ")
#
# Group the cases per municipality
#
Cases_mun <- as.data.frame(table(fem_2015$DepMun))
names(Cases_mun) <- c("DepMun", "Femicides")
Cases_mun$DepMun <- as.character(Cases_mun$DepMun)
#
# Install package colmaps
#
#install.packages("devtools")
#devtools::install_github("nebulae-co/colmaps")
library("colmaps")
#
# Set mun as a dataframe of the municipality data:
#
mun <- municipios@data
#
# Set DepMun column as a union between municipality and department
#
mun$DepMun <- paste(mun$depto, mun$municipio, sep = " ")
#
# Identified the municipality id
#
Cases_mun <- merge(Cases_mun, mun, by="DepMun")
#
# Plot femicides cases
#
library("ggplot2")
#
colmap(municipios, subset(Cases_mun), var = "Femicides", autocomplete = TRUE) 
#
# Population
#
# Instal package ""
#install.packages("readxl")
library("readxl")
#
pop <- read_excel("Population_2019.xlsx")
#
# Add 0 to municipality id's with only 4 digits and convert to characters
#
pop$MPIO <- ifelse(str_length(pop$MPIO) == 4, paste(0, as.character(pop$MPIO), sep = ""), as.character(pop$MPIO))
#
# Merge by municipality id
#
Cases_mun <- merge(Cases_mun, pop, by.x="id", by.y="MPIO")
#
# femicides cases per 100.000
#
Cases_mun$per_100k <- Cases_mun$Femicides/Cases_mun$Población * 100000
#
# Plot femicides cases per 100.000
#
colmap(municipios, subset(Cases_mun), var = "per_100k", autocomplete = TRUE) 

