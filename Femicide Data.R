
# Libraries

  library(stringr)

  # To plot municiplaities
  library("colmaps")

  #To read Excel spreadsheets
  library("readxl")

  # Plotting
  library("ggplot2")

# Download Femicides cases:

  dat_fem <- read.csv("Femicides.csv")

  # Exclude cases with file or preclusion due to atypical or non-existence

  dat_fem <- subset(dat_fem, ATIPICIDAD_INEXISTENCIA != "SI")
  
  # Exclude male cases
  
  dat_fem <- subset(dat_fem, SEXO_VICTIMA != "MASCULINO")

  # Subset data >= 2017 & <= 2019

  dat_fem <- subset(dat_fem, ANIO_HECHO >= 2017 & ANIO_HECHO <= 2019)

  # Convert First letter of every word to Uppercase

  dat_fem$MUNICIPIO <- str_to_title(dat_fem$MUNICIPIO)
  dat_fem$DEPARTAMENTO <- str_to_title(dat_fem$DEPARTAMENTO)

  # Set DepMun column as a union between municipality and department

  dat_fem$DepMun <- paste(dat_fem$DEPARTAMENTO, dat_fem$MUNICIPIO, sep = " ")

  # Group the cases per municipality and year

  fem_mun <- as.data.frame(aggregate(TOTAL_VICTIMAS~DepMun+ANIO_HECHO, data=dat_fem, sum))
  names(fem_mun) <- c("DepMun", "Year", "Femicides")
  fem_mun$DepMun <- as.character(fem_mun$DepMun)

  # Set mun as a dataframe of the municipality data:

  mun <- municipios@data

  # Set DepMun column as a union between municipality and department

  mun$DepMun <- paste(mun$depto, mun$municipio, sep = " ")

  # Find mismatch municipalities

  fem_mun$DepMun[!(fem_mun$DepMun %in% mun$DepMun)]

  # Correct the municipalities that don't match

  fem_mun$DepMun <- str_replace_all(fem_mun$DepMun, c('Boyaca' = 'Boyacá'))

  fem_mun$DepMun[fem_mun$DepMun == 'Antioquia Itagui'] <- 'Antioquia Itagüí'
  fem_mun$DepMun[fem_mun$DepMun == 'Antioquia San Vicente'] <- 'Antioquia San Vicente Ferrer'
  fem_mun$DepMun[fem_mun$DepMun == 'Antioquia Santa Barbara'] <- 'Antioquia Santa Bárbara'
  fem_mun$DepMun[fem_mun$DepMun == 'Bolívar Cartagena'] <- 'Bolívar Cartagena De Indias'
  fem_mun$DepMun[fem_mun$DepMun == 'Caquetá Belén De Los Andaquies'] <- 'Caquetá Belén De Los Andaquíes'
  fem_mun$DepMun[fem_mun$DepMun == 'Caquetá El Paujil'] <- 'Caquetá El Paujíl'
  fem_mun$DepMun[fem_mun$DepMun == 'Cauca Guapi'] <- 'Cauca Guapí'
  fem_mun$DepMun[fem_mun$DepMun == 'Cauca López'] <- 'Cauca López De Micay'
  fem_mun$DepMun[fem_mun$DepMun == 'Cauca Paez'] <- 'Cauca Páez'
  fem_mun$DepMun[fem_mun$DepMun == 'Chocó Alto Baudo'] <- 'Chocó Alto Baudó'
  fem_mun$DepMun[fem_mun$DepMun == 'Chocó Quibdo'] <- 'Chocó Quibdó'
  fem_mun$DepMun[fem_mun$DepMun == 'Cundinamarca Guayabal De Siquima'] <- 'Cundinamarca Guayabal De Síquima'
  fem_mun$DepMun[fem_mun$DepMun == 'Nariño Magüi'] <- 'Nariño Magüí'
  fem_mun$DepMun[fem_mun$DepMun == 'Nariño Tumaco'] <- 'Nariño San Andrés De Tumaco'
  fem_mun$DepMun[fem_mun$DepMun == 'Quindío Calarca'] <- 'Quindío Calarcá'
  fem_mun$DepMun[fem_mun$DepMun == 'Santander Lebríja'] <- 'Santander Lebrija'
  fem_mun$DepMun[fem_mun$DepMun == 'Tolima Mariquita'] <- 'Tolima San Sebastián De Mariquita'

  # Identified the municipality id

  fem_mun <- merge(fem_mun, mun, by="DepMun")

  # Read municipality population data

  pop <- read_excel("Population_2017-2019.xlsx")
  pop$AÑO <- as.factor(pop$AÑO)

  # Merge by municipality id and year

  Fem_DF <- merge(pop, fem_mun, by.x=c("MPIO", "AÑO") , by.y=c("id", "Year"), all.x = TRUE)

  # Delete unnecessary columns

  Fem_DF$DepMun <- NULL
  Fem_DF$id_depto <- NULL
  Fem_DF$municipio <- NULL
  Fem_DF$depto <- NULL

  # Replace NA's with zeros

  Fem_DF$Femicides[is.na(Fem_DF$Femicides)] <- 0

  # Avg femicides cases
  
  FDF <- as.data.frame(aggregate(Femicides~MPIO, data=Fem_DF, mean))
  
  # Avg women municipality populations
  
  FDF_1 <- as.data.frame(aggregate(Total_Mujeres~MPIO, data=Fem_DF, mean))
  
  # Merge Avg femicides cases and Avg women municipality populations
  
  FDF <- merge(FDF, FDF_1, by="MPIO", all.x = TRUE)
  
  # Femicides cases per 100.000
  
  FDF$Rate_Avg_Fem <- FDF$Femicides/FDF$Total_Mujeres * 100000
  
  # Change names of variables
  
  names(FDF)[2] <- "Avg_Femicides"
  names(FDF)[3] <- "Avg_Women"

  # Add other descriptive columns per municipality and department

  FDF <- merge(subset(Fem_DF, AÑO == 2019, select = c(MPIO, DP, DPNOM, DPMP)), FDF, by="MPIO", all.y = TRUE)
  
# Plot ratio of avg Femicides
  
  #colmap(municipios, subset(FDF), var = "Rate_Avg_Fem", data_id = "MPIO", autocomplete = FALSE)
  
# Read homicide data
  
  Homic <- read_excel("Homicide.xlsx")
  
  # Delete the last 3 digits of the municipality id's
  
  Homic$MPIO <- str_sub(Homic$MPIO, start = 1, end = -4)
  
  # Add 0 to municipality id's with only 4 digits
  
  Homic$MPIO <- ifelse(str_length(Homic$MPIO) == 4, paste(0, Homic$MPIO, sep = ""), Homic$MPIO)
  
  # Sum the homicides per year and municipality
  
  Homic_df <- as.data.frame(aggregate(CANTIDAD~MPIO+AÑO, data=Homic, sum))
  Homic_df$AÑO <- as.factor(Homic_df$AÑO)
  
  # Merge with municipality population data
  
  Homic_df2 <- merge(pop, Homic_df, by.x=c("MPIO", "AÑO") , by.y=c("MPIO", "AÑO"), all.x = TRUE)
  
  # Replace NA's with zeros
  
  Homic_df2$CANTIDAD[is.na(Homic_df2$CANTIDAD)] <- 0
  
  # Homicides per 100.000 men
  
  Homic_df2$M_Homicide <- Homic_df2$CANTIDAD/Homic_df2$Total_Hombres * 100000
  
  # Average homicide rates per municipality
  
  Homic_df3 <- as.data.frame(aggregate(M_Homicide~MPIO, data=Homic_df2, mean))
  
  # Add homicide data to the main data frame
  
  FDF <- merge(FDF, Homic_df3, by="MPIO")
  
# Read non-lethal violence data
  
  Non_lethal_W <- read_excel("non-lethal-violence.xlsx")
  Non_lethal_W$MPIO <- as.character(Non_lethal_W$MPIO)
  
  # Delete the last 3 digits of the municipality id's
  
  Non_lethal_W$MPIO <- str_sub(Non_lethal_W$MPIO, start = 1, end = -4)
  
  # Add 0 to municipality id's with only 4 digits
  
  Non_lethal_W$MPIO <- ifelse(str_length(Non_lethal_W$MPIO) == 4, paste(0, Non_lethal_W$MPIO, sep = ""), Non_lethal_W$MPIO)
  
  # Sum the non-lethal violence cases per year and municipality
  
  Non_lethal_df <- as.data.frame(aggregate(CANTIDAD~MPIO+AÑO, data=Non_lethal_W, sum))
  Non_lethal_df$AÑO <- as.factor(Non_lethal_df$AÑO)
  
  # Merge with municipality population data
  
  Non_lethal_df2 <- merge(pop, Non_lethal_df, by.x=c("MPIO", "AÑO") , by.y=c("MPIO", "AÑO"), all.x = TRUE)
  
  # Replace NA's with zeros
  
  Non_lethal_df2$CANTIDAD[is.na(Non_lethal_df2$CANTIDAD)] <- 0
  
  # Homicides per 100.000 women
  
  Non_lethal_df2$W_Non_lethal_V <- Non_lethal_df2$CANTIDAD/Non_lethal_df2$Total_Mujeres * 100000
  
  # Average non-lethal violence rates per municipality
  
  Non_lethal_df3 <- as.data.frame(aggregate(W_Non_lethal_V~MPIO, data=Non_lethal_df2, mean))
  
  # Add non-lethal violence data to the main data frame
  
  FDF <- merge(FDF, Non_lethal_df3, by="MPIO")
  
# Read domestic violence data
  
  Dom_v <- read_excel("Domestic_violence.xlsx")
  Dom_v$MPIO <- as.character(Dom_v$MPIO)
  
  # Delete the last 3 digits of the municipality id's
  
  Dom_v$MPIO <- str_sub(Dom_v$MPIO, start = 1, end = -4)
  
  # Add 0 to municipality id's with only 4 digits
  
  Dom_v$MPIO <- ifelse(str_length(Dom_v$MPIO) == 4, paste(0, Dom_v$MPIO, sep = ""), Dom_v$MPIO)
  
  # Subset Women cases
  
  Dom_v_W <- subset(Dom_v, GENERO == "FEMENINO")
  
  # Sum the domestic violence cases per year and municipality
  
  Dom_v_df <- as.data.frame(aggregate(CANTIDAD~MPIO+AÑO, data=Dom_v, sum))
  Dom_v_df$AÑO <- as.factor(Dom_v_df$AÑO)
  names(Dom_v_df)[3] <- "Total"
  
  Dom_v_Wdf <- as.data.frame(aggregate(CANTIDAD~MPIO+AÑO, data=Dom_v_W, sum))
  Dom_v_Wdf$AÑO <- as.factor(Dom_v_Wdf$AÑO)
  names(Dom_v_Wdf)[3] <- "Women"
  
  # Merge with municipality population data
  
  Dom_v_df2 <- merge(pop, Dom_v_df, by.x=c("MPIO", "AÑO") , by.y=c("MPIO", "AÑO"), all.x = TRUE)
  Dom_v_Wdf2 <- merge(pop, Dom_v_Wdf, by.x=c("MPIO", "AÑO") , by.y=c("MPIO", "AÑO"), all.x = TRUE)
  
  # Replace NA's with zeros
  
  Dom_v_df2$Total[is.na(Dom_v_df2$Total)] <- 0
  Dom_v_Wdf2$Women[is.na(Dom_v_Wdf2$Women)] <- 0
  
  # Domestic violence per 100.000 people / women
  
  Dom_v_df2$Domestic_V <- Dom_v_df2$Total/Dom_v_df2$Total_General * 100000
  Dom_v_Wdf2$W_Domestic_V <- Dom_v_Wdf2$Women/Dom_v_Wdf2$Total_Mujeres * 100000
  
  # Average domestic violence rates per municipality
  
  Dom_v_df3 <- as.data.frame(aggregate(Domestic_V~MPIO, data=Dom_v_df2, mean))
  Dom_v_Wdf3 <- as.data.frame(aggregate(W_Domestic_V~MPIO, data=Dom_v_Wdf2, mean))
  
  # Add domestic violence data to the main data frame
  
  FDF <- merge(FDF, Dom_v_df3, by="MPIO")
  FDF <- merge(FDF, Dom_v_Wdf3, by="MPIO")
  
# Read sexually assaulted data
  
  Sex_assault_W <- read_excel("Sexually_assaulted.xlsx")
  Sex_assault_W$MPIO <- as.character(Sex_assault_W$MPIO)
  
  # Delete the last 3 digits of the municipality id's
  
  Sex_assault_W$MPIO <- str_sub(Sex_assault_W$MPIO, start = 1, end = -4)
  
  # Add 0 to municipality id's with only 4 digits
  
  Sex_assault_W$MPIO <- ifelse(str_length(Sex_assault_W$MPIO) == 4, paste(0, Sex_assault_W$MPIO, sep = ""), Sex_assault_W$MPIO)
  
  # Sum the non-lethal violence cases per year and municipality
  
  Sex_assault_df <- as.data.frame(aggregate(CANTIDAD~MPIO+AÑO, data=Sex_assault_W, sum))
  Sex_assault_df$AÑO <- as.factor(Sex_assault_df$AÑO)
  
  # Merge with municipality population data
  
  Sex_assault_df2 <- merge(pop, Sex_assault_df, by.x=c("MPIO", "AÑO") , by.y=c("MPIO", "AÑO"), all.x = TRUE)
  
  # Replace NA's with zeros
  
  Sex_assault_df2$CANTIDAD[is.na(Sex_assault_df2$CANTIDAD)] <- 0
  
  # Sexual offense per 100.000 women
  
  Sex_assault_df2$W_Sex_assault <- Sex_assault_df2$CANTIDAD/Sex_assault_df2$Total_Mujeres * 100000
  
  # Average sexually assaulted rates per municipality
  
  Sex_assault_df3 <- as.data.frame(aggregate(W_Sex_assault~MPIO, data=Sex_assault_df2, mean))
  
  # Add non-lethal violence data to the main data frame
  
  FDF <- merge(FDF, Sex_assault_df3, by="MPIO")
  
# Read under 18 population data
  
  Pop_18 <- read_excel("Pop_Under_18.xlsx")
  
  # Average under 18 population per municipality
  
  Pop_18_Avg <- as.data.frame(aggregate(Under_18~MPIO, data=Pop_18, mean))
  
  # Add under 18 population data to the main data frame
  
  FDF <- merge(FDF, Pop_18_Avg, by="MPIO")
  
# Read teen birth rates data
  
  birth_r_W <- read_excel("Adolescent_pregnant.xlsx")
  birth_r_W$MPIO <- as.character(birth_r_W$MPIO)
  
  # Add 0 to municipality id's with only 4 digits
  
  birth_r_W$MPIO <- ifelse(str_length(birth_r_W$MPIO) == 4, paste(0, birth_r_W$MPIO, sep = ""), birth_r_W$MPIO)
  
  # Read adolescents women population data
  
  Pop_teen_W <- read_excel("Adolescents_Pop.xlsx")
  
  # Merge with adolescents women population data
  
  birth_r_W_Df <- merge(Pop_teen_W, birth_r_W, by.x=c("MPIO", "AÑO") , by.y=c("MPIO", "Year"), all.x = TRUE)
  
  # Births per 1.000 women
  
  birth_r_W_Df$W_teen_birth_r <- birth_r_W_Df$Adolescents/birth_r_W_Df$Adolescents_W * 1000
  
  # Average teen birth rates per municipality
  
  birth_r_W_Df2 <- as.data.frame(aggregate(W_teen_birth_r~MPIO, data=birth_r_W_Df, mean))
  
  # Add teen birth rates data to the main data frame
  
  FDF <- merge(FDF, birth_r_W_Df2, by="MPIO")
  
# Read school coverage data
  
  school_cov <- read.csv("School_coverage.csv")
  names(school_cov)[2] <- "MPIO"
  school_cov$MPIO <- as.character(school_cov$MPIO)
  
  # Subset data >= 2017 & <= 2019
  
  school_cov <- subset(school_cov, AÑO >= 2017 & AÑO <= 2019 & MUNICIPIO != "NACIONAL")
  
  # If the net school coverage is greater than 100%, limit it to 100
  
  school_cov$COBERTURA_NETA[school_cov$COBERTURA_NETA > 100] <- 100
  
  # Add 0 to municipality id's with only 4 digits
  
  school_cov$MPIO <- ifelse(str_length(school_cov$MPIO) == 4, paste(0, school_cov$MPIO, sep = ""), school_cov$MPIO)
  
  # Average the net school coverage rate per municipality
  
  school_cov_df <- as.data.frame(aggregate(COBERTURA_NETA~MPIO, data=school_cov, mean))
  
  # Add the net school coverage data to the main data frame
  
  FDF <- merge(FDF, school_cov_df, by="MPIO")
  names(FDF)[15] <- "School_cov"
  
# Read poverty index data
  
  Poverty <- read_excel("Poverty_index.xlsx")
  Poverty$MPIO <- as.character(Poverty$MPIO)
  
  # Remove unnecessary columns
  
  Poverty$`Código Departamento` <- NULL
  Poverty$Municipio <- NULL

  # Add to the main data frame
  
  FDF <- merge(FDF, Poverty, by="MPIO")
  
# Read women heads of households data
  
  W_head_house <- read_excel("Woman_head_households.xlsx")
  W_head_house$MPIO <- as.character(W_head_house$MPIO)
  
  # Add 0 to municipality id's with only 4 digits
  
  W_head_house$MPIO <- ifelse(str_length(W_head_house$MPIO) == 4, paste(0, W_head_house$MPIO, sep = ""), W_head_house$MPIO)
  
  # Remove unnecessary columns
  
  W_head_house$Municipio <- NULL
  W_head_house$Type <- NULL

  # Add to the main data frame
  
  FDF <- merge(FDF, W_head_house, by="MPIO")
  
# Saving
  
save(FDF, file="Fem-Data.RData")

# Load main data frame

# load(file="Fem-Data.RData")
