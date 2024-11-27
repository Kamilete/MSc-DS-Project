
# Libraries

  library(stringr)

  # Simple features (a standardised way to encode vector data ie. points, lines, polygons)
  library(sf)
  
  # To create a contiguity-based spatial weights
  library(spdep)
  
  # Other libraries
  library(tidyverse)
  
  # Thematic maps
  library(tmap)
  
  # To map a centroid function of the geometry column of an sf data structure
  library(purrr)

# Read shapefiles
  
  # Read multipolygon data per municipalities
  Mun_shp <- st_read("col_admbnda_adm2_mgn_20200416.shp")
  
  # Read latitude and longitude data per municipalities
  Mun_shp2 <- st_read("col_admbndp_admALL_mgn_itos_20200416.shp")
  
  # Delete the first 2 letters of the municipality PCODE
  
  Mun_shp$ADM2_PCODE <- str_sub(Mun_shp$ADM2_PCODE, start = 3, end = 7)
  Mun_shp2$ADM2_PCODE <- str_sub(Mun_shp2$ADM2_PCODE, start = 3, end = 7)
  
# Add latitude and longitude to the main data frame
  
  FDF <- merge(FDF, Mun_shp2[,c(6,10,11)], by.x="MPIO", by.y="ADM2_PCODE")
  FDF$geometry <- NULL
  
  # Set latitude and longitude column names
  
  names(FDF)[20] <- "Long"
  names(FDF)[21] <- "Lat"
  
# Merge the main data frame to the multipolygon shapefile
 
  Mun_shp <- merge(Mun_shp, FDF, by.x="ADM2_PCODE", by.y="MPIO")
  
# Delete San Andres Municipality
  
Mun_shp <- Mun_shp[-c(1087,1088),]
  
# Saving
  
#st_write(Mun_shp, "Mun_Col.shp")

# Spatial Empirical Bayes smoothing

  # Creating Spatial queen contiguity weights Matrices:

  Mun_shp.nb <- poly2nb(Mun_shp, queen = TRUE)
  summary(Mun_shp.nb)
  
    # To visualize the neighbors (adjacent) areas:
  
    centroids <- st_centroid(st_geometry(Mun_shp))
    plot(st_geometry(Mun_shp), border = "grey60", reset = FALSE)
    plot(Mun_shp.nb, coords = centroids, add=T, col = "red")
  
  # Creating cumulative higher order spatial queen contiguity weights Matrices:
    
    # create a queen contiguity function
    
    st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
    
    # to convert the resulting data structure from the st_relate function to nb.
    
    as.nb.sgbp <- function(x, ...) {
      attrs <- attributes(x)
      x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
      attributes(x) <- attrs
      class(x) <- "nb"
      x
    }
    
    # Now we use our st_queen function to get another sgbp neighbor list
    queen.sgbp <- st_queen(Mun_shp)
    
    # To convert to type nb
    queen.nb <- as.nb.sgbp(queen.sgbp)
    
    # To compute the second order neighbors
    second.order.queen <- nblag(queen.nb, 2)
    
    # To include both 1st and 2nd order neighbors
    second.order.queen.cumul <- nblag_cumul(second.order.queen)
    
      # To visualize the both second and first order neighbors:
    
      longitude <- map_dbl(Mun_shp$geometry, ~st_centroid(.x)[[1]])
      latitude <- map_dbl(Mun_shp$geometry, ~st_centroid(.x)[[2]])
      center.coords <- cbind(longitude, latitude)
    
      plot(st_geometry(Mun_shp), border = "grey60", reset = FALSE)
      plot(second.order.queen.cumul, coords = center.coords, col = "red", add = TRUE)
    
  # Creating k-nearest neighbors weights Matrices:
  
  longitude <- map_dbl(Mun_shp$geometry, ~st_centroid(.x)[[1]])
  latitude <- map_dbl(Mun_shp$geometry, ~st_centroid(.x)[[2]])
  center.coords <- cbind(longitude, latitude)
  
  k37 <- knn2nb(knearneigh(center.coords, k = 37))
  
    # To visualize the neighbors (adjacent) areas:
  
    plot(st_geometry(Mun_shp), border = "grey60", reset = FALSE)
    plot(k37, coords = center.coords, col = "red", add = TRUE)

  # Spatial local EB rate smoother

  eb_Fem_rate <- EBlocal(Mun_shp$Avg_Femicides, Mun_shp$Avg_Women, k37, geoda = TRUE)
  
  # EB rate per 100.000
  
  Mun_shp$EB_Fem_Rate <- eb_Fem_rate$est * 100000
  
  # Replace NA's with zeros
  #Mun_shp$EB_Fem_Rate[is.na(Mun_shp$EB_Fem_Rate)] <- 0
  
  # Proportion of zero EB rates
  #table(Mun_shp$EB_Fem_Rate == 0)
  
  # Plot ratio of means vs EB rate with means
  
  mpa1 = tm_shape(Mun_shp) +
    tm_fill(col = "EB_Fem_Rate", title = "per 100.000 women", palette = "-RdBu", style = "cont") + # add fill
    tm_borders() +
    tm_layout(main.title = "EB smoothed Femicide rate",  
              main.title.size = 2.5, legend.title.size = 2, frame = TRUE, legend.position = c("right", "top"),
              legend.text.size = 2)
  
  mpa2 = tm_shape(Mun_shp) +
    tm_fill(col = "Rate_Avg_Fem", title = "per 100.000 women", palette = "-RdBu", style = "cont") + # add fill
    tm_borders() +
    tm_layout(main.title = "Raw Femicide rate",  
              main.title.size = 2.5, legend.title.size = 2, frame = TRUE, legend.position = c("right", "top"),
              legend.text.size = 2)
  
  pdf(file="EB_Rate_vs_Raw_Rate.pdf", width=20,height=12)
  
  tmap_arrange(mpa2, mpa1, ncol = 2)
  
  dev.off()
  
# Square root transformation of the BE rate of femicides
  
  Mun_shp$Sqr_EB_Fem_Rate <- sqrt(Mun_shp$EB_Fem_Rate)
  
  # Plot ratio of means vs log EB rate with means
  
  mpa2 = tm_shape(Mun_shp) +
    tm_fill(col = "Rate_Avg_Fem", title = "per 100.000 women", palette = "-RdBu", style = "cont") + # add fill
    tm_borders() +
    tm_layout(main.title = "Raw Femicide rate",  
              main.title.size = 2.5, legend.title.size = 2, frame = TRUE, legend.position = c("right", "top"),
              legend.text.size = 2)
  
  mpa3 = tm_shape(Mun_shp) +
    tm_fill(col = "Sqr_EB_Fem_Rate", title = "per 100.000 women", palette = "-RdBu", style = "cont") + # add fill
    tm_borders() +
    tm_layout(main.title = "Log EB smoothed Femicide rate",  
              main.title.size = 2.5, legend.title.size = 2, frame = TRUE, legend.position = c("right", "top"),
              legend.text.size = 2)
  
  pdf(file="Log_EB_Rate_vs_Raw_Rate.pdf", width=20,height=12)
  
  tmap_arrange(mpa2, mpa3, ncol = 2)
  
  dev.off()
  
# Global Moran's index
  
  # Creating Spatial queen contiguity weights Matrices:
  
  Mun_shp.nb <- poly2nb(Mun_shp, queen = TRUE)
  Mun_shp.w <- nb2listw(Mun_shp.nb)
  
  # Male homicide rates
  # Global Moran’s I test
  
  moran.test(Mun_shp$M_Homicide, Mun_shp.w)

  # Rates of non-lethal violence against women
  # Global Moran’s I test
  
  moran.test(Mun_shp$W_Non_lethal_V, Mun_shp.w)
  
  # Rate of domestic violence
  # Global Moran’s I test
  
  moran.test(Mun_shp$Domestic_V, Mun_shp.w)
  
  # Rate of sexually assaulted women
  # Global Moran’s I test
  
  moran.test(Mun_shp$W_Sex_assault, Mun_shp.w)
  
  # Rate of children under 18 years of age
  # Global Moran’s I test
  
  moran.test(Mun_shp$Under_18, Mun_shp.w)
  
  # Rate of adolescent pregnant women
  # Global Moran’s I test
  
  moran.test(Mun_shp$W_teen_birth_r, Mun_shp.w)
  
  # Net school coverage rate
  # Global Moran’s I test
  
  moran.test(Mun_shp$School_cov, Mun_shp.w)
  
  # percentage of households headed by single mothers 
  # Global Moran’s I test
  
  moran.test(Mun_shp$Woman_head_house, Mun_shp.w)
  
  # Poverty 
  # Global Moran’s I test
  
  moran.test(Mun_shp$Poverty_index, Mun_shp.w)
  
  

  
