
# Libraries

  # Obtain correlation coefficients
  library(corrplot)
  
  # Assess multicollinearity
  library(car)
  
  # Simple features (a standardised way to encode vector data ie. points, lines, polygons)
  library(sf)
  
  # Thematic maps
  library(tmap) 
  
  # Colour palettes
  library(RColorBrewer) 
  
  # More colour palettes
  library(viridis) # nice colour schemes
  
  # to arrange plots in matrix form
  library(gridExtra)
  
# ensure geometry is valid
Mun_shp = sf::st_make_valid(Mun_shp)

# Exploratory Analysis of EB femicide rates
  
# Kernel density and Histogram of femicide rates
  
  ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = EB_Fem_Rate)) +
    theme_classic()
  
  ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = EB_Fem_Rate)) +
    theme_classic()
  
  qqnorm(Mun_shp$EB_Fem_Rate)
  qqline(Mun_shp$EB_Fem_Rate, col = 2)
  
  ggplot(data = Mun_shp, aes(x = EB_Fem_Rate)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1) + 
    geom_density(colour="red", size=1,  adjust=1)
  
  # Kernel density and Histogram of square root of femicide rates
  
  ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = sqrt(EB_Fem_Rate))) +
    theme_classic()
  
  ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = sqrt(EB_Fem_Rate))) +
    theme_classic()
  
  ggplot(data = Mun_shp, aes(x = sqrt(EB_Fem_Rate))) +
    geom_histogram(aes(y = ..density..), binwidth = 0.1) + 
    geom_density(colour="red", size=1,  adjust=1)
  
  qqnorm(log(Mun_shp$EB_Fem_Rate))
  qqline(log(Mun_shp$EB_Fem_Rate), col = 2)
  
  # Square root transformation of the BE rate of femicides
  
  Mun_shp$Sqr_EB_Fem_Rate <- sqrt(Mun_shp$EB_Fem_Rate)
  
# Plot Square root EB rate

  pdf(file="EB_smooth_Femicides_1.pdf", width=15,height=12)
  
  tm_shape(Mun_shp) +
    tm_fill(col = "Sqr_EB_Fem_Rate", title = "per 100.000 women", palette = "-RdBu", style = "cont") + # add fill
    tm_borders() +
    tm_layout(main.title = "Log EB smoothed Femicide rate",  
              main.title.size = 2.2, legend.title.size = 1.7, frame = TRUE, legend.position = c("right", "top"),
              legend.text.size = 1.7)
  
  dev.off()

# Tukey’s ladder of transformations
  
  symbox(~EB_Fem_Rate, powers=seq(-3,3,by=.5))
  # Best transformation: the square root
  symbox(~EB_Fem_Rate, powers=seq(0.4,0.7,by=.05))
  # Best transformation: the power of 0.55
    
    # Kernel density and Histogram 
    ggplot(data = Mun_shp, aes(x = (EB_Fem_Rate^0.55))) +
      geom_histogram(aes(y = ..density..), binwidth = 0.1) + 
      geom_density(colour="red", size=1,  adjust=1)
    
    # power of 0.55 transformation of the BE rate of femicides
    
    Mun_shp$pow55_EB_Fem_Rate <- Mun_shp$EB_Fem_Rate^0.55
  
# Descriptive statistics
  summary(Mun_shp[c(21:23, 25:29, 32, 36, 37)])
  
# Histograms and Kernel densities
  
  myboxplot = list()
  myDKplot = list()
  
  # Male homicide rates

  myboxplot[[1]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = M_Homicide)) +
    theme_classic()
  
  myDKplot[[1]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = M_Homicide)) +
    theme_classic()
  
  # Rates of non-lethal violence against women
  
  myboxplot[[2]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = W_Non_lethal_V)) +
    theme_classic()
  
  myDKplot[[2]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = W_Non_lethal_V)) +
    theme_classic()
  
  # Rate of domestic violence

  myboxplot[[3]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = Domestic_V)) +
    theme_classic()
  
  myDKplot[[3]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = Domestic_V)) +
    theme_classic()
  
  # Rate of sexually assaulted women

  myboxplot[[4]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = W_Sex_assault)) +
    theme_classic()
  
  myDKplot[[4]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = W_Sex_assault)) +
    theme_classic()
  
  # Rate of children under 18 years of age

  myboxplot[[5]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = Under_18)) +
    theme_classic()
  
  myDKplot[[5]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = Under_18)) +
    theme_classic()

  # Rate of adolescent pregnant women

  myboxplot[[6]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = W_teen_birth_r)) +
    theme_classic()
  
  myDKplot[[6]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = W_teen_birth_r)) +
    theme_classic()

  # Net school coverage rate

  myboxplot[[7]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = School_cov)) +
    theme_classic()
  
  myDKplot[[7]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = School_cov)) +
    theme_classic()
  
  # percentage of households headed by single mothers 

  myboxplot[[8]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = Single_mother_head_house)) +
    theme_classic()
  
  myDKplot[[8]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = Single_mother_head_house)) +
    theme_classic()

  # Poverty 

  myboxplot[[9]] = ggplot(data = Mun_shp) +
    geom_histogram(alpha=0.8, colour="black", fill="lightblue", aes(x = Poverty_index)) +
    theme_classic()
  
  myDKplot[[9]] = ggplot(data = Mun_shp) +
    geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = Poverty_index)) +
    theme_classic()
  
  # Plot of histrograms
  
  grid.arrange(myboxplot[[1]], myboxplot[[2]], myboxplot[[3]],
               myboxplot[[4]], myboxplot[[5]], myboxplot[[6]],
               myboxplot[[7]], myboxplot[[8]], myboxplot[[9]], ncol = 3)
  
  # Plot of kernels
  
  grid.arrange(myDKplot[[1]], myDKplot[[2]], myDKplot[[3]],
               myDKplot[[4]], myDKplot[[5]], myDKplot[[6]],
               myDKplot[[7]], myDKplot[[8]], myDKplot[[9]], ncol = 3)
  
# Spatial distribution maps
  
  # Male homicide rates
  
  map1 = tm_shape(Mun_shp) +
          tm_polygons(col = "M_Homicide", style = "jenks",palette = "Reds", 
                        title = "per 100.000 male") +
          tm_layout(main.title = "(a) Male homicide rates",  
                      main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                      legend.text.size = 1.5, legend.title.size = 1.5)
  
  # Rates of non-lethal violence against women
  
  map2 = tm_shape(Mun_shp) +
          tm_polygons(col = "W_Non_lethal_V", style = "jenks",palette = "Reds", 
                title = "per 100.000 women") +
          tm_layout(main.title = "(b) Women non-lethal violence rate",  
              main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
              legend.text.size = 1.5, legend.title.size = 1.5)
  
  # Rate of domestic violence
  
  map3 = tm_shape(Mun_shp) +
          tm_polygons(col = "Domestic_V", style = "jenks",palette = "Reds", 
                      title = "per 100.000 inhabitants") +
          tm_layout(main.title = "(c) Domestic violence rate",  
                    main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                    legend.text.size = 1.5, legend.title.size = 1.5)
  
  # Rate of sexually assaulted women
  
  map4 = tm_shape(Mun_shp) +
          tm_polygons(col = "W_Sex_assault", style = "jenks",palette = "Reds", 
                      title = "per 100.000 women") +
          tm_layout(main.title = "(a) Sexually assaulted women rate",  
                    main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                    legend.text.size = 1.5, legend.title.size = 1.5)

  # Rate of children under 18 years of age
  
  map5 = tm_shape(Mun_shp) +
          tm_polygons(col = "Under_18", style = "jenks",palette = "Reds", 
                      title = "Percentage") +
          tm_layout(main.title = "(c) Children under 18",  
                    main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                    legend.text.size = 1.5, legend.title.size = 1.5)
  
  # Rate of adolescent pregnant women
  
  map6 = tm_shape(Mun_shp) +
          tm_polygons(col = "W_teen_birth_r", style = "jenks",palette = "Reds", 
                      title = "per 1.000 teen women") +
          tm_layout(main.title = "(a) Teen birth rate",  
                    main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                    legend.text.size = 1.5, legend.title.size = 1.5) 
  
  # Net school coverage rate
  
  map7 = tm_shape(Mun_shp) +
          tm_polygons(col = "School_cov", style = "jenks",palette = "Reds", 
                      title = "Percentage") +
          tm_layout(main.title = "(b) School coverage rate",  
                    main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                    legend.text.size = 1.5, legend.title.size = 1.5) 
  
  # Poverty 
  
  map8 = tm_shape(Mun_shp) +
          tm_polygons(col = "Poverty_index", style = "jenks",palette = "Reds", 
                      title = "Percentage") +
          tm_layout(main.title = "(b) Poverty index",  
                    main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                    legend.text.size = 1.5, legend.title.size = 1.5)
  
  # percentage of households headed by single mothers 
  
  map9 = tm_shape(Mun_shp) +
    tm_polygons(col = "Single_mother_head_house", style = "jenks",palette = "Reds", 
                title = "Percentage") +
    tm_layout(main.title = "(c) Single mothers households",  
              main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
              legend.text.size = 1.5, legend.title.size = 1.5)
  
  # Plot spatial distribution maps
  
  pdf(file="Spatial_distribution_3_Variables_1.pdf", width=24,height=11)
  
  tmap_arrange(map4, map2, map3, ncol = 3)
  
  dev.off()
  
  pdf(file="Spatial_distribution_3_Variables_2.pdf", width=24,height=11)
  
  tmap_arrange(map1, map8, map5, ncol = 3)
  
  dev.off()
  
  pdf(file="Spatial_distribution_3_Variables_3.pdf", width=24,height=11)
  
  tmap_arrange(map6, map7, map9, ncol = 3)
  
  dev.off()
  
  pdf(file="Spatial_distribution_9_Variables_2.pdf", width=9,height=12)
    
  tmap_arrange(map1, map2, map3, map4, map5, map6, map7, map8, map9, ncol = 3)
    
  dev.off()
  
# correlogram
  
  # Subset of variables
  
  df_sel <- st_set_geometry(Mun_shp[,c(21:23, 25:29, 32)], NULL) # temporary data set removing geometries
  
  # Change names of variables
  
  names(df_sel) <- c("Homicides", "Non-lethal violence", "Domestic violence", 
                     "Sexual violence", "Underage", "Teen births", "School coverage",
                     "Poverty", "Single mothers")

  
  # obtain a matrix of Pearson and Spearman correlation coefficients
  
  Pear_cor <- round(cor(df_sel, use="complete.obs", method="pearson"), 2)
  Spear_cor <- round(cor(df_sel, use="complete.obs", method="spearman"), 2)
  
  # Pearson correlation matrix
  
  corrplot(Pear_cor, method = 'number', type = 'lower', diag = FALSE, col = COL2('RdBu', 10),
           tl.col = "black")
  corrplot(Pear_cor, method = 'number', type = 'lower', diag = FALSE, 
           col = brewer.pal(n = 10, name = "RdBu"), tl.col = "black")
  #corrplot(Pear_cor, method = 'number', type = 'lower', diag = FALSE, col = brewer.pal(n = 10, name = "PuOr"))
  
  # Spearman correlation matrix
  
  corrplot(Spear_cor, method = 'number', type = 'lower', diag = FALSE, col = COL2('RdBu', 10),
           tl.col = "black", tl.srt = 45)
  corrplot(Spear_cor, method = 'number', type = 'lower', diag = FALSE, 
           col = brewer.pal(n = 10, name = "RdBu"), tl.col = "black", )
  
# Map of Colombia
  
  myboxplot = list()
  
  library(ggmap)
  
  map <- get_map(c(left = -85, bottom = -10, right = -60, top = 18), source = "stamen", language = "en-EN", zoom = 5)
  plt1 <- ggmap(map) + ggtitle("(a) Location of Colombia") + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(plot.title=element_text(size=20)) 
  myboxplot[[1]] <- plt1

  map2 <- get_map(c(left = -80, bottom = -4.5, right = -66, top = 13), source = "stamen", language = "en-EN", zoom = 5, maptype = "watercolor")
  plt2 <- ggmap(map2) + ggtitle("(b) Municipalities of colombia") + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    geom_sf(data = Mun_shp, aes(fill=EB_Fem_Rate), inherit.aes = FALSE, fill=NA) +
    theme(plot.title=element_text(size=20)) 
  myboxplot[[2]] <- plt2
  

  pp <- grid.arrange(myboxplot[[1]], myboxplot[[2]], ncol = 2)
  
  ggsave(file="Colombia.pdf", pp, width = 15, height = 8)
  
  pdf(file="Colombia_1.pdf", width=24,height=36)
  
  plot(plt1)
  
  dev.off()

  pdf(file="Colombia_2.pdf", width=24,height=36)
  
  plot(plt2)
  
  dev.off()
  

  
  
