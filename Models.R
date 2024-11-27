
# Libraries

  # Fitting geographically weighted regression models
  library(spgwr)

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
  
  # To create a contiguity-based spatial weights and test for spatial autocorrelation
  library(spdep)
  
  # To produce an organized table of results
  library(flextable)
  
  # Goodness of fit measures
  library(stats)
  
  # Obtain correlation coefficients
  library(corrplot)
  
# Set of variables
  
  Dat <- Mun_shp[-c(16:20,30,31)]
  
# Delete San Andres Municipality
  
  #Dat <- Dat[-c(1087,1088),]
  
# Square root transformation of the BE rate of femicides
  
  Dat$Sqr_EB_Fem_Rate <- sqrt(Dat$EB_Fem_Rate)
  
# ensure geometry is valid
Dat = sf::st_make_valid(Dat)

# A global linear regression model
  
  # attach data
  attach(Dat)
  
  # specify a model equation
  eq1 <- Sqr_EB_Fem_Rate ~ M_Homicide + W_Non_lethal_V + W_Sex_assault + Under_18 +
                          W_teen_birth_r + School_cov + Single_mother_head_house
  L_mod <- lm(formula = eq1, data = Dat)
  
  # estimates
  summary(L_mod)
  # Organized table of results
  L_mod %>% as_flextable()
  
  # Analysis of multicollinearity
  
  eq2 <- Sqr_EB_Fem_Rate ~ M_Homicide + W_Non_lethal_V + W_Sex_assault + Under_18 +
    W_teen_birth_r + School_cov + Single_mother_head_house + Domestic_V +
    Poverty_index
  L_mod <- lm(formula = eq2, data = Dat)
  vif(L_mod)
  
  # Plot of the model residuals
  
  Dat$res_lm <- residuals(L_mod)
  
  pdf(file="Residuals_OLS_1.pdf", width=15,height=12)
  
  tm_shape(Dat) +
        tm_fill(col = "res_lm", title = "OLS residuals", palette = "-RdBu", style = "cont") + # add fill
        tm_borders() +
        tm_layout(legend.title.size = 2, frame = TRUE, legend.position = c("right", "top"),
              legend.text.size = 2)

  dev.off()

  pdf(file="Residuals_OLS_2.pdf", width=15,height=12)
  
  legend_title = expression("OLS residuals")
  tm_shape(Dat) +
    tm_fill(col = "res_lm", title = legend_title, palette = "RdBu", style = "cont") + # add fill
    tm_layout(bg.color = "white") # change background colour
  
  dev.off()
  
  # Spatial Autocorrelation test in the residuals
  
    # Creating Spatial queen contiguity weights Matrices:
    
    Dat.nb <- poly2nb(Dat, queen = TRUE)
    Dat.w <- nb2listw(Dat.nb)
    
    # Male homicide rates
    # Global Moran’s I test
    
    moran.test(Dat$res_lm, Dat.w)
    lm.morantest(L_mod, Dat.w)

  # Diagnostics of the Normality assumption:
  
    ggplot(data = Dat) + 
      +     geom_histogram(mapping = aes(x=res_lm)) +
      +     xlab("OLS residuals")
    
    qqPlot(Dat$res_lm)
  
  # Goodness of fit measures  
    # Akaike Information Criterion
    AIC(L_mod)
    
    # Bayesian Information Criterion
    BIC(L_mod)
  
# A Geographically Weighted Regression with Adaptive Bandwidth
  
  # find optimal kernel bandwidth using cross validation
  abw <- gwr.sel(eq1, 
                 data = Dat, 
                 coords=cbind( Long, Lat),
                 longlat = TRUE,
                 adapt = TRUE, 
                 gweight = gwr.bisquare, 
                 verbose = FALSE,
                 method = "aic")
  
  # view selected bandwidth
  abw
  
  # fit a gwr based on adaptive bandwidth
  ab_gwr <- gwr(eq1, 
                data = Dat,
                coords=cbind( Long, Lat),
                longlat = TRUE,
                adapt = abw, 
                gweight = gwr.bisquare,
                hatmatrix=TRUE, 
                se.fit=TRUE)
  
  ab_gwr
  
  # Quasi-global R2
  
  qGlobalR2 <- (1 - (ab_gwr$results$rss/ab_gwr$gTSS))
  qGlobalR2
  
  # write gwr output into a data frame
  ab_gwr_out <- as.data.frame(ab_gwr$SDF)

  Dat$GWR1_localR2 <- ab_gwr_out$localR2
  
  Dat$GWR1_localR2 <- ifelse(Dat$GWR1_localR2 < 0, -Dat$GWR1_localR2,  Dat$GWR1_localR2)
  
  # map Local R2
    
  pdf(file="GWR1_Local_R2.pdf", width=15,height=12)
  
  tm_shape(Dat) +
    tm_fill(col = "GWR1_localR2", title = "Local R2", palette = "Blues", style = "cont") + # add fill
    tm_borders() +
    tm_layout(legend.title.size = 1.7, frame = TRUE, legend.position = c("right", "top"),
              legend.text.size = 1.7)
  
  dev.off()
  
  # R2 values above 0.6 
  table(Dat$GWR1_localR2 >= 0.6)
  
  # Extract the estimated local coefficients for the explanatory variable
  
    Dat$GWR1_M_Homicide <- ab_gwr_out$M_Homicide
    Dat$GWR1_W_Non_lethal_V <- ab_gwr_out$W_Non_lethal_V
    Dat$GWR1_W_Sex_assault <- ab_gwr_out$W_Sex_assault
    Dat$GWR1_Under_18 <- ab_gwr_out$Under_18
    Dat$GWR1_W_teen_birth_r <- ab_gwr_out$W_teen_birth_r
    Dat$GWR1_School_cov <- ab_gwr_out$School_cov
    Dat$GWR1_Single_mother_head_house <- ab_gwr_out$Single_mother_head_house

    # Male homicidea
    map1 = tm_shape(Dat) +
      tm_polygons(col = "GWR1_M_Homicide", style = "cont",palette = "-RdBu", 
                  title = "Est. local coefficient") +
      tm_layout(main.title = "(a) Male homicide rates",  
                main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                legend.text.size = 1.5, legend.title.size = 1.5)
    
    # Rates of non-lethal violence against women
    
    map2 = tm_shape(Dat) +
      tm_polygons(col = "GWR1_W_Non_lethal_V", style = "cont",palette = "-RdBu", 
                  title = "Est. local coefficient") +
      tm_layout(main.title = "(b) Women non-lethal violence rate",  
                main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                legend.text.size = 1.5, legend.title.size = 1.5)
    
    # Rate of sexually assaulted women
    
    map4 = tm_shape(Dat) +
      tm_polygons(col = "GWR1_W_Sex_assault", style = "cont",palette = "-RdBu", 
                  title = "Est. local coefficient") +
      tm_layout(main.title = "(c) Sexually assaulted women rate",  
                main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                legend.text.size = 1.5, legend.title.size = 1.5)
    
    # Rate of children under 18 years of age
    
    map5 = tm_shape(Dat) +
      tm_polygons(col = "GWR1_Under_18", style = "cont",palette = "-RdBu", 
                  title = "Est. local coefficient") +
      tm_layout(main.title = "(d) % children under 18",  
                main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                legend.text.size = 1.5, legend.title.size = 1.5)
    
    # Rate of adolescent pregnant women
    
    map6 = tm_shape(Dat) +
      tm_polygons(col = "GWR1_W_teen_birth_r", style = "cont",palette = "-RdBu", 
                  title = "Est. local coefficient") +
      tm_layout(main.title = "(e) Teen birth rate",  
                main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                legend.text.size = 1.5, legend.title.size = 1.5) 
    
    # Net school coverage rate
    
    map7 = tm_shape(Dat) +
      tm_polygons(col = "GWR1_School_cov", style = "cont",palette = "-RdBu", 
                  title = "Est. local coefficient") +
      tm_layout(main.title = "(f) School coverage rate",  
                main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                legend.text.size = 1.5, legend.title.size = 1.5) 
    
    # percentage of households headed by single mothers 
    
    map9 = tm_shape(Dat) +
      tm_polygons(col = "GWR1_Single_mother_head_house", style = "cont",palette = "-RdBu", 
                  title = "Est. local coefficient") +
      tm_layout(main.title = "(g) % single mothers households",  
                main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                legend.text.size = 1.5, legend.title.size = 1.5)
    
      # Plot local coefficients
        
      pdf(file="local_coefficients_7_Variables.pdf", width=24,height=36)
      
      tmap_arrange(map1, map2, map4, map5, map6, map7, map9, ncol = 3)
      
      dev.off()
      
      pdf(file="Local_coefficients_Variables_1.pdf", width=24,height=11)
      
      tmap_arrange(map1, map2, map4, ncol = 3)
      
      dev.off()
      
      pdf(file="Local_coefficients_Variables_2.pdf", width=24,height=11)
      
      tmap_arrange(map5, map6, map7, ncol = 3)
      
      dev.off()
      
      pdf(file="Local_coefficients_Variables_3.pdf", width=8,height=11)
      
      map9
      
      dev.off()
    
    # Assessing statistical significance

      # compute t statistic for Male homicide
      Dat$t_M_Homicide = ab_gwr_out$M_Homicide / ab_gwr_out$M_Homicide_se

        # categorise t values
        Dat$t_M_Homicide_cat <- cut(Dat$t_M_Homicide,
                                 breaks=c(min(Dat$t_M_Homicide)-1, -1.96, 1.96, max(Dat$t_M_Homicide)+1),
                                 labels=c("sig","nonsig", "sig"))
        
      # compute t statistic for non-lethal violence against women
      Dat$t_W_Non_lethal_V = ab_gwr_out$W_Non_lethal_V / ab_gwr_out$W_Non_lethal_V_se
    
        # categorise t values
        Dat$t_W_Non_lethal_V_cat <- cut(Dat$t_W_Non_lethal_V,
                                 breaks=c(min(Dat$t_W_Non_lethal_V)-1, -1.96, 1.96, max(Dat$t_W_Non_lethal_V)+1),
                                 labels=c("sig","nonsig", "sig"))

      # compute t statistic for sexually assaulted women
      Dat$t_W_Sex_assault = ab_gwr_out$W_Sex_assault / ab_gwr_out$W_Sex_assault_se
    
        # categorise t values
        Dat$t_W_Sex_assault_cat <- cut(Dat$t_W_Sex_assault,
                                 breaks=c(min(Dat$t_W_Sex_assault)-1, -1.96, 1.96, max(Dat$t_W_Sex_assault)+1),
                                 labels=c("sig","nonsig", "sig"))   

      # compute t statistic for children under 18 years of age
      Dat$t_Under_18 = ab_gwr_out$Under_18 / ab_gwr_out$Under_18_se
    
        # categorise t values
        Dat$t_Under_18_cat <- cut(Dat$t_Under_18,
                                 breaks=c(min(Dat$t_Under_18)-1, -1.96, 1.96, max(Dat$t_Under_18)+1),
                                 labels=c("sig","nonsig", "sig"))      
        
      # compute t statistic for Teen birth rates
      Dat$t_W_teen_birth_r = ab_gwr_out$W_teen_birth_r / ab_gwr_out$W_teen_birth_r_se
    
        # categorise t values
        Dat$t_W_teen_birth_r_cat <- cut(Dat$t_W_teen_birth_r,
                                 breaks=c(min(Dat$t_W_teen_birth_r)-1, -1.96, 1.96, max(Dat$t_W_teen_birth_r)+1),
                                 labels=c("sig","nonsig", "sig"))   
        
      # compute t statistic for School coverage
      Dat$t_School_cov = ab_gwr_out$School_cov / ab_gwr_out$School_cov_se
    
        # categorise t values
        Dat$t_School_cov_cat <- cut(Dat$t_School_cov,
                                 breaks=c(min(Dat$t_School_cov)-1, -1.96, 1.96, max(Dat$t_School_cov)+1),
                                 labels=c("sig","nonsig", "sig"))  
        
      # compute t statistic for single mothers
      Dat$t_Single_mother_head_house = ab_gwr_out$Single_mother_head_house / ab_gwr_out$Single_mother_head_house_se
    
        # categorise t values
        Dat$t_Single_mother_head_house_cat <- cut(Dat$t_Single_mother_head_house,
                                 breaks=c(min(Dat$t_Single_mother_head_house)-1, -1.96, 1.96, max(Dat$t_Single_mother_head_house)+1),
                                 labels=c("sig","nonsig", "sig"))  
        
      # map statistically significant coefs
      
      # Male homicides
        
      Dat$GWR1_M_Homicide[Dat$t_M_Homicide_cat == "nonsig"] <- NaN
      
      map1S = tm_shape(Dat) +
        tm_polygons(col = "GWR1_M_Homicide", style = "cont",palette = "-RdBu", 
                    title = "Est. local coefficient", textNA = "Not Significant") +
        tm_layout(main.title = "(a) Male homicide rates",  
                  main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                  legend.text.size = 1.5, legend.title.size = 1.5)
      
      # Rates of non-lethal violence against women
      
      Dat$GWR1_W_Non_lethal_V[Dat$t_W_Non_lethal_V_cat == "nonsig"] <- NaN
      
      map2S = tm_shape(Dat) +
        tm_polygons(col = "GWR1_W_Non_lethal_V", style = "cont",palette = "-RdBu", 
                    title = "Est. local coefficient", textNA = "Not Significant") +
        tm_layout(main.title = "(b) Women non-lethal violence rate",  
                  main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                  legend.text.size = 1.5, legend.title.size = 1.5)
      
      # Rate of sexually assaulted women
      
      Dat$GWR1_W_Sex_assault[Dat$t_W_Sex_assault_cat == "nonsig"] <- NaN
      
      map4S = tm_shape(Dat) +
        tm_polygons(col = "GWR1_W_Sex_assault", style = "cont",palette = "-RdBu", 
                    title = "Est. local coefficient", textNA = "Not Significant") +
        tm_layout(main.title = "Sexually assaulted women rate",  
                  main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                  legend.text.size = 1.5, legend.title.size = 1.5)
      
      # Rate of children under 18 years of age
      
      Dat$GWR1_Under_18[Dat$t_Under_18_cat == "nonsig"] <- NaN
      
      map5S = tm_shape(Dat) +
        tm_polygons(col = "GWR1_Under_18", style = "cont",palette = "-RdBu", 
                    title = "Est. local coefficient", textNA = "Not Significant") +
        tm_layout(main.title = "(a) % children under 18",  
                  main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                  legend.text.size = 1.5, legend.title.size = 1.5)
      
      # Rate of adolescent pregnant women
      
      Dat$GWR1_W_teen_birth_r[Dat$t_W_teen_birth_r_cat == "nonsig"] <- NaN
      
      map6S = tm_shape(Dat) +
        tm_polygons(col = "GWR1_W_teen_birth_r", style = "cont",palette = "-RdBu", 
                    title = "Est. local coefficient", textNA = "Not Significant") +
        tm_layout(main.title = "(b) Teen birth rate",  
                  main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                  legend.text.size = 1.5, legend.title.size = 1.5)
      
      # Net school coverage rate
      
      Dat$GWR1_School_cov[Dat$t_School_cov_cat == "nonsig"] <- NaN
      
      map7S = tm_shape(Dat) +
        tm_polygons(col = "GWR1_School_cov", style = "cont",palette = "-RdBu", 
                    title = "Est. local coefficient", textNA = "Not Significant") +
        tm_layout(main.title = "(c) School coverage rate",  
                  main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                  legend.text.size = 1.5, legend.title.size = 1.5)
      
      # percentage of households headed by single mothers 
      
      Dat$GWR1_Single_mother_head_house[Dat$t_Single_mother_head_house_cat == "nonsig"] <- NaN
      
      map9S = tm_shape(Dat) +
        tm_polygons(col = "GWR1_Single_mother_head_house", style = "cont",palette = "-RdBu", 
                    title = "Est. local coefficient", textNA = "Not Significant") +
        tm_layout(main.title = "(c) % single mothers households",  
                  main.title.size = 2.5, frame = FALSE, legend.position = c("right", "top"),
                  legend.text.size = 1.5, legend.title.size = 1.5)
      
      # Plot local coefficients and local statistical significance
          
      #pdf(file="statistical_signif_Variables_1.pdf", width=24,height=36)
      
      #tmap_arrange(map1S, map2S, map4S, map5S, map6S, map7S, map9S, ncol = 3)
      
      #dev.off()
      
      pdf(file="Local_signif_coeff_1.pdf", width=24,height=11)
      
      tmap_arrange(map1S, map2S, map9S, ncol = 3)
      
      dev.off()
      
      pdf(file="Local_signif_coeff_2.pdf", width=24,height=11)
      
      tmap_arrange(map5S, map6S, map7S, ncol = 3)
      
      dev.off()
      
      pdf(file="Local_signif_coeff_3.pdf", width=8,height=11)
      
      map4S
      
      dev.off()
      
      # Male homicide rate - statistical significance table
      table(Dat$t_W_Sex_assault_cat)
      table(Dat$t_W_teen_birth_r_cat)
      
  # Local Multicollinearity
    
    # Subset of variables
      
    df_sel <- ab_gwr_out[,2:9]
    
      # Change names of variables
  
    names(df_sel) <- c("Intercept", "Homicides", "Non-lethal violence",  
                     "Sexual violence", "Underage", "Teen births", "School coverage",
                     "Single mothers")

    # obtain a matrix of Pearson and Spearman correlations between the coefficients produced by the GWR
    
    Pear_cor <- round(cor(df_sel, use="complete.obs", method="pearson"), 2)
    Spear_cor <- round(cor(df_sel, use="complete.obs", method="spearman"), 2)
    
    # Pearson correlation matrix
    
    corrplot(Pear_cor, method = 'number', type = 'lower', diag = FALSE, col = COL2('RdBu', 10),
             tl.col = "black")
    corrplot(Pear_cor, method = 'number', type = 'lower', diag = FALSE, 
             col = brewer.pal(n = 10, name = "RdBu"), tl.col = "black")
    
    # Spearman correlation matrix
    
    corrplot(Spear_cor, method = 'number', type = 'lower', diag = FALSE, col = COL2('RdBu', 10),
             tl.col = "black")
    corrplot(Spear_cor, method = 'number', type = 'lower', diag = FALSE, 
             col = brewer.pal(n = 10, name = "RdBu"), tl.col = "black", )
    
    # correlations visually
    
    pairs(ab_gwr_out[,2:9], pch=".")

  # Plot of the model residuals
    
    Dat$res_GWR <- ab_gwr_out$gwr.e
        
    pdf(file="Residuals_GWR.pdf", width=15,height=12)
    
    tm_shape(Dat) +
      tm_fill(col = "res_GWR", title = "GWR residuals", palette = "-RdBu", style = "cont") + # add fill
      tm_borders() +
      tm_layout(legend.title.size = 2, frame = TRUE, legend.position = c("right", "top"),
                legend.text.size = 2)
    
    dev.off()
    
    # Spatial Autocorrelation test in the residuals
    
    # Creating Spatial queen contiguity weights Matrices:
    
    Dat.nb <- poly2nb(Dat, queen = TRUE)
    Dat.w <- nb2listw(Dat.nb)
    
    # Male homicide rates
    # Global Moran’s I test
    
    moran.test(Dat$res_GWR, Dat.w)

# tests comparing OLS and GWR models
    
  # Test 1
  
  BFC02.gwr.test(ab_gwr)
    
  # Test 2
  
  #BFC99.gwr.test(ab_gwr)
  
  # Test 3
  
  #LMZ.F1GWR.test(ab_gwr)
  
  # Test 4
  
  #LMZ.F2GWR.test(ab_gwr)
  
  # Test 5
  
  LMZ.F3GWR.test(ab_gwr)
  

  

  
