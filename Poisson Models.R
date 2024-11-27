
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

# Plotting
library("ggplot2")

# Set of variables

Dat <- Mun_shp[c(1:15,21:24,26:32,35:40)]

# Delete San Andres Municipality

Dat <- Dat[-c(1087,1088),]

# Exploratory Analysis of femicides

# Histogram femicides

ggplot(data = Dat) +
  geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = Femicides)) +
  theme_classic()

# distribution in numbers
summary(Dat$Femicides)

# ensure geometry is valid
Dat = sf::st_make_valid(Dat)

# map
legend_title = expression("Femicides")
tm_shape(Dat) +
  tm_fill(col = "Femicides", title = legend_title, palette = magma(256), style = "cont") + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_layout(bg.color = "white") # change background colour

# correlogram

# obtain a matrix of Pearson correlation coefficients
df_sel <- st_set_geometry(Dat[,15:27], NULL) # temporary data set removing geometries
cormat <- cor(df_sel, use="complete.obs", method="pearson")

# significance test
sig1 <- corrplot::cor.mtest(df_sel, conf.level = .95)

# creta a correlogram
corrplot::corrplot(cormat, type="lower",
                   method = "circle", 
                   order = "original", 
                   tl.cex = 0.7,
                   p.mat = sig1$p, sig.level = .05, 
                   col = viridis::viridis(100, option = "plasma"),
                   diag = FALSE)

# A global Poisson regression model

# attach data
attach(Dat)

# specify a model equation with log(exposure) as a offset in the regression.
eq1 <- Femicides ~ Rural + M_Homicide + W_Non_lethal_V + Domestic_V + W_Sex_assault +
  W_teen_birth_r + School_cov + Pob_misery + Single_mother_head_house + offset(log(Pop))
L_mod <- glm(formula = eq1, family = poisson, data = Dat)

# estimates
summary(L_mod)

# To test the null hypothesis that our Poisson regression model is correct\

pchisq(L_mod$deviance, nrow(Dat) - length(L_mod$coefficients), lower.tail = FALSE) 
# We reject our null hypothesis

# Analysis of multicollinearity

vif(L_mod)

# Plot of the model residuals

library(arm) # Gelman and Hill library

y_hat <- predict(L_mod, type = "response")

# plot of the raw residuals against predicted values:
residual <- y_hat - Dat$Femicides
sigma_hat <- sd(residual)
residual.plot(y_hat, residual, sigma_hat, main = expression(paste("raw residuals, ", y - hat(y))))

# Plot of the standardized residuals against predicted values:
sd_residual <- (y_hat - Dat$Femicides) / sqrt(y_hat)
residual.plot(y_hat, sd_residual, 2, main = expression(paste("standardized residuals, ", (y - hat(y)) / sqrt(hat(y)))))
# Indicates overdispersion.

# Overdispersion test. Under the null of a Poisson GLM, 
# the statistic below should be a chi-squared 

df <- nrow(Dat) - length(L_mod$coefficients)
chisq_police <- sum(sd_residual^2)
cat(sprintf("Observed/expected chi-squared under Poisson: [%.2f, %2.f]\n", chisq_police, df))
cat(sprintf("Estimated overdispersion: %.2f\n", chisq_police / df))
cat(sprintf("Upper tail probability: %.2f\n", pchisq(chisq_police, df, lower.tail = FALSE))) # Probability of a value that is as high or higher than the observed one
# Indicates overdispersion.

# specify a model equation with log(exposure) as a predictor in the regression.
eq2 <- Femicides ~ Rural + M_Homicide + W_Non_lethal_V + Domestic_V + W_Sex_assault +
  W_teen_birth_r + School_cov + Pob_misery + Single_mother_head_house + log(Pop)
L_mod2 <- glm(formula = eq2, family = poisson, data = Dat)

# estimates
summary(L_mod2)

# To test the null hypothesis that our Poisson regression model is correct\

pchisq(L_mod2$deviance, nrow(Dat) - length(L_mod2$coefficients), lower.tail = FALSE) 
# We reject our null hypothesis

# Analysis of multicollinearity

vif(L_mod2)

# Plot of the model residuals

library(arm) # Gelman and Hill library

y_hat <- predict(L_mod2, type = "response")

# plot of the raw residuals against predicted values:
residual <- y_hat - Dat$Femicides
sigma_hat <- sd(residual)
residual.plot(y_hat, residual, sigma_hat, main = expression(paste("raw residuals, ", y - hat(y))))

# Plot of the standardized residuals against predicted values:
sd_residual <- (y_hat - Dat$Femicides) / sqrt(y_hat)
residual.plot(y_hat, sd_residual, 2, main = expression(paste("standardized residuals, ", (y - hat(y)) / sqrt(hat(y)))))
# Indicates overdispersion.

# Overdispersion test. Under the null of a Poisson GLM, 
# the statistic below should be a chi-squared 

df <- nrow(Dat) - length(L_mod2$coefficients)
chisq_police <- sum(sd_residual^2)
cat(sprintf("Observed/expected chi-squared under Poisson: [%.2f, %2.f]\n", chisq_police, df))
cat(sprintf("Estimated overdispersion: %.2f\n", chisq_police / df))
cat(sprintf("Upper tail probability: %.2f\n", pchisq(chisq_police, df, lower.tail = FALSE))) # Probability of a value that is as high or higher than the observed one
# Indicates overdispersion

# A Geographically Weighted Poisson Regression with Fixed Bandwidth

# find optimal kernel bandwidth using cross validation
fbw <- ggwr.sel(eq1, 
               data = Dat, 
               coords=cbind( Long, Lat),
               longlat = TRUE,
               adapt=FALSE, 
               family=poisson(), 
               gweight = gwr.Gauss, 
               verbose = FALSE)

# view selected bandwidth
fbw

# fit a gwr model based on fixed bandwidth
fb_gwr <- ggwr(eq1, 
              data = Dat,
              coords=cbind( Long, Lat),
              longlat = TRUE,
              family=poisson(),
              bandwidth = fbw, 
              gweight = gwr.Gauss,
              type = "deviance")

fb_gwr

# write gwr output into a data frame
fb_gwr_out <- as.data.frame(fb_gwr$SDF)

Dat$fmb_localR2 <- fb_gwr_out$localR2

# map Local R2
legend_title = expression("Fixed: Local R2")
tm_shape(Dat) +
  tm_fill(col = "fmb_localR2", title = legend_title, palette = magma(256), style = "cont") + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_scale_bar(breaks = c(0,1,2), text.size = 0.7, position =  c("center", "bottom")) + # add scale bar
  tm_layout(bg.color = "white") # change background colour

# A Geographically Weighted Regression with Adaptive Bandwidth

# find optimal kernel bandwidth using cross validation
abw <- ggwr.sel(eq1, 
               data = Dat, 
               coords=cbind( Long, Lat),
               longlat = TRUE,
               adapt = TRUE, 
               family=poisson(), 
               gweight = gwr.Gauss, 
               verbose = FALSE)

# view selected bandwidth
abw

# fit a gwr based on adaptive bandwidth
ab_gwr <- ggwr(eq1, 
              data = Dat,
              coords=cbind( Long, Lat),
              longlat = TRUE,
              family=poisson(),
              adapt = abw, 
              gweight = gwr.Gauss,
              type = "deviance")

ab_gwr

# write gwr output into a data frame
ab_gwr_out <- as.data.frame(ab_gwr$SDF)

Dat$amb_deviance_resids <- ab_gwr_out$deviance_resids

# map Local R2
legend_title = expression("Adaptive: Deviance Residuals")
tm_shape(Dat) +
  tm_fill(col = "amb_deviance_resids", title = legend_title, palette = magma(256), style = "cont") + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_scale_bar(breaks = c(0,1,2), text.size = 0.7, position =  c("center", "bottom")) + # add scale bar
  tm_layout(bg.color = "white") # change background colour

# Extract the estimated local coefficients for the explanatory variable

Dat$amb_M_Homicide <- ab_gwr_out$M_Homicide
Dat$amb_W_Sex_assault <- ab_gwr_out$W_Sex_assault

# Male homicide
legend_title = expression("Male homicide")
map_abgwr2 = tm_shape(Dat) +
  tm_fill(col = "amb_M_Homicide", title = legend_title, palette = magma(256), style = "cont") + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_scale_bar(breaks = c(0,1,2), text.size = 0.7, position =  c("center", "bottom")) + # add scale bar
  tm_layout(bg.color = "white") # change background colour

# Sexually assaulted women
legend_title = expression("Sexually assaulted women")
map_abgwr3 = tm_shape(Dat) +
  tm_fill(col = "amb_W_Sex_assault", title = legend_title, palette = magma(256), style = "cont") + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_scale_bar(breaks = c(0,1,2), text.size = 0.7, position =  c("center", "bottom")) + # add scale bar
  tm_layout(bg.color = "white") # change background colour

tmap_arrange(map_abgwr2, map_abgwr3)

# Assessing statistical significance for Male homicide

# compute t statistic for Male homicide
Dat$t_M_Homicide = ab_gwr_out$M_Homicide / ab_gwr_out$M_Homicide_se

# categorise t values
Dat$t_M_Homicide_cat <- cut(Dat$t_M_Homicide,
                            breaks=c(min(Dat$t_M_Homicide), -1.96, 1.96, max(Dat$t_M_Homicide)),
                            labels=c("sig","nonsig", "sig"))

# map statistically significant coefs for Male homicide
legend_title = expression("Male homicide: significant")
tm_shape(Dat) + 
  tm_fill(col = "t_M_Homicide_cat", title = legend_title, legend.hist = TRUE, midpoint = NA, textNA = "", colorNA = "white") +  # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_scale_bar(breaks = c(0,1,2), text.size = 0.7, position =  c("center", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.outside = TRUE) # change background colour & place legend outside

# Municipalities count
table(Dat$t_M_Homicide_cat)

# Get the degrees of freedom from the gwr results object

Dat$dfree <- ab_gwr$results$edf

# Calculate the pvalue

Dat$p_t_M_Homicide <- 2 * pt(-abs(Dat$t_M_Homicide), Dat$dfree)

# Map the pvalue

breaks <- c(0,0.01,0.05,0.1,1)

tm_shape(Dat) +
  tm_polygons(col = "p_t_M_Homicide",palette = "Reds", breaks = breaks,
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 1, 2), size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "t-stat",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE)

# Extract local bandwidths

Dat$bwadapt <- ab_gwr$bandwidth

tm_shape(Dat) +
  tm_polygons(col = "bwadapt", style = "quantile",palette = "Reds", 
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 1, 2), size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "GWR bandwidth",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE)

# Multicollinearity

# correlations between the coefficients produced by the GWR

round(cor(ab_gwr_out[,2:11], use ="complete.obs"),2)

# correlations visually

pairs(ab_gwr_out[,2:11], pch=".")

pairs(as(ab_gwr$SDF, "data.frame")[,2:11])

# tests comparing OLS and GWR models

# Test 1

BFC02.gwr.test(ab_gwr)

# Test 2

BFC99.gwr.test(ab_gwr)

# Test 3

LMZ.F1GWR.test(ab_gwr)

# Test 4

LMZ.F2GWR.test(ab_gwr)

# Test 5

LMZ.F3GWR.test(ab_gwr)