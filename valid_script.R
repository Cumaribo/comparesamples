#Sample Evaluation
#Jerónimo Rodriguez, August 2020  

# This code is takes three polygonspatial dataframes, samples, poly1 and Poly 2
# it selects the polygons of poly1 and poly2 inside each of the sampling windows,
# rasterizes them as "change, no-change" binary masks, compares them and calculates the 
# contingency matrices for each window. It offers the option to plot the agreement maps and
# export them as Geotiffs
# the script returns a list with all the contingency tables

#It requires the package "greenbrown" available here:
http://greenbrown.r-forge.r-project.org

library(rgdal)
library(sf)
library(rgeos)
library(raster)
library(greenbrown)
library(tidyverse)
library(data.table)
library(lwgeom)
library(aRn)
library(diffeR)

setwd("~/Documents/victor_valid/Orinoquia")

# load study windows
dir()
rm(list=ls())
samples <- st_read("~/Documents/victor_valid/Orinoquia/samples.shp")
plot(samples)
# Load Polygons. (using sf package)
poly1 <- st_read('~/Documents/victor_valid/Orinoquia/poly_Ori_JRE_j.shp')
poly2 <- st_read('~/Documents/victor_valid/Orinoquia/poly_Ori_MVS_j.shp')


# load template raster (for rasterization)

template <- raster('template.tif')
mat <- rclMatrix(-1796,oneFirst=FALSE)
msk <- reclassify(template, mat)

plot(msk)
# here, i generated a vector with the unique values of the id's of the windows
#plot(window1)
samplesID <- samples$sr_band2_1

CCACHsq <- raster("LC08_L1TP_005057_20140128_20170426_removeChgMsk12_CCAChsq.tif")
Gamma <- raster("LC08_L1TP_005057_20140128_20170426_removeChgMsk14_Gamma.tif")
CC <- raster("LC08_L1TP_005057_20140128_20170426_removeChgMsk8_CCA.tif")

###############################################################

square_cont <- function(poly1, poly2, alg1,alg2,alg3, msk, samples, writeraster=FALSE, plotAgMap=TRUE){
  samplesID <- samples$sr_band2_1
  test1 <- poly1[poly1$sr_band2_1%in%samplesID,] 
  test2 <- poly2[poly2$sr_band2_1%in%samplesID,]
  window1 <- samples[samples$sr_band2_1%in%samplesID,]
   test1 <- st_crop(test1, extent(window1))
   test2 <- st_crop(test2, extent(window1))
  msk <- crop(msk, extent(window1))
  alg1 <- crop(alg1, extent(window1))
  alg2 <- crop(alg2, extent(window1))
  alg3 <- crop(alg3, extent(window1))
  test1.r <- rasterize(test1, msk, test1$change_b)
  test2.r <- rasterize(test2, msk, test2$change_b)
  #return(list(test1.r,test2.r))}
  alg1[is.na(alg1[])] <- 0
  alg2[is.na(alg2[])] <- 0
  alg3[is.na(alg3[])] <- 0
  #return(alg)}
  #test2.r <- rasterize(test2, template., test2$change_b)
  #claculate the cont_table between each dataset and a plain background
  crosstab1 <- crosstabm(msk, test1.r, percent=TRUE, population = NULL)
  crosstab2 <- crosstabm(msk, test2.r, percent=TRUE, population=NULL)
  crosstab3 <- crosstabm(msk,alg1,percent=TRUE, population = NULL)
  crosstab4 <- crosstabm(msk,alg2,percent=TRUE, population = NULL)
  crosstab5 <- crosstabm(msk,alg3,percent=TRUE, population = NULL)
  crosstab6 <- crosstabm(test1.r,test2.r,percent=TRUE, population = NULL)
  #differences <- differenceMR(test1.r,msk, eval='original', percent=TRUE)
  #overallComponentsPlot(comp = test1.r, ref = msk, ctmatrix = NULL, units = NULL,population = NULL)
  return(list(crosstab1, crosstab2, crosstab3, crosstab4, crosstab5,crosstab6))}
  
#Extract Square Contingency matrices
con_tables <- (x=1:nrow(samples))%>%map(function(x) square_cont(poly1,poly2, alg1=CC, alg2=Gamma,alg3=CCACHsq, msk, samples=samples[x,], writeraster=TRUE, plotAgMap =TRUE))
names(con_tables) <- samplesID


#Caculate Difference Metrics
tablej1 <- (x=1:nrow(samples))%>%map(function(x) diffTablej(con_tables[[x]][[1]], digits = 2, analysis = 'error'))
tablej2 <- (x=1:nrow(samples))%>%map(function(x) diffTablej(con_tables[[x]][[2]], digits = 2, analysis = 'error'))
tablejCC<- (x=1:nrow(samples))%>%map(function(x) diffTablej(con_tables[[x]][[3]], digits = 2, analysis = 'error'))
tablejgamma<- (x=1:nrow(samples))%>%map(function(x) diffTablej(con_tables[[x]][[4]], digits = 2, analysis = 'error'))
tablejCCchsq<- (x=1:nrow(samples))%>%map(function(x) diffTablej(con_tables[[x]][[5]], digits = 2, analysis = 'error'))#Caclculate Overall Differences (the inverse of the agreement) 

# calculate Overall Differences
overalldiff1 <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[1]]))
overalldiff2 <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[2]]))
overalldiff_CC <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[3]]))
overalldiff_gamma <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[4]]))
overalldiff_CCchsq <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[5]]))
overalldiff1_2 <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[6]]))


#Assign the IDs from Samples
names(overalldiff1) <- samplesID
names(overalldiff2) <- samplesID
names(overalldiff_CC) <- samplesID
names(overalldiff_gamma) <- samplesID
names(overalldiff_CCchsq) <- samplesID
names(overalldiff1_2) <- samplesID


#bind the data together and convert into a tibble
binded <- cbind(overalldiff1,overalldiff2,overalldiff_CC,overalldiff_gamma,overalldiff_CCchsq,overalldiff1_2)
binded <- as.data.frame(binded)
binded <- as_tibble(binded)

ggplot(data=binded,mapping = )
#Set the column names
names(binded) <- c('User1', 'User2', 'CC','Gamma', 'CCChsq', 'User1/2')
