#Sample Evaluation
#Jerónimo Rodriguez, August 2020  

# This code takes three polygonspatial dataframes, samples, poly1 and Poly 2
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
library(tidyverse)
library(lwgeom)
library(diffeR)
library(aRn)


setwd("~/path/to/your/folder")

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

samplesID[5]
###############################################################

square_cont <- function(poly1, poly2, alg, msk, samples, writeraster=FALSE, plotAgMap=TRUE){
  samplesID <- samples$sr_band2_1
  test1 <- poly1[poly1$sr_band2_1%in%samplesID,] 
  test2 <- poly2[poly2$sr_band2_1%in%samplesID,]
  window1 <- samples[samples$sr_band2_1%in%samplesID,]
   test1 <- st_crop(test1, extent(window1))
   test2 <- st_crop(test2, extent(window1))
  msk <- crop(msk, extent(window1))
  alg <- crop(alg, extent(window1))
  test1.r <- rasterize(test1, msk, test1$change_b2)
  test2.r <- rasterize(test2, msk, test2$change_b2)
  alg[is.na(alg[])] <- 0
  crosstab1 <- crosstabm(msk, test1.r, percent=TRUE, population = NULL)
  crosstab2 <- crosstabm(msk, test2.r, percent=TRUE, population=NULL)
  crosstab3 <- crosstabm(msk,alg,percent=TRUE, population = NULL)
  crosstab4 <- crosstabm(test1.r,test2.r,percent=TRUE, population = NULL)
  #differences <- differenceMR(test1.r,msk, eval='original', percent=TRUE)
  return(list(crosstab1, crosstab2, crosstab3, crosstab4))}
  
# Square Contingency matrices
con_tables <- (x=1:nrow(samples))%>%map(function(x) square_cont(poly1,poly2, alg=CC, msk, samples=samples[x,], writeraster=TRUE, plotAgMap =TRUE))
names(con_tables) <- samplesID
  
#Caculate Difference Metrics
tablej <- (x=1:nrow(samples))%>%map(function(x) diffTablej(con_tables[[x]][[1]], digits = 2, analysis = 'error'))
tablej2 <- (x=1:nrow(samples))%>%map(function(x) diffTablej(con_tables[[x]][[2]], digits = 2, analysis = 'error'))
#Caclculate Overall Differences (the inverse of the agreement) 
overalldff <- (x=1:nrow(samples))%>%map(function(x) overallDiffCatj(results[[x]][[1]]))
# Run the function

results <- (x=1:1)%>%map(function(x) compare.r(poly1,msk=CC, template, samples=samples[x,], writeraster=TRUE, plotAgMap =TRUE))

results <- (x=1:nrow(samples))%>%map(function(x) compare.r(poly1,msk=CC, template, samples=samples[x,], writeraster=TRUE, plotAgMap =TRUE))
results2 <- (x=1:nrow(samples))%>%map(function(x) compare.r(poly1,msk=CC, template, samples=samples[x,], writeraster=TRUE, plotAgMap =TRUE))

compare <- (x=1:nrow(samples))%>%map(function(x) agreementj(results[[x]][[2]]))

agreement <- (x=1:nrow(samples))%>%map(function(x) agreementj(results[[x]][[1]]))

omissionm <-(x=1:nrow(samples))%>%map(function(x) omissionj(results[[x]][[1]]))
comission.<-(x=1:nrow(samples))%>%map(function(x) comissionj(results[[x]][[1]]))

                                       
                                       
                                       
                                       
                                       
agreement <- (x=1:nrow(samples))%>%map(function(x) agreementj(results([[1][1]])))



omission <- omissionj(results[[1]])
comission <- comissionj(results[[1]])



