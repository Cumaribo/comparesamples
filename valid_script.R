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
library(greenbrown)
library(tidyverse)
library(data.table)
library(lwgeom)
library(aRn)

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

samplesID[5]
###############################################################

compare.r <- function(poly1, msk, template, samples, writeraster=FALSE, plotAgMap=TRUE){
  samplesID <- samples$sr_band2_1
  test1 <- poly1[poly1$sr_band2_1%in%samplesID,] 
  test2 <- poly2[poly2$sr_band2_1%in%samplesID,]
  window1 <- samples[samples$sr_band2_1%in%samplesID,]
  test1 <- st_crop(test1, extent(window1))
  msk <- crop(msk, extent(window1))
  template. <- crop(template,extent(window1))
  test1.r <- rasterize(test1, template., test1$change_b)
  m <- c(-Inf,1,0,1, Inf, 1)
  m <- matrix(m, ncol=3,byrow=TRUE)
  test1.r <-reclassify(test1.r,m)
  msk[is.na(msk[])] <- 0
  #return(list(test1.r, msk))}
  #test2.r <- rasterize(test2, template., test2$change_b)
  crosstab <- crosstabm(test1.r,msk, percent=TRUE, population = NULL)
  differences <- differenceMR(test1.r,msk, eval='original', percent=TRUE)
  #overallComponentsPlot(comp = test1.r, ref = msk, ctmatrix = NULL, units = NULL,population = NULL)
  #return(crosstab)}
  #return(differences)}
  return(list(crosstab, differences))}
  

#MADscatterplot(test1.r, msk, strata = template.)
  comparedata <- CompareClassification(test1.r, msk, names = list('Ref'=c('no-change','change'),'chg_msk'=c('no-change','change')), samplefrac = 1)
  if(plotAgMap==TRUE){
  plot(comparedata)}
  return(comparedata)}


  if(writeraster==TRUE){
    writeRaster(comparedata$raster, paste(samplesID, "agreement",sep='_'), format='GTiff', overwrite=TRUE)
  }}
res<- as.data.frame(comparedata$table)
  #names(res) <- samplesID
  colnames(res) <- c('no-change', 'change', 'Sum','UserAccuracy')
  #names(res) <- samplesID
  return(res)
  #return(comparedata)
}

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




results <- (x=1:nrow(samples))%>%map(function(x) compare.r(poly1,msk=Gamma, template, samples=samples[x,], writeraster=TRUE, plotAgMap =TRUE))
results3 <- (x=1:4)%>%map(function(x) compare.r(poly1,msk=CCACHsq, template, samples=samples[x,], writeraster=TRUE, plotAgMap =TRUE))
plot(results[[1]]), add=TRUE)

plot(results[[1]], add=TRUE)
plot(results[[1]][[1]])

plot(results[[1]][[2]], add=TRUE)
results

extent(results[[1]])

plot(results[[5]])
results <- (x=1:9)%>%map(function(x) compare.r(poly1, template, samples=samples[x,], writeraster=TRUE, plotAgMap =TRUE))

samplesID[10]

names(results) <- samplesID

# save the results
save(results, file= "contingencym_Orinoquia.RData")

load('contingencym_DF_8.RData')
load('contingency_m.RData')

results

results_rev <- results
dir()
results2 <- do.call(rbind, results)


resultCCSChsq <- results2
resultsGamma <- results2
resultsCC <- results2

summary(resultsGamma$Overall)
summary(resultCCSChsq$Overall)
summary(resultsCC$Overall)
names(results)
names(results_rev)
