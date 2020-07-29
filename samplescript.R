# # This code takes three polygonspatial dataframes, samples, poly1 and Poly 2
# it selects the polygons of poly1 and poly2 inside each of the sampling windows,
# rasterizes them as "change, no-change" binary masks, compares them and calculates the 
# contingency matrices for each window. It offers the option to plot the agreement maps and
# export them as Geotiffs
# the script returns a list with all the contingency tables

library(rgdal)
library(sf)
library(rgeos)
library(raster)
library(greenbrown)
library(tidyverse)
library(data.table)

setwd("~/Documents/victor_valid")

# load study windows

rm(list=ls())
samples <- st_read("~/Documents/victor_valid/samples.shp")

# Load Polygons. (using sf package)
poly1 <- st_read('~/Documents/victor_valid/poly_ori_JRE_j.shp')
poly2 <- st_read('~/Documents/victor_valid/poly_ori_MVS_j.shp')

# load template raster (for rasterization)

template <- raster('template.tif')


# here, i generated a vector with the unique values of the id's of the windows

samplesID <- unique(poly1$sr_band2_1)

###############################################################

compare.r <- function(poly1, poly2, template, samples, writeraster=TRUE){
  samplesID <- samples$sr_band2_1
  test1 <- poly1[poly1$sr_band2_1%in%samplesID,] 
  test2 <- poly2[poly2$sr_band2_1%in%samplesID,]
  window1 <- samples[samples$sr_band2_1%in%samplesID,]
  #return(list(test1, test2, window1))}
  test1 <- st_crop(test1, extent(window1))
  test2 <- st_crop(test2, extent(window1))
  test2 <- test2 %>% st_collection_extract(type="POLYGON")
  template. <- crop(template,extent(window1))
  #return(list(test1, test2, window1))}
  test1.r <- rasterize(test1, template., test1$change_b)
  test2.r <- rasterize(test2, template., test2$change_b)
   suma <- test1.r-test2.r
   (x.stats <- data.frame(x.mean=cellStats(suma, "mean")))
   #return(x.stats)}
   if(x.stats==0){
       DT <-  data.table(
        change <-  c(0,0,0,100),
        no_change <-  c(0, 11130, 11130, 100),
        Sum <-  c(0, 11130,11130, NA),
        UserAccuracy <-  c(100,100,NA,100)
      )
#return(DT)} 
      kappa <- 1
      DT <- as.data.frame(DT)
      rownames(DT) <- c('change', 'no-change', 'Sum','ProducerAccuracy')
      colnames(DT) <- c('change', 'no-change', 'Sum','UserAccuracy')
      comparedata <- list(test1.r,DT,kappa)
      names(comparedata) <- c('raster', 'table', 'kappa')
      class(comparedata) <- "CompareClassification"
      #return(comparedata)
if(writeraster==TRUE){
        writeRaster(test1.r, paste(samplesID, "agreement",sep='_'), format='GTiff', overwrite=TRUE)
   rm(suma, x.stats, DT, kappa)
   #return(comparedata)}
  }
}
  else
  {comparedata <- CompareClassification(test1.r, test2.r, names = list('JRE'=c('change','no-change'),'MVS'=c('change','no-change')), samplefrac = 1)
  plot(comparedata)
  if(writeraster==TRUE){
    writeRaster(comparedata$raster, paste(samplesID, "agreement",sep='_'), format='GTiff', overwrite=TRUE)
  }}
res<- as.data.frame(comparedata$table)

  names(res) <- samplesID
  colnames(res) <- c('change', 'no-change', 'Sum','UserAccuracy') 
  return(res)
  #return(comparedata)
}

results <- (x=1:nrow(samples))%>%map(function(x) compare.r(poly1,poly2, template, samples=samples[x,], writeraster=TRUE))

names(results) <- samplesID
save(results, file= "contingency_m.RData")


