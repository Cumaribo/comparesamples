
library(rgdal)
library(sf)
library(rgeos)
library(raster)
library(greenbrown)
library(tidyverse)

setwd("~/Documents/victor_valid")

# load study windows


samples <- st_read("~/Documents/victor_valid/samples.shp")

# Load Polygons. (using sf package)
polyJRE <- st_read('~/Documents/victor_valid/poly_ori_JRE_j.shp')
polyMVS <- st_read('~/Documents/victor_valid/poly_ori_MVS_j.shp')

# load template raster (for rasterization)

template <- raster('template.tif')


# here, i generated a vector with the unique values of the id's of the windows

samplesID <- unique(polyJRE$sr_band2_1)

samplesf <- sampleslist
poly1=polyJRE
poly2=polyMVS
test1 <- poly1[poly1$sr_band2_1%in%samplesID,]  
test2 <- poly2[poly2$sr_band2_1%in%samplesID,]


poly1[poly1$sr_band2_1%in%samplesID,'sr_band2_1'] 
(poly1$sr_band2_1)
(poly2$sr_band2_1) 


###############################################################
# This is the function that I need to iterate
#This part selects form the list using each one of the names. 
compare.r <- function(poly1, poly2, template, samples, writeraster=TRUE){
  samplesID <- unique(poly1$sr_band2_1)
  # ahora el problema esta acá. En lugar de iterar a lo largo del vector samplesID
  # lo que hace es repetir el primer caso 29 veces....
  test1 <- poly1[poly1$sr_band2_1==samplesID,]  
  test2 <- poly2[poly2$sr_band2_1==samplesID,]
  window1 <- samples[samples$sr_band2_1==samplesID[[i]],]
  test1 <- st_crop(test1, extent(window1))
  test2 <- st_crop(test2, extent(window1))
  template. <- crop(template,extent(window1))
  test1.r <- rasterize(test1, template., test1$change_b)
  test2.r <- rasterize(test2, template., test2$change_b)
  comparedata <- CompareClassification(test1.r, test2.r, names = list('JRE'=c('change','no-change'),'MVS'=c('change','no-change')), samplefrac = 1)
  if(writeraster==TRUE){
    writeRaster(comparedata$raster, paste(samplesID[[i]], "agreement",sep='_'), format='GTiff', overwrite=TRUE)
  }
  # if(writeraster==FALSE){
  #   my_func  <- function(x){
  #     invisible()
  #   }
  #   my_func(100)}
  plot(comparedata)
  
  res<- as.data.frame(comparedata$table)
  return(res)
  #return(comparedata)
}


output <- list()
# no he podido ni con for loop 
# for(i in 1:1)#length(sampleslist)){
# output[i] <-compare.r(polyJRE,polyMVS, template, samples, writeraster = TRUE)
# }

# ni mapeando la función

results <- sampleslist%>%map(compare.r, poly1=polyJRE,poly2=polyMVS, template= template, samples=samples)


compare.wilson <- function(poly1, poly2, template, samples, writeraster=FALSE){
  samplesID <- unique(poly1$sr_band2_1)
  test1 <- poly1[poly1$sr_band2_1==samplesID,] 
  test2 <- poly2[poly2$sr_band2_1==samplesID,]
  window1 <- samples[samples$sr_band2_1==samplesID,]
  return(window1)
  test1 <- st_crop(test1, extent(window1))
  test2 <- st_crop(test2, extent(window1))
  template. <- crop(template,extent(window1))
  test1.r <- rasterize(test1, template., test1$change_b)
  test2.r <- rasterize(test2, template., test2$change_b)
  return(list(test1.r,test2.r))
  comparedata <- CompareClassification(test1.r, test2.r, names = list('JRE'=c('change','no-change'),'MVS'=c('change','no-change')), samplefrac = 1)
  
  if(writeraster==TRUE){
    writeRaster(comparedata$raster, paste(samplesID, "agreement",sep='_'), format='GTiff', overwrite=TRUE)
  }
  # if(writeraster==FALSE){
  #   my_func  <- function(x){
  #     invisible()
  #   }
  #   my_func(100)}
  #plot(comparedata)
  
  res<- as.data.frame(comparedata$table)
  return(res)
  #return(comparedata)
}

tmp <- compare.wilson(polyJRE,polyMVS, template, samples=samples[1,])
tmp2 <- stack(tmp)

comparedata <- CompareClassification(tmp2[[1]], tmp2[[2]], names = list('JRE'=c('change','no-change'),'MVS'=c('change','no-change')), samplefrac = 1)
compareRaster(tmp2[[1]], tmp2[[2]])

plot(tmp2)

#####################3
Mp <- Map(function(x)
  compare.wilson(polyJRE,polyMVS, template, samples=samples[x,],
                 x=1:nrow(samples)))
#######################3
compare.r <- function(poly1, poly2, template, samples, writeraster=TRUE){
  
  temp <- compare.r(polyJRE, polyMVS, template, samples, writeraster = FALSE)
  
  smp <- samples[2,]
  
  Mp <- Map(function(x)
    compare.r(polyJRE,polyMVS, template, samples=samples[x,],
              x=1:nrow(samples)))
  
  
  
  samplesID <- unique(poly1$sr_band2_1)
  # ahora el problema esta acá. En lugar de iterar a lo largo del vector samplesID
  # lo que hace es repetir el primer caso 29 veces....
  test1 <- poly1[poly1$sr_band2_1==unique(poly1$sr_band2_1),]  
  test2 <- poly2[poly2$sr_band2_1==unique(poly1$sr_band2_1),]
  window1 <- samples[samples$sr_band2_1==samples$sr_band2_1,]
  test1 <- st_crop(test1, extent(window1))
  test2 <- st_crop(test2, extent(window1))
  template. <- crop(template,extent(window1))
  test1.r <- rasterize(test1, template., test1$change_b)
  test2.r <- rasterize(test2, template., test2$change_b)
  comparedata <- CompareClassification(test1.r, test2.r, names = list('JRE'=c('change','no-change'),'MVS'=c('change','no-change')), samplefrac = 1)
  # if(writeraster==TRUE){
  #   writeRaster(comparedata$raster, paste(samples$sr_band2_1, "agreement",sep='_'), format='GTiff', overwrite=TRUE)
  # }
  # if(writeraster==FALSE){
  #   my_func  <- function(x){
  #     invisible()
  #   }
  #   my_func(100)}
  # plot(comparedata)
  # 
  res<- as.data.frame(comparedata$table)
  return(res)
  #return(comparedata)
}




stack2df=function(inrast=rastack,invec=polygons, classcolname="id"){
  # extracts into a data frame the pixel values for all bands from different classes
  # defined in a spatial dataframe
  # inrast: the raster dataset containing pixel values to extract in [[1]]
  # invec: spatial dataframe object defining the locations where the data
  # should be extracted from 
  # classcolname: the column in the spatial dataframe containing the names 
  # of the attributes associated to those areas
  # value: a data frame with columns representing the pixel values in each band for
  # the areas labeled as defined by classcolname
  if (is.null(intersect(extent(invec), extent(inrast)))){
    stop("the extents of inrast and invec do not overlap")
  }
  if(as.character(crs(inrast))!= as.character(crs(invec))){
    stop("inrast and invec should have the same projection")
  }
  # required function
  extractval=function(inraster=inrast, msk=msk){
    outvector=raster::mask(inraster, msk) 
    outvector=na.omit(raster::getValues(outvector))
    return(outvector)
  }
  # mask the input raster including  pixels with valid values in all bands only
  inrast=rastmask(inrast)
  
  invec@data$ones=rep(1, nrow(invec@data))
  for (i in 1:nrow(invec@data)){
    testvec=invec[i,]
    testrast=crop(inrast[[1]], extent(testvec))
    # create a raster of class_ids. TRY gdalUtils::gdal_rasterize. It might be faster!!!
    vecrast=raster::rasterize(testvec, testrast, field=invec$ones)
    testrastmskd=mask(testrast,vecrast)
    outstats=ClassStat(testrastmskd) #### This part must be modified for other metrics
    outstats$id=rep(as.character(testvec@data[[classcolname]], nrow(outstats)))
    outstats=data.frame(outstats)
    if(i==1){(totstats=outstats)}else{(totstats=rbind(totstats, outstats))}
  }
  return(totstats)
}

results
test1 <- poly1[poly1$sr_band2_1==samplesID[[c(1:29)],]]  


remotes::install_github("geocompr/geocompkg")
