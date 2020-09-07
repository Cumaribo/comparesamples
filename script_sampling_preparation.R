#Purpose: Preprocesing Landsat to obtain tass caps with cv
ommon valid pixels
#Author: Victoria Sarmiento
#Date used: August 2020
#Contact: 
#

#Necesary packages-------------------------------------------------------
library(raster)
library(rgdal)
library(RStoolbox)
library(aRn)
library(maptools)
library(fitdistrplus)

# Setting temporary folder -----------------------------------------
path=("~/Documents/victor_valid/Pucallpa")
setwd(path)
dir.create('tempfiledir')
tempdir=paste(getwd(),'tempfiledir', sep="/")
rasterOptions(tmpdir=tempdir)

#Setting Working Directory -------------------------------------------------------
wd="/Volumes/shared/VictorShare/s3dFiles/Pucallpa"
setwd(wd) 
dir(wd)

# Defining parameters ------------------------------------

listr <- list.files(".", ".gz")
listr <- listr[c(1,3)]
 #load images
stacks<- EEstackWithoutMeta(listr, sat.nm="LO08")
 # get the bands

im1 <- subset(stacks[[1]], c(2:7), drop=TRUE)
im2 <- subset(stacks[[2]], c(1:6), drop=TRUE)
 #this calculates the common extent for both images
commonExtent=function(inlist=rasterlist, method="intersection"){
    # calculates the common extent between all the raster objects within a list.
    # The objects should all overlap at least partially and have the same projection, spatial resolution and be aligned
    # This can be written in a function. It can calculate either the largest extent that includes
    # all the listed elements (union) orthe minimum common extent (intersection).
    for (i in 1: length(inlist)){
      ext=as.matrix(extent(inlist[[i]]))
      if(i==1){
        extall=ext} else {extall=rbind(extall, ext)}
    }

    extall=data.frame(extall)
    if (method=="intersection"){
    minx=max(extall$min[c(seq(1,nrow(extall), by=2))])
    maxx=min(extall$max[c(seq(1,nrow(extall), by=2))])
    miny=max(extall$min[c(seq(2,nrow(extall), by=2))])
    maxy=min(extall$max[c(seq(2,nrow(extall), by=2))])
    } else if (method=="union"){
      minx=min(extall$min[c(seq(1,nrow(extall), by=2))])
      maxx=max(extall$max[c(seq(1,nrow(extall), by=2))])
      miny=min(extall$min[c(seq(2,nrow(extall), by=2))])
      maxy=max(extall$max[c(seq(2,nrow(extall), by=2))])
    } else {stop("unvalid method name")
      }
    e=extent(minx, maxx, miny, maxy)
    return(e)
  }
  

e <- commonExtent(stacks, method = 'intersection')

# use the common extent to crop the images 
im1 <- crop(im1, e)
im2 <- crop(im2,e)


# create reclassification matrices (check the min value of the band(s) you're going to use)
# and calculate a common binary mask (keeps only non-NA pixels)
mat <- rclMatrix(-1860, oneFirst = FALSE)
mat2 <- rclMatrix(-847, oneFirst = FALSE)
msk <- reclassify(im1[[1]],mat)
msk2 <- reclassify(im2[[1]],mat2)
msk3 <- msk*msk2


im1 <- mask(im1,msk3)
im2 <- mask(im2,msk3)
ggRGB(im1, r=3,g=2,b=1, stretch='lin')

#calculate TassCap. As in Stackwithoutmeta sat.nm="LO08", it assigns the 
#band names of LandSat8 to the bands, and thus you have to set the same in sat 
im1tc <- tasseledCap(im1,sat='Landsat8OLI')
im2tc <- tasseledCap(im2,sat='Landsat8OLI')

#plot to check your data

ggRGB(im1tc, r=1,g=2,b=3, stretch='lin')
ggRGB(im2tc, r=1,g=2,b=3, stretch='lin')

#create a stack with all bands
stacktc <- stack(im2tc, im1tc)

writeRaster(stacktc,"pucallpa_tasscapall", format='GTiff')
writeRaster(im1tc,"pu_tasscap2015", format='GTiff')
writeRaster(im2tc,"pu_tasscap2011", format='GTiff')

stacktc <- stack('pucallpa_tasscapall.tif')


plotRGB(stacktc, r=3,g=2,b=1, stretch='lin')
#calculate sampling windows
e <- drawExtent()
stacktc <- crop(stacktc, e)
samples <- sampleRaster(stacktc, n=1)

plot(samples$polygons, add=TRUE)
writeOGR(samples$polygons, '.', 'samples_pu_5', driver="ESRI Shapefile")
