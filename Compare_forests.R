# This code is used to download all the forest- no forest binary masks from the 
# Global forest cover Dataset by Hansen et al  using the package ForestChange.
#It  returns a list containiing the succesive masks for the requested period and spatial extent
# given the Canopy cover threshold.
#It requires the package "greenbrown" available here:
http://greenbrown.r-forge.r-project.org
# Load libraries 

library(raster)
library(rgdal)
library(forestChange)
library(greenbrown)
library(rgeos)
library(diffeR)
library(parallel)

#Set your working folder
setwd("~/path/to/your/folder")

#you will need a shapefile of your study area. You can vectorize the mosaic or 
#any of the masks provided for that. You can also use the getGADM() function of the package 
#forestChange for that purpose.  
vectorname <- 'vectorfile'
 mun <- readOGR('~/path/to/yorufolder', vectorname)
 #make sure that your vectorfile is in crs=WGS84, as this is the one of the gobal forest dataset.
 
# if necessary, use this to reproject:
 #mun <- spTransform(mun, crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

Sys.setlocale(locale="C")

#create a list of succesive years. Note. As of today, August 31 2020, the latest year for which
#Hansen Global data is available is 2018. Exp
#available up t
years <- list(2000:2018)
years <- as.data.frame(years)
#FCmask creates a binary forest/no-forest mask for a given year and for the selected polygon.
#run the FCMask function for each year. Set 95 as the threshold. the "mun" argument defines the mask to clip and
# download the forest data 
#the mc.cores argument tells R to run the process using all the available cores in the computer 
#simultaneously.The only limitation  is the available memory. 
#for one single year, the function looks like this:

forest1 <- FCMask(mun, year=2000, cummask=FALSE, perc=95:100, mc.cores = detectCores())

¡
#you will have to run for each one of the years. However we can auotmatize the process
# and let the computer work for us. So we are going to map the function to iterate for the whole *years* list. It returns a list 
# with all the rasters 

forests <- (x=1:nrow(years))%>%map(function(x) FCMask(mun, year=years[x,1], cummask=FALSE, perc=95:100, mc.cores = detectCores()))
#create binary forest mask with forestChange 
# you can plot any just to check. not required
plot(forests[[2]])


# convert the NAs into 0 (zero)
for(i in 1:length(forests)){
forests[[i]][is.na(forests[[i]])] <- 0}

# mask to your study area
forests <- (x=1:length(years))%>%map(function(x) mask(forests[[x]], mun))


# compare each pair of years. 

compare1 <- CompareClassification(forests[[1]], forests[[2]], 
                                   names = list('year n'=c('no-Forest','forest'),'year n+1'=c('no-Forest','forest')), samplefrac = 1)

#you could try to map this function too. Else, you will have to run each step manually, changing the index for each forests Argument. 


#of course, you can run a comparison between any given pair of years for your study period, they don´t need to be consecutive. 
#CompareClassification produces a list with three objects.
# 1. A raster with the changes. The pixel values range from  1 to 4; 1. stands for class 1 that remained the same, 2 and 3 changes 
# in one way or another and 4 is class two remained the same. Yo can export it with writeraster
# 2. A square contingency table that tells you the difference between the pair of maps numerically. it's like a confusion matrix
# but it discloses the level of agreemwnt between both maps and how many pixles were gsined/lost for each class.
# The kappa bwtween the two maps. It is supposed to represent the overall level of agreement between the two maps.
# however, as it does not tell where these pixels are, or to which class they belong, some authors do not recommend using it
# for more information, check https://www.researchgate.net/profile/Marco_Millones/publication/233196329_Death_to_Kappa_Birth_of_quantity_disagreement_and_allocation_disagreement_for_accuracy_assessment/links/0deec531e1ea538616000000.pdf

plot(compare1)
#this plots an agrerement map between each pai of maps, showing what pixels remained the same, and for the ones that changed
# the type of change (forest to no-forest; a forest loss or no-forest to forest, a forest gain). You can export it as a .tiff or png or 
#whatever you prefer
