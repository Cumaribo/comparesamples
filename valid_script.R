#Sample Evaluation
#Jerónimo Rodriguez, August 2020  

# This code is used to compare the level of agreement between different change/no-change classifications for a set of polygons inside a raster extent.
# It works both with raster as well as with vector data (by rasterizing it) and extracts square contingency tables for each pair. 
# It generates a tibble with the different conparisons and plots with the agreement. 
################## It is still a work in progress#########################

library(rgdal)
library(sf)
library(rgeos)
library(raster)
library(tidyverse)
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

square_cont <- function(poly1, poly2, alg1,alg2,alg3, msk, samples){
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
User1 <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[1]]))
User2 <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[2]]))
CC <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[3]]))
gamma <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[4]]))
CCchsq <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[5]]))
diff1_2 <-(x=1:nrow(samples))%>%map(function(x) overallDiff(con_tables[[x]][[6]]))

#build a nice tibble

User1 <- as.data.frame(User1)
User1 <- t(User1)
User2 <- as.data.frame(User2)
User2 <- t(User2)
CC <-as.data.frame(CC)
CC <- t(CC)
gamma <-as.data.frame(gamma)
gamma <- t(gamma)
CCchsq <-as.data.frame(CCchsq)
CCchsq <- t(CCchsq)
diff1_2 <-as.data.frame(diff1_2)
diff1_2 <- t(diff1_2)
#bind the data together and convert into a tibble
binded <- cbind(samplesID,User1,User2,CC,gamma,CCchsq,diff1_2)
binded <- as.data.frame(binded)
binded <- as_tibble(binded)

binded[,'samplesID'] <- factor(binded[,'samplesID'])
names(binded) <- c('samplesID','User1', 'User2', 'CC','Gamma', 'CCChsq', 'twouser')
binded$samplesID <- as.factor(binded$samplesID)

summary <- binded
save(summary,file= "results.RData")
dir()
ggplot(binded, aes(x=User1, y=User2))+
  geom_point(shape=1)
binded2 <- pivot_longer(binded, cols=User1:User2, names_to='user')
binded3 <- pivot_longer(binded2, cols=CC:CCchsq, names_to='Algorithm')
binded3 <- binded2%+% subset(binded2, source %in% c('User1', 'User2','gamma')) 
  
  ggplot(binded2, aes(x=value,y=CC, color=user))+
  geom_smooth(method = "lm", se=FALSE, color="gray", formula = y ~ x) +
  geom_point(shape=1)+
  geom_abline(slope=1, intercept=0, color='red')+
    labs(title = "Method = CC")
  ggplot(binded2, aes(x=value,y=Gamma, color=user))+
    geom_smooth(method = "lm", se=FALSE, color="gray", formula = y ~ x) +
    geom_point(shape=1)+
    geom_abline(slope=1, intercept=0, color='red')+
    labs(title = "Method = Gamma")
  ggplot(binded2, aes(x=value,y=CCChsq, color=user))+
    geom_smooth(method = "lm", se=FALSE, color="gray", formula = y ~ x) +
    geom_point(shape=1)+
    geom_abline(slope=1, intercept=0, color='red')+
    labs(title = "Method = CCChsq")
