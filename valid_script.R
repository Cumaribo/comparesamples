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
library(aRn)
library(diffeR)
library(tidyverse)
setwd("/Users/sputnik/Documents/victor_valid/DF")

# load study windows
dir()
samples <- st_read("/Users/sputnik/Documents/victor_valid/DF/samples_df.shp")
plot(samples, add=TRUE)
# Load Polygons. (using sf package)
poly1 <- st_read('~/Documents/victor_valid/DF/User1_mex.shp')
poly2 <- st_read('~/Documents/victor_valid/DF/user2_DF_2.shp')


plot(poly2)
# load template raster (for rasterization)

template <- raster('/Users/sputnik/Documents/victor_valid/DF/mask_DF.tif')

samplesID <- samples$sr_band2_1

plot(samples$brght_1, add=TRUE)

CCACHsq <- raster("/Volumes/shared/VictorShare/s3dFiles/Mexico/OutputsChsqCCA/LT05_L1TP_026046_20100205_20161016_removeChgMsk6.tif")
Gamma <- raster("/Volumes/shared/VictorShare/s3dFiles/Mexico/OutputsGamma/LT05_L1TP_026046_20100205_20161016_removeChgMsk8.tif")
CC <- raster("/Volumes/shared/VictorShare/s3dFiles/Mexico/OutputsGammaCCA/LT05_L1TP_026046_20100205_20161016_removeChgMsk5.tif")
# rasters and vectors as lists. 
# type of analysis and output that can be specified. Set results as % or · pixels
# erxport results, create agreement tiff. Create agreement map with symb ology
# what type of output is there 

###############################################################

square_cont <- function(poly1, poly2, alg1,alg2,alg3, msk, samples){
  samplesID <- samples$sr_band2_1
  test1 <- poly1[poly1$sr_band2_1%in%samplesID,] 
  test2 <- poly2[poly2$sr_band2_1%in%samplesID,]
  window1 <- samples[samples$sr_band2_1%in%samplesID,]
  test1 <- st_crop(test1, extent(window1))
  test2 <- st_crop(test2, extent(window1))
  #return(test2)}
  msk <- crop(msk, extent(window1))
  #return(msk)}
  alg1 <- crop(alg1, extent(window1))
  #return(alg1)}
  alg2 <- crop(alg2, extent(window1))
  #return(alg2)}
  alg3 <- crop(alg3, extent(window1))
  #return(alg3)}
  test1.r <- rasterize(test1, msk, test1$change_b)
  test2.r <- rasterize(test2, msk, test2$change_b)
  #return(list(test1.r,test2.r))}
  alg1[is.na(alg1[])] <- 0
  alg2[is.na(alg2[])] <- 0
  alg3[is.na(alg3[])] <- 0
  #return(alg)}
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
con_tables <- (x=1:nrow(samples))%>%map(function(x) square_cont(poly1,poly2, alg1=CC, alg2=Gamma,alg3=CCACHsq, msk, samples=samples[x,]))



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



names(binded) <- c('samplesID','User1', 'User2', 'CC','Gamma', 'CCChsq', 'Diff_user')
binded$samplesID <- as.factor(binded$samplesID)

binded
save(binded,file= "results_Mexico_2.RData")

###########################################Hasta acá llegá. De acá para 
# abajo van las gráficas. 
# binded <- summary
# rm(binded2)
# dir()




binded2 <- pivot_longer(binded, cols=User1:User2, names_to='user')
# this still does not work, but this is what i need to solve!!!
#binded2 <- pivot_longer(binded, cols=CC:CCChsq, names_to='Algorithm')

#binded2 %>% pivot_longer(everything(), names_to = c("algo","algomas"),names_pattern = "algo")
#<- pivot_longer
# this does nothing, and i don't rmemeber what i was trying to accomplish here, i think
# i wanted to organice it into facets. Still needs to be done. 
binded3 <- pivot_longer(binded2, cols=User1:User2, names_to='User')
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
rm(summary)

