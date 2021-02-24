Sample Evaluation
#Jerónimo Rodriguez, August 2020  

# This code is used to compare the level of agreement between different change/no-change classifications for a set of polygons inside a raster extent.
# It works both with raster as well as with vector data (by rasterizing it) and extracts square contingency tables for each pair. 
# It generates a tibble with the different conparisons and plots with the agreement. 
################## It is still a work in progress#########################
rm(list=ls())


library(rgdal)
library(sf)
library(rgeos)
library(raster)
library(aRn)
library(diffeR)
library(tidyverse)
library(furrr)
setwd("~/Documents/victor_valid")

# load study windows
dir()
rm(list=ls())
samples <- st_read("~/Documents/victor_valid/Pucallpa/samples_pu.shp")
plot(samples)
# Load Polygons. (using sf package)
user1 <- st_read('~/Documents/victor_valid/Pucallpa/user1_4.shp')
user2 <- st_read('~/Documents/victor_valid/Pucallpa/user2_3.shp')

# load template raster (for rasterization)

template <- raster('pucallpa_tasscapall.tif')
mat <- rclMatrix(152,oneFirst=FALSE)
msk <- reclassify(template, mat)

plot(msk)
# here, i generated a vector with the unique values of the id's of the windows
#plot(window1)
samplesID <- samples$sr_band2_1

CCACHsq <- raster("pu_chsq_ChgMsk.tif")

Gamma <- raster("pu_Gamma_ChgMsk.tif")

CC <- raster("pu_CC_ChgMsk.tif")

# rasters and vectors as lists. 
# type of analysis and output that can be specified. Set results as % or · pixels
# erxport results, create agreement tiff. Create agreement map with symb ology
# what type of output is there 

###############################################################

square_cont <- function(poly1,poly2, alg1,alg2,alg3, msk, samples){
  samplesID <- samples$sr_band2_1
  test1 <- poly1[poly1$sr_band2_1%in%samplesID,] 
  test2 <- poly2[poly2$sr_band2_1%in%samplesID,]
  window1 <- samples[samples$sr_band2_1%in%samplesID,]
  test1 <- st_crop(test1, extent(window1))
  test2 <- st_crop(test2, extent(window1))
  #return(test2)}
  msk <- crop(msk, extent(window1))
  alg1 <- crop(alg1, extent(window1))
  alg2 <- crop(alg2, extent(window1))
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
  #crosstab1 <- crosstabm(msk, test1.r, percent=FALSE, population = NULL)
  #crosstab2 <- crosstabm(msk, test2.r, percent=FALSE, population=NULL)
  crosstab1 <- crosstabm(test1.r,alg1,percent=FALSE, population = NULL)
  crosstab2 <- crosstabm(test1.r,alg2,percent=FALSE, population = NULL)
  crosstab3 <- crosstabm(test1.r,alg3,percent=FALSE, population = NULL)
  crosstab4 <- crosstabm(test2.r,alg1,percent=FALSE, population = NULL)
  crosstab5 <- crosstabm(test2.r,alg2,percent=FALSE, population = NULL)
  crosstab6 <- crosstabm(test2.r,alg3,percent=FALSE, population = NULL)
  
  #crosstab6 <- crosstabm(test1.r,test2.r,percent=FALSE, population = NULL)
  #differences <- differenceMR(test1.r,msk, eval='original', percent=TRUE)
  #overallComponentsPlot(comp = test1.r, ref = msk, ctmatrix = NULL, units = NULL,population = NULL)
  return(list(crosstab1, crosstab2, crosstab3,crosstab4, crosstab5, crosstab6))}#, crosstab4, crosstab5,crosstab6))}

plan(multisession, workers=12)
#Extract Square Contingency matrices
con_tables <- (x=1:nrow(samples))%>%future_map(function(x) square_cont(user1, user2, alg1=CCACHsq, alg2=Gamma,alg3=CC, msk=msk, samples=samples[x,]))

con_tables

#table1: ref= user1, simulated= CCchsq
#table2: ref= user1, simulated= Gamma
#table3: ref= user1, simulated= CC

#table1: ref= user2, simulated= CCchsq
#table2: ref= user2, simulated= Gamma
#table3: ref= user2, simulated= CC


test <- con_tables[[1]]

accuracies <- function(diff_mat, place){
  test <- diff_mat
  pixel_count <- sum(rowSums(test[[1]]))
  UA1 <- test[[1]]/rowSums(test[[1]])
  UA1_chsq <- UA1[1]
  upixels_no_ch1 <- rowSums(test[[1]])[1]/pixel_count
  UA2 <- test[[2]]/rowSums(test[[2]])
  UA1_gamma <- UA2[1]
  upixels_no_ch2 <- rowSums(test[[2]])[1]/pixel_count
  UA3 <- test[[3]]/rowSums(test[[3]])
  UA1_cc <- UA3[1]
  upixels_no_ch3 <- rowSums(test[[3]])[1]/pixel_count
  UA1 <- test[[4]]/rowSums(test[[4]])
  UA2_chsq <- UA1[1]
  upixels_no_ch4 <- rowSums(test[[4]])[1]/pixel_count
  UA2 <- test[[5]]/rowSums(test[[5]])
  UA2_gamma <- UA2[1]
  upixels_no_ch5 <- rowSums(test[[5]])[1]/pixel_count
  UA3 <- test[[6]]/rowSums(test[[6]])
  UA2_cc <- UA3[1]
  upixels_no_ch6 <- rowSums(test[[6]])[1]/pixel_count
  #output1 <- c(UA1_nc,UA1_c,UA2_nc, UA2_c,UA3_nc, UA3_c)
  #return(output1)}
  #return(output1)}
  PA1 <- test[[1]]/colSums(test[[1]])
  PA1_chsq <- PA1[1]
  ppix_noch1 <- colSums(test[[1]])[1]/pixel_count
  PA2 <- test[[2]]/colSums(test[[2]])
  PA1_gamma <- PA2[1]
  ppix_noch2 <- colSums(test[[2]])[1]/pixel_count
  PA3 <- test[[3]]/colSums(test[[3]])
  PA1_cc <- PA3[1]
  ppix_noch3 <- colSums(test[[3]])[1]/pixel_count
  PA1 <- test[[4]]/colSums(test[[4]])
  PA2_chsq <- PA1[1]
  ppix_noch4 <- colSums(test[[4]])[1]/pixel_count
  PA2 <- test[[5]]/colSums(test[[5]])
  PA2_gamma <- PA2[1]
  ppix_noch5 <- colSums(test[[5]])[1]/pixel_count
  PA3 <- test[[6]]/colSums(test[[6]])
  PA2_cc <- PA3[1]
  ppix_noch6 <- colSums(test[[6]])[1]/pixel_count
  OA1_chsq <- sum(diag(test[[1]]))/sum(colSums(test[[1]]))
  OA1_gamma <- sum(diag(test[[2]]))/sum(colSums(test[[2]]))
  OA1_cc <- sum(diag(test[[3]]))/sum(colSums(test[[3]]))
  OA2_chsq <- sum(diag(test[[4]]))/sum(colSums(test[[4]]))
  OA2_gamma <- sum(diag(test[[5]]))/sum(colSums(test[[5]]))
  OA2_cc <- sum(diag(test[[6]]))/sum(colSums(test[[6]]))
  output1 <- list(UA1_chsq,UA1_gamma,UA1_cc, UA2_chsq, UA2_gamma, UA2_cc, PA1_chsq, PA1_gamma, 
                  PA1_cc, PA2_chsq, PA2_gamma, PA2_cc, OA1_chsq,OA1_gamma,OA1_cc,OA2_chsq,OA2_gamma,
                  OA2_cc)
  output1 <- as.data.frame(output1)
  output1 <- t(output1)
  rownames(output1) <- c(1:18)
  user <- c(rep(('A'), times=3),rep(('B'), times=3),rep(('A'), times=3),rep(('B'), times=3), rep(('A'), times=3),rep(('B'),
                                                                                                                     times=3))
  type <-c(rep('UA',times=6), rep('PA', times=6), rep('OA', times=6)) 
  algorithm <- c(rep(c('ch_sq', 'gamma', 'cc'), times=6))
  location <- rep(place, times=18)
  pixels <- list(upixels_no_ch1,upixels_no_ch2,upixels_no_ch3,upixels_no_ch4,upixels_no_ch5,upixels_no_ch6,
                 ppix_noch1,ppix_noch2,ppix_noch3,ppix_noch4,ppix_noch5,ppix_noch6, NA,NA,NA,NA,NA,NA)
  pixels <- as.data.frame(pixels)
  pixels <- t(pixels)
  pixels <- as.data.frame(pixels)
  names(pixels) <- 'pixels'
  rownames(pixels) <- c(1:18)
  user <- as.data.frame(user)
  type <- as.data.frame(type)# #  
  algorithm <- as.data.frame(algorithm)
  location <- as.data.frame(location)
  output1 <- cbind(output1, user,type, algorithm, location, pixels)
  colnames(output1) <- c('accuracy', 'user', 'type', 'algorithm', 'location', 'pixels')
  # #  # output1 <- as.data.frame(output1)
  return(output1)}

accu <- map(1:length(con_tables), function(x) accuracies(con_tables[[x]], place='Pucallpa'))
accu <- do.call(rbind, accu)
accu <- as_tibble(accu)
accup <- accu
save(accup, file='accuracy_pucallpa.RData')
# accu%>%mutate(user=as.character)
# convert(fct(user))
pdf(file='acc_pucallpa.pdf',
    width = 8, height=4.5)
ggplot(subset(accu, type!='pix'), aes(x=type,y=accuracy, color=user))+#, fill=algorithm))+
  geom_boxplot()+
  facet_wrap(~algorithm)
 dev.off() 

 
 accuracies2 <- function(diff_mat, place){
   test <- diff_mat
   pixel_count <- sum(rowSums(test[[1]]))
   UA1 <- test[[1]]/rowSums(test[[1]])
   UA1_chsq <- UA1[2,2]
   UA2 <- test[[2]]/rowSums(test[[2]])
   UA1_gamma <- UA2[2,2]
   UA3 <- test[[3]]/rowSums(test[[3]])
   UA1_cc <- UA3[2,2]
   UA1 <- test[[4]]/rowSums(test[[4]])
   UA2_chsq <- UA1[2,2]
   UA2 <- test[[5]]/rowSums(test[[5]])
   UA2_gamma <- UA2[2,2]
   UA3 <- test[[6]]/rowSums(test[[6]])
   UA2_cc <- UA3[2,2]
   #output1 <- c(UA1_nc,UA1_c,UA2_nc, UA2_c,UA3_nc, UA3_c)
   #return(output1)}
   #return(output1)}
   PA1 <- test[[1]]/colSums(test[[1]])
   PA1_chsq <- PA1[2,2]
   PA2 <- test[[2]]/colSums(test[[2]])
   PA1_gamma <- PA2[2,2]
   PA3 <- test[[3]]/colSums(test[[3]])
   PA1_cc <- PA3[2,2]
   PA1 <- test[[4]]/colSums(test[[4]])
   PA2_chsq <- PA1[2,2]
   PA2 <- test[[5]]/colSums(test[[5]])
   PA2_gamma <- PA2[2,2]
   PA3 <- test[[6]]/colSums(test[[6]])
   PA2_cc <- PA3[2,2]
   OA1_chsq <- sum(diag(test[[1]]))/sum(colSums(test[[1]]))
   OA1_gamma <- sum(diag(test[[2]]))/sum(colSums(test[[2]]))
   OA1_cc <- sum(diag(test[[3]]))/sum(colSums(test[[3]]))
   OA2_chsq <- sum(diag(test[[4]]))/sum(colSums(test[[4]]))
   OA2_gamma <- sum(diag(test[[5]]))/sum(colSums(test[[5]]))
   OA2_cc <- sum(diag(test[[6]]))/sum(colSums(test[[6]]))
   output1 <- list(UA1_chsq,UA1_gamma,UA1_cc, UA2_chsq, UA2_gamma, UA2_cc, PA1_chsq, PA1_gamma, 
                   PA1_cc, PA2_chsq, PA2_gamma, PA2_cc, OA1_chsq,OA1_gamma,OA1_cc,OA2_chsq,OA2_gamma,
                   OA2_cc, pixel_count)
   output1 <- as.data.frame(output1)
   output1 <- t(output1)
   rownames(output1) <- c(1:19)
   user <- c(rep(('A'), times=3),rep(('B'), times=3),rep(('A'), times=3),rep(('B'), times=3), rep(('A'), times=3),rep(('B'), times=3),'C')
   type <-c(rep('UA',times=6), rep('PA', times=6), rep('OA', times=6), 'pix') 
   algorithm <- c(rep(c('ch_sq', 'gamma', 'cc'), times=6),'pix')
   location <- rep(place, times=19)
   user <- as.data.frame(user)
   type <- as.data.frame(type)# #  
   algorithm <- as.data.frame(algorithm)
   location <- as.data.frame(location)
   output1 <- cbind(output1, user,type, algorithm, location)
   colnames(output1) <- c('accuracy', 'user', 'type', 'algorithm', 'location')
   # #  # output1 <- as.data.frame(output1)
   return(output1)}
 
 accu <- map(1:length(con_tables), function(x) accuracies2(con_tables[[x]], place='Pucallpa'))
 accu <- do.call(rbind, accu)
 accu <- as_tibble(accu)
 accupu2 <- accu
 save(accupu2, file='accuracy_pu_ch.RData')
 # accu%>%mutate(user=as.character)
 # convert(fct(user))
 pdf(file='acc_orinoquia.pdf',
     width = 8, height=4.5)
 ggplot(subset(accupu2, type!='pix'), aes(x=type,y=accuracy, color=user))+#, fill=algorithm))+
   geom_boxplot()+
   facet_wrap(~algorithm)
 

 # convert(fct(user))
 pdf(file='acc_pucallpa.pdf',
     width = 8, height=4.5)
 ggplot(subset(accu, type!='pix'), aes(x=type,y=accuracy, color=user))+#, fill=algorithm))+
   geom_boxplot()+
   facet_wrap(~algorithm)
 dev.off() 
 

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

bindedCC <- binded2[c(1,2,5,6)]


