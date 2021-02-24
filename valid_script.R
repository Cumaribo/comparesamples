Sample Evaluation
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
library(furrr)
setwd("~/Documents/victor_valid/Orinoquia")

# load study windows
dir()
rm(list=ls())
samples <- st_read("~/Documents/victor_valid/Orinoquia/samples.shp")
plot(samples)
# Load Polygons. (using sf package)
user1 <- st_read('~/Documents/victor_valid/Orinoquia/poly_Ori_JRE_j.shp')
user2 <- st_read('~/Documents/victor_valid/Orinoquia/poly_Ori_MVS_j.shp')

plot(poly1$geometry, add=TRUE)
# load template raster (for rasterization)
plot(template)
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
#plot(Gamma)
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
  return(list(crosstab1, crosstab2, crosstab3,crosstab4, crosstab5, crosstab6))}
#, crosstab4, crosstab5,crosstab6))}

plan(multisession, workers=12)
#Extract Square Contingency matrices

con_tables<- (x=1:nrow(samples))%>%future_map(function(x) square_cont(user1, user2, alg1=CCACHsq, alg2=Gamma,alg3=CC, msk=msk, samples=samples[x,]))

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
  #ppix_noch1 <- colSums(test[[1]])[1]/pixel_count
  PA2 <- test[[2]]/colSums(test[[2]])
  PA1_gamma <- PA2[1]
  #ppix_noch2 <- colSums(test[[2]])[1]/pixel_count
  PA3 <- test[[3]]/colSums(test[[3]])
  PA1_cc <- PA3[1]
  #ppix_noch3 <- colSums(test[[3]])[1]/pixel_count
  PA1 <- test[[4]]/colSums(test[[4]])
  PA2_chsq <- PA1[1]
  #ppix_noch4 <- colSums(test[[4]])[1]/pixel_count
  PA2 <- test[[5]]/colSums(test[[5]])
  PA2_gamma <- PA2[1]
  #ppix_noch5 <- colSums(test[[5]])[1]/pixel_count
  PA3 <- test[[6]]/colSums(test[[6]])
  PA2_cc <- PA3[1]
  #ppix_noch6 <- colSums(test[[6]])[1]/pixel_count
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
                 upixels_no_ch1,upixels_no_ch2,upixels_no_ch3,upixels_no_ch4,upixels_no_ch5,upixels_no_ch6,NA,NA,NA,NA,NA,NA)#ppix_noch1,ppix_noch2,ppix_noch3,ppix_noch4,ppix_noch5,ppix_noch6, 
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

accu <- map(1:length(con_tables), function(x) accuracies(con_tables[[x]], place='Orinoquia'))
accu <- do.call(rbind, accu)
accu <- as_tibble(accu)
accuo <- accu
save(accuo, file='accuracy_orinoquia2.RData')
# accu%>%mutate(user=as.character)
# convert(fct(user))
pdf(file='acc_orinoquia.pdf',
    width = 8, height=4.5)
ggplot(subset(accu, type!='pix'), aes(x=type,y=accuracy, color=user))+#, fill=algorithm))+
  geom_boxplot()+
  facet_wrap(~algorithm)
dev.off() 

test <- con_tables[[1]]

accuracies2 <- function(diff_mat){
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
  #output1 <- c(UA1_chsq,UA1_gamma,UA1_cc, UA2_chsq,UA2_gamma, UA3_cc)
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
  user <- c('A','B','A','B','A','B','A','B','A','B','A','B','A','B','A','B','A','B','C')
  type <-c('UA', 'UA', 'UA','UA', 'UA', 'UA','PA','PA','PA','PA','PA','PA','OA','OA','OA','OA','OA','OA','pix')  
  algorithm <- c('ch_sq', 'gamma', 'cc','ch_sq', 'gamma', 'cc','ch_sq', 'gamma', 'cc','ch_sq', 'gamma', 'cc','ch_sq', 'gamma', 'cc','ch_sq', 'gamma', 'cc','pix')
  location <- rep('Orinoquia', times=19)
  user <- as.data.frame(user)
  type <- as.data.frame(type)# #  
  algorithm <- as.data.frame(algorithm)
  location <- as.data.frame(location)
  output1 <- cbind(output1, user,type, algorithm, location)
  colnames(output1) <- c('accuracy', 'user', 'type', 'algorithm', 'location')
  # #  # output1 <- as.data.frame(output1)
  return(output1)}

con_tables2[[9]]
con_tables2[[23]]
con_tables2 <- con_tables[-c(5,10,25)]
con_tables2 <- con_tables2[-23]
accu <- map(1:22, function(x) accuracies2(con_tables2[[x]]))

accu <- map(1:1, function(x) accuracies2(con_tables2[[x]]))
accu <- map(1:4, function(x) accuracies2(con_tables[[x]]))


accu <- map(1:length(con_tables2), function(x) accuracies2(con_tables2[[x]]))
accu <- do.call(rbind, accu)
accu <- as_tibble(accu)
accuor2 <- accu
save(accuor2, file='accuracy_ori_ch.RData')
# accu%>%mutate(user=as.character)
# convert(fct(user))
pdf(file='acc_orinoquia.pdf',
    width = 8, height=4.5)
ggplot(subset(accuor2, type!='pix'), aes(x=type,y=accuracy, color=user))+#, fill=algorithm))+
  geom_boxplot()+
  facet_wrap(~algorithm)

accu <- map(1:4, function(x) accuracies2(con_tables[[x]]))


load('/Users/sputnik/Documents/victor_valid/DF/accuracy_df_ch.RData')
load('/Users/sputnik/Documents/victor_valid/Orinoquia/accuracy_ori_ch.RData')
load('/Users/sputnik/Documents/victor_valid/Pucallpa/accuracy_pu_ch.RData')


acufin2 <- rbind(accuor2,accupu2,accudf2)
save(acufin2, file='acc_change.RData')


pdf(file='acc_ch.pdf',
    width = 16, height=9)
ggplot(subset(acufin2, type!='pix'), aes(x=type,y=accuracy, color=user))+#, fill=algorithm))+
  geom_boxplot()+
  facet_grid(vars(location),vars(algorithm))+
  ggtitle('Accuracies change')
dev.off()  


load('/Users/sputnik/Documents/victor_valid/accuracies_no_ch.RData')
pdf(file='acc_no_ch.pdf',
    width = 16, height=9)
ggplot(subset(accufin, type!='pix'), aes(x=type,y=accuracy, color=user))+#, fill=algorithm))+
  geom_boxplot()+
  facet_grid(vars(location),vars(algorithm))+
  ggtitle('Accuracies no-change')
dev.off()
