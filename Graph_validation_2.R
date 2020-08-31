# I want to create graphs for all the confusiin matrices.
rm(list=ls())

detach(package:raster)

setwd("~/Documents/Bosques_Victor/cumaribo")

# Foer this, I used greenbrown to calculate the difference between the reference map and a predicted
# or target map. Now i need to manipiulate all the confussion matrices to convert them into a nice big tidy 
# tibble that I will torture with ggplot until it confesses everyting 
library(tidyverse)
#load list with the matrices
list_matrix = list.files(pattern ="Conf_mat")


list_matrix1 <- list_matrix[c(1,6,7,8,15,17,18,19)]


# load empty lists (containers)

# create functions to load the data
fdc <- function(files,i){
  fld1 <- substr(files[i], 1, 6)
  path1 <- paste(fld1)
  return(path1)}  

fdc2 <- function(files,i){
  load(files[i])
  matrix <- confusion_matrices
  return(matrix)}

labels <- c('t_100','t_100','t_99','t_99', 't_98', 't_98','t_97','t_97', 't_96', 't_96',
            't_95','t_95', 't_90','t_90','t_80', 't_80', 't_70','t_70', 't_60', 't_60') 

labels <- t(labels)
labels <- as.data.frame(labels)
labels <- t(labels)
l=1
#run functions
list_c <- list()
list_names <- list()
for(i in 1:length(list_matrix)){
  list_names[[i]] <- fdc(list_matrix,i)
  list_c[[i]] <- fdc2(list_matrix,i)
}


# get the name
names <- unlist(list_names)


# convert the list into a dataframe
the_matrix <- do.call(cbind.data.frame, list_c)

# extract Producers Accuracy and convert into a nice row for each place 

PA <- as.data.frame((the_matrix[][1,]))
PA <- subset(PA, select = c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30))#,33,34,37,38))
PA <-t(PA)
colnames(PA) <- names
# PA <- t(PA)
# PA <- as.data.frame(PA)
# #PA <- t(PA)
# names_col <- colnames(PA)
# #PA <- as.data.frame(PA)

# extract Users Accuracy and convert it into a nice row
UA <- subset(the_matrix, select=c(4,8,12,16,20,24,28,32))#,36,40))
UA <- t(UA)
UA <- subset(UA, select=c(1,2))
UA <- as_tibble(UA)
UA <- UA %>% pivot_longer(c('no-Forest', 'forest'), names_to = 'Turbo', values_to = 'valores')
#UA <- t(UA)
UA <- as.data.frame(UA)


UA[1]=NULL

# names(UA) <- names_col
# rownames(UA) <- names
# UA <- t(UA)
rownames(UA) <- rownames(PA)
colnames(UA) <- colnames(PA)
UA <- as.data.frame(UA)
PA
#UA <- as_tibble(UA)


# extract Overall Accuracy 
OA <- as.data.frame((the_matrix[][4,]))
OA <- subset(OA, select = c(4,8,12,16,20,24,28))#,32,36,40))
OA <-t(OA) 
colnames(OA) <- names
OA <- t(OA)
OA <- as.data.frame(OA)
#PA <- t(PA)
names_col <- colnames(OA)
#PA <- as.data.frame(PA)

# save one for each case

# UA1 <- UA
# PA1 <- PA
# OA1 <- OA
# 
# UA2 <- UA
# PA2 <- PA
# OA2 <- OA
# 
# UA3 <- UA
# PA3 <- PA
# OA3 <- OA

# UA4 <- UA
# PA4 <- PA
# OA4 <- OA

# UA5 <- UA
# PA5 <- PA
# OA5 <- OA

UA6 <- UA
PA6 <- PA
OA6 <- OA
# UA7 <- UA
# PA7 <- PA

# UA8 <- UA
# PA8 <- PA


UA1t <- t(UA1)
UA2t <- t(UA2)
UA3t <- t(UA3)
UA4t <- t(UA4)
UA5t <- t(UA5)
UA6t <- t(UA6)

PA1t <- t(PA1)
PA2t <- t(PA2)
PA3t <- t(PA3)
PA4t <- t(PA4)
PA5t <- t(PA5)
PA6t <- t(PA6)

UA_all <- rbind(UA1t, UA2t, UA3t, UA4t, UA5t, UA6t)#, UA7, UA8)
PA_all <- rbind(PA1t, PA2t, PA3t, PA4t, PA5t, PA6t)#, PA7, PA8)
OA_all <- rbind(OA1, OA2, OA3, OA4, OA5, OA6)#, OA7, OA8)

UA_allt <- t(UA_all)
UA_all <- cbind(UA_all, row.names(UA_all))
PA_all <- cbind(PA_all, row.names(PA_all))
OA_all <- cbind(OA_all, row.names(OA_all))

UA_allt <- UA_allt%>%pivot_longer((names_col), names_to= 'canopy_t', values_to='UA')
PA_all <- PA_all%>%pivot_longer((names_col), names_to= 'canopy_t', values_to='PA')
OA_all <- OA_all%>%pivot_longer((names_col), names_to= 'canopy_t', values_to='OA')

forest_vector
UA_all <- cbind(UA_all, forest_vector)


UA_all <- cbind(UA_all, PA_all[3,])
#create label vector for OA
labels <- c('t_100','t_99','t_98','t_97','t_96','t_95', 't_90', 't_80','t_70', 't_60')
labels <- as.data.frame(labels)
labels <- t(labels)
labels <- cbind(labels,labels,labels,labels,labels,labels,labels,labels)
labels <- t(labels)
labels <- as.data.frame(labels)
OA_all <- cbind(OA_all, labels)

OA_all$canopy_t <- NULL 
names(OA_all)<- c("municipality",'Accuracy','Threshold', 'Type')
Ov_Accu <- as_tibble((OA_all))
Ov_Accu$Threshold <- factor(Ov_Accu$Threshold, levels=c('t_100','t_99','t_98','t_97','t_96'
                                                            ,'t_95', 't_90', 't_80','t_70', 't_60'))

names(UA_all.) <- c("municipality", 'UA', 'forest', 'PA', 'Treshold')


save(Ov_Accu, file = 'Overall_accuracy.RData')

UA_all <-UA_all. 

Accu <- as_tibble(UA_all)
Accu$UA <- as.numeric(as.character(Accu$UA))
type <- c('OA','OA', 'OA', 'OA', 'OA', 'OA', 'OA', 'OA', 'OA', 'OA')
type <- as.data.frame(type)
type <- t(type)
type <- cbind(type,type,type,type,type,type,type, type)
type <- t(type)
type <- as.data.frame(type)
Ov_Accu <- cbind(Ov_Accu, type)
names(Ov_Accu)<- c("municipality",'Accuracy','Threshold', 'Type')

Accu_long <- Accu%>% pivot_longer(c('UA','PA'), names_to = 'Type', values_to = 'Accuracy')

Accu_long$Threshold <- factor(Accu_long$Threshold, levels=c('t_100','t_99','t_98','t_97','t_96'
                                                            ,'t_95', 't_90', 't_80','t_70', 't_60'))

save(Accu_long, file='Accu_long.RData')
load('Accu_long.RData')

Accu_long <- subset(Accu_long, municipality=='montes_m'| municipality=='Cumaribo')

jpeg(filename= 'hansen_plot.jpg', height=8, width = 8, units='in', res=75)
ggplot(Accu_long, aes(x=Threshold, y=Accuracy, fill= Type))+
  geom_bar(stat='identity', position = 'dodge')+#, aes(fill=factor(Type)))+
  #geom_bar(data = Ov_Accu)+
  facet_grid(forest~municipality)+
  theme(strip.text.x = element_text(size = 20, face = "bold"
    ),strip.text.y = element_text(
      size = 20, face = "bold"))+
      theme(axis.text.x = element_text(size=17, angle=90))+ 
  theme(axis.text.y = element_text(size=17))+
  theme(legend.title = element_text( size = 18),
        legend.text = element_text(size = 16))+
  theme(axis.title = element_text(size=17))

  
dev.off()



UA_all <- as.data.frame(UA_all)


ggplot(Ov_Accu, aes(x=Threshold, y=Accuracy))+
  geom_bar(stat='identity')+
  geom_text(aes(label=round(Accuracy, digits = 2)), vjust=-0.3, size=2.5)+
  facet_wrap(.~municipality, nrow=2)+
  theme(axis.text.x = element_text(size=9.5, angle=90))


Accu_long <- as_tibble(Accu_long)

load('Accu_long.RData')

labels <- rbind(labels, labels, labels, labels)
labels <- t(labels)
labels <- as.data.frame(labels)
alldata

PA1
UA1
