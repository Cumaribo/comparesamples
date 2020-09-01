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
load(list_matrix[[1]])
#select the matrices i am going to use

list_matrix1 <- list_matrix[c(1,6,7,8,15,17,18,19)]

######here starts the function 

  fdc <- function(files){
  fld1 <- substr(files, 1, 6)
  path1 <- paste(fld1)
  load(files)
  matrix <- confusion_matrices
  return(list(path1, matrix))}
  
  test1 <- fdc(list_matrix1[[1]])
#load data  
conf_mat <- (x=1:length(list_matrix1))%>%map(function(x) fdc(list_matrix1[x]))
#############3pendiente
labels2 <- c('t_100','t_100','t_99','t_99', 't_98', 't_98','t_97','t_97', 't_96', 't_96',
            't_95','t_95', 't_90','t_90','t_80', 't_80', 't_70','t_70', 't_60', 't_60')
labels2 <- c('t_100','t_99', 't_98','t_97', 't_96','t_95', 't_90','t_80', 't_70','t_70', 't_60')
#labels <- t(labels)
#################################
labels <- (x=1:length(list_matrix1))%>%map(function(x) (conf_mat[x][[1]][[1]]))
#labels <- as.data.frame(labels)
#labels <- t(labels)
labels[1]
mat1 <- conf_mat[1]
mat1[[1]][[2]][[9]]
data_prep <- function(mat1){
# extract Producers Accuracy and convert into a nice row for each place 
PA <- as.data.frame((mat1[[1]][[2]][[l]]))
PA <- subset(PA, select = c(1,2))
PA <-t(PA)
PA <- subset(PA,select=4)
PA <- cbind(PA, labels[[l]])
colnames(PA) <- c('PA', 'municipality')
#return(PA)}
UAO <- mat1[[1]][[2]][[l]]
UAO <- subset(UAO, select=4)
UA <- UAO[-c(3,4),]
UA <- as.data.frame(UA)
colnames(UA) <- c('UA')
OA <- UAO[-c(1,2,3),]
OA <- as.data.frame(OA)
OA <- cbind(OA, labels[l])
colnames(OA) <- c('OA', 'municipality')
return(list(PA, UA, OA))}

l=1
test2_mapt1 <- (x=1:length(list_matrix1))%>%map(function(x) data_prep(conf_mat[x]))
names(test2_mapt) <- labels
l=2
test2_mapt2 <- (x=1:length(list_matrix1))%>%map(function(x) data_prep(conf_mat[x]))
names(test2_mapt2) <- labels
l=3
test2_mapt3 <- (x=1:length(list_matrix1))%>%map(function(x) data_prep(conf_mat[x]))
names(test2_mapt3) <- labels
l=4
test2_mapt4 <- (x=1:length(list_matrix1))%>%map(function(x) data_prep(conf_mat[x]))
names(test2_mapt4) <- labels
l=5
test2_mapt5 <- (x=1:length(list_matrix1))%>%map(function(x) data_prep(conf_mat[x]))
names(test2_mapt5) <- labels
l=6
test2_mapt6 <- (x=1:length(list_matrix1))%>%map(function(x) data_prep(conf_mat[x]))
names(test2_mapt6) <- labels
l=7
test2_mapt7 <- (x=1:length(list_matrix1))%>%map(function(x) data_prep(conf_mat[x]))
names(test2_mapt7) <- labels
l=8
test2_mapt8 <- (x=1:length(list_matrix1))%>%map(function(x) data_prep(conf_mat[x]))
names(test2_mapt8) <- labels



forest <- c('no-forest', 'forest')

# tablebuilder <- function(test2_mapt1,test2_mapt2,test2_mapt3,test2_mapt4,test2_mapt5,test2_mapt6,
#                          test2_mapt7,test2_mapt8){
j=1
PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
               test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
PA_all1 <- cbind(PA_all, labels2[[j]], forest)
UA_all1 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
                test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
OA_all1<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
               test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
OA_all1 <- cbind(OA_all, labels2[[j]], forest)
# return(list(PA_all, UA_all, OA_all))}
j=2
PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
                test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
PA_all2 <- cbind(PA_all, labels2[[j]], forest)
UA_all2 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
                 test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
OA_all<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
                test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
OA_all2 <- cbind(OA_all, labels2[[j]], forest)
j=3
PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
                test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
PA_all3 <- cbind(PA_all, labels2[[j]], forest)
UA_all3 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
                 test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
OA_all3<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
                test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
OA_all3 <- cbind(OA_all, labels2[[j]], forest)
j=4
PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
                test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
PA_all4 <- cbind(PA_all, labels2[[j]], forest)
UA_all4 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
                 test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
OA_all4<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
                test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
OA_all4 <- cbind(OA_all, labels2[[j]], forest)
j=5
PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
                test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
PA_all5 <- cbind(PA_all, labels2[[j]], forest)
UA_all5 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
                 test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
OA_all5<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
                test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
OA_all5 <- cbind(OA_all, labels2[[j]], forest)
j=6
PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
                test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
PA_all6 <- cbind(PA_all, labels2[[j]], forest)
UA_all6 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
                 test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
OA_all6<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
                test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
OA_all6 <- cbind(OA_all, labels2[[j]], forest)
j=7
PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
                test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
PA_all7 <- cbind(PA_all, labels2[[j]], forest)
UA_all7 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
                 test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
OA_all7<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
                test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
OA_all7 <- cbind(OA_all, labels2[[j]], forest)
j=8
PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
                test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
PA_all8 <- cbind(PA_all, labels2[[j]], forest)
UA_all8 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
                 test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
OA_all8<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
                test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
OA_all8 <- cbind(OA_all, labels2[[j]], forest)
# j=9
# PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
#                 test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
# PA_all9 <- cbind(PA_all, labels2[[j]], forest)
# UA_all9 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
#                  test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
# OA_all9<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
#                 test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
# OA_all9 <- cbind(OA_all, labels2[[j]], forest)
# j=10
# PA_all <- rbind(test2_mapt1[[j]][[1]],test2_mapt2[[j]][[1]],test2_mapt3[[j]][[1]],test2_mapt4[[j]][[1]],
#                 test2_mapt5[[j]][[1]],test2_mapt6[[j]][[1]],test2_mapt7[[j]][[1]],test2_mapt8[[j]][[1]])
# PA_all10 <- cbind(PA_all, labels2[[j]], forest)
# UA_all10 <- rbind(test2_mapt1[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt2[[j]][[2]],test2_mapt4[[j]][[2]],
#                  test2_mapt5[[j]][[2]],test2_mapt6[[j]][[2]],test2_mapt7[[j]][[2]],test2_mapt8[[j]][[2]])
# OA_all10<- rbind(test2_mapt1[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt2[[j]][[3]],test2_mapt4[[j]][[3]],
#                 test2_mapt5[[j]][[3]],test2_mapt6[[j]][[3]],test2_mapt7[[j]][[3]],test2_mapt8[[j]][[3]])
# OA_all10 <- cbind(OA_all, labels2[[j]], forest)
#PA_all <- as.data.frame(PA_all)
PAUA1 <- cbind(PA_all1,UA_all1)
PAUA2 <- cbind(PA_all2,UA_all2)
PAUA3 <- cbind(PA_all3,UA_all3)
PAUA4 <- cbind(PA_all4,UA_all4)
PAUA5 <- cbind(PA_all5,UA_all5)
PAUA6 <- cbind(PA_all6,UA_all6)
PAUA7 <- cbind(PA_all7,UA_all7)
PAUA8 <- cbind(PA_all8,UA_all8)

PAUA_all <- rbind(PAUA1,PAUA2,PAUA3,PAUA4,PAUA5,PAUA6,PAUA7,PAUA8)
OA_all <- rbind(OA_all1,OA_all2,OA_all3, OA_all4,OA_all5,OA_all6,OA_all7,OA_all8)

PAUA <- as_tibble(PAUA_all)
PAUA$PA <- as.numeric(as.character(PAUA$PA))
PAUA_all <- pivot_longer(PAUA, cols=c(PA,UA), names_to='Type', values_to='Accuracy')
names(PAUA_all)<- c("municipality",'Threshold', 'forest','Type','Accuracy')

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

Accu_long
labels <- rbind(labels, labels, labels, labels)
labels <- t(labels)
labels <- as.data.frame(labels)
alldata

PA1
UA1
