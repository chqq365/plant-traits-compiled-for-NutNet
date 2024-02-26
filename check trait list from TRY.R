# trait list was downloaded on 12 February 2023

rm(list=ls())
## package needed 
library(data.table);library(tidyverse)
###################################################################################################
##########################check traits with most coverage of species###############################
###################################################################################################
#### check trait names 
# set the working directory 
setwd("C:/Users/chqq3/work/plant traits compiled for NutNet/raw data/")

tn<-fread("trait list.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
colnames(tn)
check.leaf<-tn%>%filter(grepl("leaf", Trait))

tn1<-tn[order(tn$AccSpecNum, decreasing = TRUE), ]
tn_30<-tn1[1:30,]
check.ldmc<-tn1%>%filter(grepl("LDMC", Trait))
check.ldmc<-tn1%>%filter(Trait=="Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)")

# it seems that most traits with highest number of species are categorical traits 
# maybe better to focus on continuous traits 
## get trait id 
paste0(tn_30$TraitID, collapse=",")
## 3400,3401,42,38,43,17,22,759,3106,37,31,26,218,59,28,8,819,95,343,29,154,213,16,3117,3096,14,3113,4,207,98

tn_30.leaf<-tn_30[grepl("leaf", tn_30$Trait), ]## 4 leaf traits
tn_30.leaf$Trait

# check trait names used in previous download (2021)
check.trait.names<-tn%>%filter( TraitID%in% c(3106, 3107, 1, 3108, 3109, 3110, 3111, 3112, 3113, 3114, 11, 3115, 3116, 3117, 3085, 3086, 
                                              403, 3445, 3446, 50, 14, 51, 15, 52, 44, 570, 13, 26))
unique(check.trait.names$Trait)

tn_30.not.include<-tn_30%>%filter(!Trait %in% check.trait.names$Trait)
# most of them were categorical traits 

# maybe still use the traits in previous download 
