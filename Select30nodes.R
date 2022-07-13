rm(list=ls())
getwd()
setwd('C:/data')
library(dplyr)
library(tidyverse)
data <- read.delim('graphs.txt')
names(data) <- c('username','team','lid','score','matrix')

#Select research world data

#Level 1
level1_data <- filter(data,lid%in%101) #401
level1_data <- filter(level1_data,matrix!='') #397

level1_matrix <- select(level1_data,matrix)
level1_df <- data.frame(matrix(ncol=1,nrow=397))
level1_food_web <- list()
for (i in 1:397){
  level1_food_web[i] <- strsplit(level1_matrix[i,1],split=':')
  level1_df[i,1] <- level1_food_web[[i]][1]
}

#get b
level1_b <- data.frame(matrix(ncol=1,nrow=397))
for(i in 1:397){
  level1_b[i,1] <- level1_food_web[[i]][2]
}

list1_b <- list()
for (j in 1:397){
  list1_b[[j]] <- stringr::str_split(level1_b[j,1], ",") %>% .[[1]] %>% as.numeric()
}

num <- list()
for (i in 1:397){
  num[[i]] <- rep(i,length(list1_b[[i]]))
}

num_vector <- c(1,1,1)
for(i in 2:397){
  num_vector <- c(num_vector,num[[i]])
}

level1_mass <- data.frame(matrix(ncol=3,nrow=3104))
level1_mass[,1] <- num_vector
names(level1_mass) <- c('foodweb','b','mass')

b <- list1_b[[1]]
for(i in 2:397){
  b <- c(b,list1_b[[i]])
}
level1_mass[,2] <- b

for (i in 1:3104){
  if(level1_mass[i,2] < 0){
    level1_mass[i,3] <- (-level1_mass[i,2]/1.71e-06)^(1/(0.75-1))
  }else{
    level1_mass[i,3] <- (level1_mass[i,2]/4.15e-08)^(1/(0.75-1))
  }
}

#level2
level2_data <- filter(data,lid%in%102) #301
level2_data <- filter(level2_data,matrix!='') #282

level2_matrix <- select(level2_data,matrix)
level2_df <- data.frame(matrix(ncol=1,nrow=282))
level2_food_web <- list()
for (i in 1:282){
  level2_food_web[i] <- strsplit(level2_matrix[i,1],split=':')
  level2_df[i,1] <- level2_food_web[[i]][1]
}

level2_b <- data.frame(matrix(ncol=1,nrow=282))
for(i in 1:282){
  level2_b[i,1] <- level2_food_web[[i]][2]
}

list2_b <- list()
for (j in 1:282){
  list2_b[[j]] <- stringr::str_split(level2_b[j,1], ",") %>% .[[1]] %>% as.numeric()
}

num <- list()
for (i in 398:679){
  num[[i]] <- rep(i,length(list2_b[[i-397]]))
}

num_vector <- num[[398]]
for(i in 399:679){
  num_vector <- c(num_vector,num[[i]])
}
level2_mass <- data.frame(matrix(ncol=2,nrow=7280))
level2_mass[,1] <- num_vector

b <- list2_b[[1]]
for(i in 2:282){
  b <- c(b,list2_b[[i]])
}
level2_mass[,2] <- b
level2_mass <- level2_mass[-1049,]
for (i in 1:7279){
  if(level2_mass[i,2] < 0){
    level2_mass[i,3] <- (-level2_mass[i,2]/1.71e-06)^(1/(0.75-1))
  }else{
    level2_mass[i,3] <- (level2_mass[i,2]/4.15e-08)^(1/(0.75-1))
  }
}
names(level2_mass) <- c('foodweb','b','mass')

#level3
level3_data <- filter(data,lid%in%103) #197
level3_data <- filter(level3_data,matrix!='') #186

level3_matrix <- select(level3_data,matrix)
level3_df <- data.frame(matrix(ncol=1,nrow=186))
level3_food_web <- list()
for (i in 1:186){
  level3_food_web[i] <- strsplit(level3_matrix[i,1],split=':')
  level3_df[i,1] <- level3_food_web[[i]][1]
}

level3_b <- data.frame(matrix(ncol=1,nrow=186))
for(i in 1:186){
  level3_b[i,1] <- level3_food_web[[i]][2]
}

list3_b <- list()
for (j in 1:186){
  list3_b[[j]] <- stringr::str_split(level3_b[j,1], ",") %>% .[[1]] %>% as.numeric()
}

num <- list()
for (i in 680:865){
  num[[i]] <- rep(i,length(list3_b[[i-679]]))
}
num_vector <- num[[680]]
for(i in 681:865){
  num_vector <- c(num_vector,num[[i]])
}
level3_mass <- data.frame(matrix(ncol=2,nrow=2594))
level3_mass[,1] <- num_vector

b <- list3_b[[1]]
for(i in 2:186){
  b <- c(b,list3_b[[i]])
}
level3_mass[,2] <- b

for (i in 1:2594){
  if(level3_mass[i,2] < 0){
    level3_mass[i,3] <- (-level3_mass[i,2]/1.71e-06)^(1/(0.75-1))
  }else{
    level3_mass[i,3] <- (level3_mass[i,2]/4.15e-08)^(1/(0.75-1))
  }
}
names(level3_mass) <- c('foodweb','b','mass')

#The name of each food web is different, select the food web with no less than 10 nodes
num1 <- vector()
for(i in 1:397){
  if(sum(level1_mass[,1]==i)>=30){
    num1 <- c(num1,i)
  }else{
    num1 <- num1
  }
}
num1 #3

level1_30nodes <- filter(level1_mass,foodweb%in%num1)
level1_30nodes <- filter(level1_30nodes,mass>8.0e-04)

num2 <- vector()
for(i in 1:282){
  if(sum(level2_mass[,1]==i+397)>=30){
    num2 <- c(num2,i+397)
  }else{
    num2 <- num2
  }
}
num2
length(num2) #102
level2_30nodes <- filter(level2_mass,foodweb%in%num2)
level2_30nodes <- filter(level2_30nodes,mass>8.0e-04)

num3 <- vector()
for(i in 1:186){
  if(sum(level3_mass[,1]==i+679)>=30){
    num3 <- c(num3,i+679)
  }else{
    num3 <- num3
  }
}
num3
length(num3) #19
level3_30nodes <- filter(level3_mass,foodweb%in%num3)
level3_30nodes <- filter(level3_30nodes,mass>8.0e-04)

save(level1_30nodes,level2_30nodes,level3_30nodes,file='research_30nodes.RData')
