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

#Select the data(level1_data) of the matrix
level1_matrix <- select(level1_data,matrix)
level1_df <- data.frame(matrix(ncol=1,nrow=397))
level1_food_web <- list()
for (i in 1:397){
  level1_food_web[i] <- strsplit(level1_matrix[i,1],split=':')
  level1_df[i,1] <- level1_food_web[[i]][1]
}


#Save the data of each matrix to the 'level1_matrix_list',the horizontal axis is the consumer
list1 <- list()
for (j in 1:397){
  list1[[j]] <- stringr::str_split(level1_df[j,1], "[,;:]") %>% .[[1]] %>% as.numeric()
}
level1_matrix_list <- list()
for (k in 1:397){
  level1_matrix_list[[k]] <- matrix(list1[[k]],sqrt(length(list1[[k]])),sqrt(length(list1[[k]])),byrow=F)
}
level1_binary_matrix <- list()
for (i in 1:397){
  level1_binary_matrix[[i]] <- level1_matrix_list[[i]]>0
}
for (i in 1:397){
  level1_binary_matrix[[i]][level1_binary_matrix[[i]]] <- 1
}


#Level2
level2_data <- filter(data,lid%in%102) #301
level2_data <- filter(level2_data,matrix!='') #282

level2_matrix <- select(level2_data,matrix)
level2_df <- data.frame(matrix(ncol=1,nrow=282))
level2_food_web <- list()
for (i in 1:282){
  level2_food_web[i] <- strsplit(level2_matrix[i,1],split=':')
  level2_df[i,1] <- level2_food_web[[i]][1]
}

list2 <- list()
for (j in 1:282){
  list2[[j]] <- stringr::str_split(level2_df[j,1], "[,;:]") %>% .[[1]] %>% as.numeric()
}
level2_matrix_list <- list()
for (k in 1:282){
  level2_matrix_list[[k]] <- matrix(list2[[k]],sqrt(length(list2[[k]])),sqrt(length(list2[[k]])),byrow=F)
}
level2_binary_matrix <- list()
for (i in 1:282){
  level2_binary_matrix[[i]] <- level2_matrix_list[[i]]>0
}
for (i in 1:282){
  level2_binary_matrix[[i]][level2_binary_matrix[[i]]] <- 1
}

#Level3
level3_data <- filter(data,lid%in%103) #197
level3_data <- filter(level3_data,matrix!='') #186

level3_matrix <- select(level3_data,matrix)
level3_df <- data.frame(matrix(ncol=1,nrow=186))
level3_food_web <- list()
for (i in 1:186){
  level3_food_web[i] <- strsplit(level3_matrix[i,1],split=':')
  level3_df[i,1] <- level3_food_web[[i]][1]
}

list3 <- list()
for (j in 1:186){
  list3[[j]] <- stringr::str_split(level3_df[j,1], "[,;:]") %>% .[[1]] %>% as.numeric()
}
level3_matrix_list <- list()
for (k in 1:186){
  level3_matrix_list[[k]] <- matrix(list3[[k]],sqrt(length(list3[[k]])),sqrt(length(list3[[k]])),byrow=F)
}
level3_binary_matrix <- list()
for (i in 1:186){
  level3_binary_matrix[[i]] <- level3_matrix_list[[i]]>0
}
for (i in 1:186){
  level3_binary_matrix[[i]][level3_binary_matrix[[i]]] <- 1
}

save(level1_binary_matrix,level2_binary_matrix,level3_binary_matrix,file='researchworld_binary_matrix.RData')


