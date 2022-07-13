rm(list=ls())
getwd()
setwd('C:/data')
library(dplyr)
library(tidyverse)
load('research_30nodes.RData')
load('researchworld_binary_matrix.RData')
#web.name
level1_summary <- as.data.frame(unique(level1_30nodes$foodweb))

#num.sp
num.sp <- table(level1_30nodes$foodweb)[[1]]
for(i in 2:3){
  num.sp <- c(num.sp,table(level1_30nodes$foodweb)[[i]])
  num.sp
}
level1_summary[,2] <- num.sp

#num.B
datanum.B <- filter(level1_30nodes,b<0)
num.B <- table(datanum.B$foodweb)[[1]]
for(i in 2:3){
  num.B <- c(num.B,table(datanum.B$foodweb)[[i]])
  num.B
}
level1_summary[,3] <- num.B

#num.T
a <- list()

for(i in level1_summary$`unique(level1_30nodes$foodweb)`){
  count <- 0
  for(j in 1:nrow(level1_binary_matrix[[i]])){
    if(sum(level1_binary_matrix[[i]][j,]==1)==0){
      count <- count+1
    }else{
      count <- count
    }
  }
  a[[i]] <- count
}

num.T <- c()
for(i in level1_summary$`unique(level1_30nodes$foodweb)`){
  num.T <- c(num.T,a[[i]])
}
level1_summary[,4] <- num.T

#num.I
num.I <- c()
num.I <- level1_summary$V2-level1_summary$V3-level1_summary$V4
level1_summary[,5] <- num.I

#num.link
num.link <- c()
for(i in level1_summary$`unique(level1_30nodes$foodweb)`){
  num.link <- c(num.link,sum(level1_binary_matrix[[i]]))
}
level1_summary[,6] <- num.link

#get B.num, T.num, I.num
list.B.num <- list()
for(k in unique(level1_30nodes$foodweb)){
  B.num <- c()
  for(i in 1:nrow(level1_binary_matrix[[k]])){
    if((sum(level1_binary_matrix[[k]][,i]==1))==0){
      B.num <- c(B.num,i)
    }else{
      B.num <- B.num
    }
  }
  list.B.num[[k]] <- B.num
}

list.T.num <- list()
for(k in unique(level1_30nodes$foodweb)){
  T.num <- c()
  for(i in 1:nrow(level1_binary_matrix[[k]])){
    if((sum(level1_binary_matrix[[k]][i,]==1))==0){
      T.num <- c(T.num,i)
    }else{
      T.num <- T.num
    }
  }
  list.T.num[[k]] <- T.num
}


list.I.num <- list()
for(k in unique(level1_30nodes$foodweb)){
  I.num <- c()
  I.num <- setdiff(c(1:nrow(level1_binary_matrix[[k]])),c(list.B.num[[k]],list.T.num[[k]]))
  list.I.num[[k]] <- I.num
}

#num.TI
list.num.TI <- list()
for(k in unique(level1_30nodes$foodweb)){
  num.TI <- 0
  for(i in list.I.num[[k]]){
    for(j in list.T.num[[k]]){
      if(level1_binary_matrix[[k]][i,j]==1){
        num.TI <- num.TI+1
      }else{
        num.TI <- num.TI
      }
    }
  }
  list.num.TI[[k]] <- num.TI
}
num.TI <- c()
for(k in unique(level1_30nodes$foodweb)){
  num.TI <- c(num.TI,list.num.TI[[k]])
}
level1_summary[,7] <- num.TI

#num.TB
list.num.TB <- list()
for(k in unique(level1_30nodes$foodweb)){
  num.TB <- 0
  for(i in list.B.num[[k]]){
    for(j in list.T.num[[k]]){
      if(level1_binary_matrix[[k]][i,j]==1){
        num.TB <- num.TB+1
      }else{
        num.TB <- num.TB
      }
    }
  }
  list.num.TB[[k]] <- num.TB
}
num.TB <- c()
for(k in unique(level1_30nodes$foodweb)){
  num.TB <- c(num.TB,list.num.TB[[k]])
}
level1_summary[,8] <- num.TB


#num.II
list.num.II <- list()
for(k in unique(level1_30nodes$foodweb)){
  num.II <- 0
  for(i in list.I.num[[k]]){
    for(j in list.I.num[[k]]){
      if(level1_binary_matrix[[k]][i,j]==1){
        num.II <- num.II+1
      }else{
        num.II <- num.II
      }
    }
  }
  list.num.II[[k]] <- num.II
}
num.II <- c()
for(k in unique(level1_30nodes$foodweb)){
  num.II <- c(num.II,list.num.II[[k]])
}
level1_summary[,9] <- num.II

#num.IB
list.num.IB <- list()
for(k in unique(level1_30nodes$foodweb)){
  num.IB <- 0
  for(i in list.B.num[[k]]){
    for(j in list.I.num[[k]]){
      if(level1_binary_matrix[[k]][i,j]==1){
        num.IB <- num.IB+1
      }else{
        num.IB <- num.IB
      }
    }
  }
  list.num.IB[[k]] <- num.IB
}
num.IB <- c()
for(k in unique(level1_30nodes$foodweb)){
  num.IB <- c(num.IB,list.num.IB[[k]])
}
level1_summary[,10] <- num.IB

#Prop.T (num.T/num.sp)
Prop.T <- level1_summary[,4]/level1_summary[,2]
level1_summary[,11] <- Prop.T

#Prop.TI (num.TI/num.link)
Prop.TI <- level1_summary[,7]/level1_summary[,6]
level1_summary[,12] <- Prop.TI

#Prop.TB (num.TB/num.link)
Prop.TB <- level1_summary[,8]/level1_summary[,6]
level1_summary[,13] <- Prop.TB

#Prop.II (num.II/num.link)
Prop.II <- level1_summary[,9]/level1_summary[,6]
level1_summary[,14] <- Prop.II

#Prop.IB (num.IB/num.link)
Prop.IB <- level1_summary[,10]/level1_summary[,6]
level1_summary[,15] <- Prop.IB

#T.score (Prop.T-0.5*Prop.T*(num.TB/(num.T*num.B)))
T.score <- level1_summary[,11]-0.5*level1_summary[,11]*(level1_summary[,8]/(level1_summary[,4]*level1_summary[,3]))
level1_summary[,16] <- T.score

#mean.mass
mean.mass <- aggregate(level1_30nodes$mass,by=list(foodweb=level1_30nodes$foodweb),mean)[,2]
level1_summary[,17] <- mean.mass

#R.C.slope
con.mass <- c()
res.mass <- c()
number <- c()
number[1] <- 0
number[2] <- 33
for(i in 3){
  number[i] <- level1_summary[i-1,2]+number[i-1]
}
level1_foodwebnum <- unique(level1_30nodes$foodweb)
for(k in 1:length(level1_foodwebnum)){
  for(i in 1:sqrt(length(level1_binary_matrix[[level1_foodwebnum[k]]]))){
    for(j in 1:sqrt(length(level1_binary_matrix[[level1_foodwebnum[k]]]))){
      if(level1_binary_matrix[[level1_foodwebnum[k]]][i,j]==1){
        con.mass <- c(con.mass,level1_30nodes$mass[j+number[k]])
        res.mass <- c(res.mass,level1_30nodes$mass[i+number[k]])
      }
    }
  }
}

R.C.slope.data <- as.data.frame(con.mass)
R.C.slope.data[,2] <- res.mass
names(R.C.slope.data) <- c('con.mass','res.mass')

position_level1 <- c()
position_level1[1] <- 0
position_level1[2] <- 84
for(i in 3:4){
  position_level1[i] <- level1_summary[i-1,6]+position_level1[i-1]
}
R.C.slope.level1 <- list()
for(i in 1:3){
  R.C.slope.level1[[i]] <- R.C.slope.data[(position_level1[i]+1):(position_level1[i+1]),]
}

R.C.slope <- c()
for(i in 1:3){
  R.C.slope[i] <- summary(lm(formula=log10(R.C.slope.level1[[i]][1:level1_summary$V6[i],2])~log10(R.C.slope.level1[[i]][1:level1_summary$V6[i],1])))[[4]][2]
}
level1_summary[,18] <- R.C.slope

##R.C.cor 
R.C.cor <- c()
for(i in 1:3){
  R.C.cor[i] <- cor(log10(R.C.slope.level1[[i]][1:level1_summary$V6[i],2]),log10(R.C.slope.level1[[i]][1:level1_summary$V6[i],1]))
}
level1_summary[,19] <- R.C.cor

#R.C.R2 
R.C.R2 <- c()
for(i in 1:3){
  R.C.R2[i] <- summary(lm(formula = log10(R.C.slope.level1[[i]][1:level1_summary$V6[i],2])~log10(R.C.slope.level1[[i]][1:level1_summary$V6[i],1])))[[8]]
}
level1_summary[,20] <- R.C.R2

#R.C.slope.R2
level1_summary[,21] <- level1_summary[,18]*level1_summary[,20]

#Prop.C.big.R.small
Prop.C.big.R.small <- c()
for(i in 1:3){
  Prop.C.big.R.small[i] <- (sum(R.C.slope.level1[[i]][1:level1_summary$V6[i],1]>R.C.slope.level1[[i]][1:level1_summary$V6[i],2]))/level1_summary[i,6]
}
level1_summary[,22] <- Prop.C.big.R.small

names(level1_summary) <- c('web.name','num.sp','num.B','num.T','num.I',
                           'num.link','num.TI','num.TB','num.II','num.IB',
                           'Prop.T','Prop.TI','Prop.TB','Prop.II','Prop.IB','T.score',
                           'mean.mass','R.C.slope','R.C.cor','R.C.R2','R.C.slope.R2',
                           'Prop.C.big.R.small')

level1_summary[is.na(level1_summary)] <- 0

#level2
#web.name
level2_summary <- as.data.frame(unique(level2_30nodes$foodweb))

#num.sp
num.sp <- table(level2_30nodes$foodweb)[[1]]
for(i in 2:100){
  num.sp <- c(num.sp,table(level2_30nodes$foodweb)[[i]])
  num.sp
}
level2_summary[,2] <- num.sp

#num.B
datanum.B <- filter(level2_30nodes,b<0)
num.B <- table(datanum.B$foodweb)[[1]]
for(i in 2:100){
  num.B <- c(num.B,table(datanum.B$foodweb)[[i]])
  num.B
}
level2_summary[,3] <- num.B

#num.T
a <- list()
for(i in level2_summary$`unique(level2_30nodes$foodweb)`){
  count <- 0
  for(j in 1:nrow(level2_binary_matrix[[i-397]])){
    if(sum(level2_binary_matrix[[i-397]][j,]==1)==0){
      count <- count+1
    }else{
      count <- count
    }
  }
  a[[i]] <- count
}

num.T <- c()
for(i in level2_summary$`unique(level2_30nodes$foodweb)`){
  num.T <- c(num.T,a[[i]])
}
level2_summary[,4] <- num.T

#num.I
num.I <- c()
num.I <- level2_summary$V2-level2_summary$V3-level2_summary$V4
level2_summary[,5] <- num.I

#num.link
num.link <- c()
for(i in level2_summary$`unique(level2_30nodes$foodweb)`){
  num.link <- c(num.link,sum(level2_binary_matrix[[i-397]]))
}
level2_summary[,6] <- num.link

#get B.num, T.num, I.num
list.B.num <- list()
for(k in unique(level2_30nodes$foodweb)){
  B.num <- c()
  for(i in 1:nrow(level2_binary_matrix[[k-397]])){
    if((sum(level2_binary_matrix[[k-397]][,i]==1))==0){
      B.num <- c(B.num,i)
    }else{
      B.num <- B.num
    }
  }
  list.B.num[[k-397]] <- B.num
}

list.T.num <- list()
for(k in unique(level2_30nodes$foodweb)){
  T.num <- c()
  for(i in 1:nrow(level2_binary_matrix[[k-397]])){
    if((sum(level2_binary_matrix[[k-397]][i,]==1))==0){
      T.num <- c(T.num,i)
    }else{
      T.num <- T.num
    }
  }
  list.T.num[[k-397]] <- T.num
}

list.I.num <- list()
for(k in unique(level2_30nodes$foodweb)){
  I.num <- c()
  I.num <- setdiff(c(1:nrow(level2_binary_matrix[[k-397]])),c(list.B.num[[k-397]],list.T.num[[k-397]]))
  list.I.num[[k-397]] <- I.num
}


#num.TI
list.num.TI <- list()
for(k in unique(level2_30nodes$foodweb)){
  num.TI <- 0
  for(i in list.I.num[[k-397]]){
    for(j in list.T.num[[k-397]]){
      if(level2_binary_matrix[[k-397]][i,j]==1){
        num.TI <- num.TI+1
      }else{
        num.TI <- num.TI
      }
    }
  }
  list.num.TI[[k-397]] <- num.TI
}
num.TI <- c()
for(k in unique(level2_30nodes$foodweb)){
  num.TI <- c(num.TI,list.num.TI[[k-397]])
}
level2_summary[,7] <- num.TI

#num.TB
list.num.TB <- list()
for(k in unique(level2_30nodes$foodweb)){
  num.TB <- 0
  for(i in list.B.num[[k-397]]){
    for(j in list.T.num[[k-397]]){
      if(level2_binary_matrix[[k-397]][i,j]==1){
        num.TB <- num.TB+1
      }else{
        num.TB <- num.TB
      }
    }
  }
  list.num.TB[[k-397]] <- num.TB
}
num.TB <- c()
for(k in unique(level2_30nodes$foodweb)){
  num.TB <- c(num.TB,list.num.TB[[k-397]])
}
level2_summary[,8] <- num.TB

#num.II
list.num.II <- list()
for(k in unique(level2_30nodes$foodweb)){
  num.II <- 0
  for(i in list.I.num[[k-397]]){
    for(j in list.I.num[[k-397]]){
      if(level2_binary_matrix[[k-397]][i,j]==1){
        num.II <- num.II+1
      }else{
        num.II <- num.II
      }
    }
  }
  list.num.II[[k-397]] <- num.II
}
num.II <- c()
for(k in unique(level2_30nodes$foodweb)){
  num.II <- c(num.II,list.num.II[[k-397]])
}
level2_summary[,9] <- num.II

#num.IB
list.num.IB <- list()
for(k in unique(level2_30nodes$foodweb)){
  num.IB <- 0
  for(i in list.B.num[[k-397]]){
    for(j in list.I.num[[k-397]]){
      if(level2_binary_matrix[[k-397]][i,j]==1){
        num.IB <- num.IB+1
      }else{
        num.IB <- num.IB
      }
    }
  }
  list.num.IB[[k-397]] <- num.IB
}
num.IB <- c()
for(k in unique(level2_30nodes$foodweb)){
  num.IB <- c(num.IB,list.num.IB[[k-397]])
}
level2_summary[,10] <- num.IB


#Prop.T (num.T/num.sp)
Prop.T <- level2_summary[,4]/level2_summary[,2]
level2_summary[,11] <- Prop.T

#Prop.TI (num.TI/num.link)
Prop.TI <- level2_summary[,7]/level2_summary[,6]
level2_summary[,12] <- Prop.TI

#Prop.TB (num.TB/num.link)
Prop.TB <- level2_summary[,8]/level2_summary[,6]
level2_summary[,13] <- Prop.TB

#Prop.II (num.II/num.link)
Prop.II <- level2_summary[,9]/level2_summary[,6]
level2_summary[,14] <- Prop.II

#Prop.IB (num.IB/num.link)
Prop.IB <- level2_summary[,10]/level2_summary[,6]
level2_summary[,15] <- Prop.IB

#T.score (Prop.T-0.5*Prop.T*(num.TB/(num.T*num.B)))
T.score <- level2_summary[,11]-0.5*level2_summary[,11]*(level2_summary[,8]/(level2_summary[,4]*level2_summary[,3]))
level2_summary[,16] <- T.score

#mean.mass
mean.mass <- aggregate(level2_30nodes$mass,by=list(foodweb=level2_30nodes$foodweb),mean)[,2]
level2_summary[,17] <- mean.mass

#R.C.slope
con.mass <- c()
res.mass <- c()
number <- c()
number[1] <- 0
number[2] <- 30
for(i in 3:100){
  number[i] <- level2_summary[i-1,2]+number[i-1]
}

level2_foodwebnum <- unique(level2_30nodes$foodweb)
for(k in 1:length(level2_foodwebnum)){
  for(i in 1:sqrt(length(level2_binary_matrix[[(level2_foodwebnum[k])-397]]))){
    for(j in 1:sqrt(length(level2_binary_matrix[[(level2_foodwebnum[k])-397]]))){
      if(level2_binary_matrix[[(level2_foodwebnum[k])-397]][i,j]==1){
        con.mass <- c(con.mass,level2_30nodes$mass[j+number[k]])
        res.mass <- c(res.mass,level2_30nodes$mass[i+number[k]])
      }
    }
  }
}
R.C.slope.data <- as.data.frame(con.mass)
R.C.slope.data[,2] <- res.mass
names(R.C.slope.data) <- c('con.mass','res.mass')

position_level2 <- c()
position_level2[1] <- 0
position_level2[2] <- 70
for(i in 3:101){
  position_level2[i] <- level2_summary[i-1,6]+position_level2[i-1]
}
R.C.slope.level2 <- list()
for(i in 1:100){
  R.C.slope.level2[[i]] <- R.C.slope.data[(position_level2[i]+1):(position_level2[i+1]),]
}

R.C.slope <- c()
for(i in 1:100){
  R.C.slope[i] <- summary(lm(formula=log10(R.C.slope.level2[[i]][1:level2_summary$V6[i],2])~log10(R.C.slope.level2[[i]][1:level2_summary$V6[i],1])))[[4]][2]
}
level2_summary[,18] <- R.C.slope

##R.C.cor 
R.C.cor <- c()
for(i in 1:100){
  R.C.cor[i] <- cor(log10(R.C.slope.level2[[i]][1:level2_summary$V6[i],2]),log10(R.C.slope.level2[[i]][1:level2_summary$V6[i],1]))
}
level2_summary[,19] <- R.C.cor

#R.C.R2 
R.C.R2 <- c()
for(i in 1:100){
  R.C.R2[i] <- summary(lm(formula = log10(R.C.slope.level2[[i]][1:level2_summary$V6[i],2])~log10(R.C.slope.level2[[i]][1:level2_summary$V6[i],1])))[[8]]
}
level2_summary[,20] <- R.C.R2

#R.C.slope.R2
level2_summary[,21] <- level2_summary[,18]*level2_summary[,20]

#Prop.C.big.R.small
Prop.C.big.R.small <- c()
for(i in 1:100){
  Prop.C.big.R.small[i] <- (sum(R.C.slope.level2[[i]][1:level2_summary$V6[i],1]>R.C.slope.level2[[i]][1:level2_summary$V6[i],2]))/level2_summary[i,6]
}
level2_summary[,22] <- Prop.C.big.R.small

names(level2_summary) <- c('web.name','num.sp','num.B','num.T','num.I',
                           'num.link','num.TI','num.TB','num.II','num.IB',
                           'Prop.T','Prop.TI','Prop.TB','Prop.II','Prop.IB','T.score',
                           'mean.mass','R.C.slope','R.C.cor','R.C.R2','R.C.slope.R2',
                           'Prop.C.big.R.small')
level2_summary[is.na(level2_summary)] <- 0

#level3
#web.name
level3_summary <- as.data.frame(unique(level3_30nodes$foodweb))

#num.sp
num.sp <- table(level3_30nodes$foodweb)[[1]]
for(i in 2:9){
  num.sp <- c(num.sp,table(level3_30nodes$foodweb)[[i]])
  num.sp
}
level3_summary[,2] <- num.sp

#num.B
datanum.B <- filter(level3_30nodes,b<0)
num.B <- table(datanum.B$foodweb)[[1]]
for(i in 2:9){
  num.B <- c(num.B,table(datanum.B$foodweb)[[i]])
  num.B
}
level3_summary[,3] <- num.B

#num.T
a <- list()
for(i in level3_summary$`unique(level3_30nodes$foodweb)`){
  count <- 0
  for(j in 1:nrow(level3_binary_matrix[[i-679]])){
    if(sum(level3_binary_matrix[[i-679]][j,]==1)==0){
      count <- count+1
    }else{
      count <- count
    }
  }
  a[[i]] <- count
}

num.T <- c()
for(i in level3_summary$`unique(level3_30nodes$foodweb)`){
  num.T <- c(num.T,a[[i]])
}
level3_summary[,4] <- num.T

#num.I
num.I <- c()
num.I <- level3_summary$V2-level3_summary$V3-level3_summary$V4
level3_summary[,5] <- num.I

#num.link
num.link <- c()
for(i in level3_summary$`unique(level3_30nodes$foodweb)`){
  num.link <- c(num.link,sum(level3_binary_matrix[[i-679]]))
}
level3_summary[,6] <- num.link

#get B.num, T.num, I.num
list.B.num <- list()
for(k in unique(level3_30nodes$foodweb)){
  B.num <- c()
  for(i in 1:nrow(level3_binary_matrix[[k-679]])){
    if((sum(level3_binary_matrix[[k-679]][,i]==1))==0){
      B.num <- c(B.num,i)
    }else{
      B.num <- B.num
    }
  }
  list.B.num[[k-679]] <- B.num
}

list.T.num <- list()
for(k in unique(level3_30nodes$foodweb)){
  T.num <- c()
  for(i in 1:nrow(level3_binary_matrix[[k-679]])){
    if((sum(level3_binary_matrix[[k-679]][i,]==1))==0){
      T.num <- c(T.num,i)
    }else{
      T.num <- T.num
    }
  }
  list.T.num[[k-679]] <- T.num
}

list.I.num <- list()
for(k in unique(level3_30nodes$foodweb)){
  I.num <- c()
  I.num <- setdiff(c(1:nrow(level3_binary_matrix[[k-679]])),c(list.B.num[[k-679]],list.T.num[[k-679]]))
  list.I.num[[k-679]] <- I.num
}


#num.TI
list.num.TI <- list()
for(k in unique(level3_30nodes$foodweb)){
  num.TI <- 0
  for(i in list.I.num[[k-679]]){
    for(j in list.T.num[[k-679]]){
      if(level3_binary_matrix[[k-679]][i,j]==1){
        num.TI <- num.TI+1
      }else{
        num.TI <- num.TI
      }
    }
  }
  list.num.TI[[k-679]] <- num.TI
}
num.TI <- c()
for(k in unique(level3_30nodes$foodweb)){
  num.TI <- c(num.TI,list.num.TI[[k-679]])
}
level3_summary[,7] <- num.TI

#num.TB
list.num.TB <- list()
for(k in unique(level3_30nodes$foodweb)){
  num.TB <- 0
  for(i in list.B.num[[k-679]]){
    for(j in list.T.num[[k-679]]){
      if(level3_binary_matrix[[k-679]][i,j]==1){
        num.TB <- num.TB+1
      }else{
        num.TB <- num.TB
      }
    }
  }
  list.num.TB[[k-679]] <- num.TB
}
num.TB <- c()
for(k in unique(level3_30nodes$foodweb)){
  num.TB <- c(num.TB,list.num.TB[[k-679]])
}
level3_summary[,8] <- num.TB


#num.II
list.num.II <- list()
for(k in unique(level3_30nodes$foodweb)){
  num.II <- 0
  for(i in list.I.num[[k-679]]){
    for(j in list.I.num[[k-679]]){
      if(level3_binary_matrix[[k-679]][i,j]==1){
        num.II <- num.II+1
      }else{
        num.II <- num.II
      }
    }
  }
  list.num.II[[k-679]] <- num.II
}
num.II <- c()
for(k in unique(level3_30nodes$foodweb)){
  num.II <- c(num.II,list.num.II[[k-679]])
}
level3_summary[,9] <- num.II

#num.IB
list.num.IB <- list()
for(k in unique(level3_30nodes$foodweb)){
  num.IB <- 0
  for(i in list.B.num[[k-679]]){
    for(j in list.I.num[[k-679]]){
      if(level3_binary_matrix[[k-679]][i,j]==1){
        num.IB <- num.IB+1
      }else{
        num.IB <- num.IB
      }
    }
  }
  list.num.IB[[k-679]] <- num.IB
}
num.IB <- c()
for(k in unique(level3_30nodes$foodweb)){
  num.IB <- c(num.IB,list.num.IB[[k-679]])
}
level3_summary[,10] <- num.IB


#Prop.T (num.T/num.sp)
Prop.T <- level3_summary[,4]/level3_summary[,2]
level3_summary[,11] <- Prop.T

#Prop.TI (num.TI/num.link)
Prop.TI <- level3_summary[,7]/level3_summary[,6]
level3_summary[,12] <- Prop.TI

#Prop.TB (num.TB/num.link)
Prop.TB <- level3_summary[,8]/level3_summary[,6]
level3_summary[,13] <- Prop.TB

#Prop.II (num.II/num.link)
Prop.II <- level3_summary[,9]/level3_summary[,6]
level3_summary[,14] <- Prop.II

#Prop.IB (num.IB/num.link)
Prop.IB <- level3_summary[,10]/level3_summary[,6]
level3_summary[,15] <- Prop.IB

#T.score (Prop.T-0.5*Prop.T*(num.TB/(num.T*num.B)))
T.score <- level3_summary[,11]-0.5*level3_summary[,11]*(level3_summary[,8]/(level3_summary[,4]*level3_summary[,3]))
level3_summary[,16] <- T.score

#mean.mass
mean.mass <- aggregate(level3_30nodes$mass,by=list(foodweb=level3_30nodes$foodweb),mean)[,2]
level3_summary[,17] <- mean.mass

#R.C.slope
con.mass <- c()
res.mass <- c()
number <- c()
number[1] <- 0
number[2] <- 42
for(i in 3:9){
  number[i] <- level3_summary[i-1,2]+number[i-1]
}
level3_foodwebnum <- unique(level3_30nodes$foodweb)
for(k in 1:length(level3_foodwebnum)){
  for(i in 1:sqrt(length(level3_binary_matrix[[(level3_foodwebnum[k])-679]]))){
    for(j in 1:sqrt(length(level3_binary_matrix[[(level3_foodwebnum[k])-679]]))){
      if(level3_binary_matrix[[(level3_foodwebnum[k])-679]][i,j]==1){
        con.mass <- c(con.mass,level3_30nodes$mass[j+number[k]])
        res.mass <- c(res.mass,level3_30nodes$mass[i+number[k]])
      }
    }
  }
}

R.C.slope.data <- as.data.frame(con.mass)
R.C.slope.data[,2] <- res.mass
names(R.C.slope.data) <- c('con.mass','res.mass')

position_level3 <- c()
position_level3[1] <- 0
position_level3[2] <- 711
for(i in 3:10){
  position_level3[i] <- level3_summary[i-1,6]+position_level3[i-1]
}
R.C.slope.level3 <- list()
for(i in 1:9){
  R.C.slope.level3[[i]] <- R.C.slope.data[(position_level3[i]+1):(position_level3[i+1]),]
}

R.C.slope <- c()
for(i in 1:9){
  R.C.slope[i] <- summary(lm(formula=log10(R.C.slope.level3[[i]][1:level3_summary$V6[i],2])~log10(R.C.slope.level3[[i]][1:level3_summary$V6[i],1])))[[4]][2]
}
level3_summary[,18] <- R.C.slope

##R.C.cor 
R.C.cor <- c()
for(i in 1:9){
  R.C.cor[i] <- cor(log10(R.C.slope.level3[[i]][1:level3_summary$V6[i],2]),log10(R.C.slope.level3[[i]][1:level3_summary$V6[i],1]))
}
level3_summary[,19] <- R.C.cor

#R.C.R2 
R.C.R2 <- c()
for(i in 1:9){
  R.C.R2[i] <- summary(lm(formula = log10(R.C.slope.level3[[i]][1:level3_summary$V6[i],2])~log10(R.C.slope.level3[[i]][1:level3_summary$V6[i],1])))[[8]]
}
level3_summary[,20] <- R.C.R2

#R.C.slope.R2
level3_summary[,21] <- level3_summary[,18]*level3_summary[,20]

#Prop.C.big.R.small
Prop.C.big.R.small <- c()
for(i in 1:9){
  Prop.C.big.R.small[i] <- (sum(R.C.slope.level3[[i]][1:level3_summary$V6[i],1]>R.C.slope.level3[[i]][1:level3_summary$V6[i],2]))/level3_summary[i,6]
}
level3_summary[,22] <- Prop.C.big.R.small

names(level3_summary) <- c('web.name','num.sp','num.B','num.T','num.I',
                           'num.link','num.TI','num.TB','num.II','num.IB',
                           'Prop.T','Prop.TI','Prop.TB','Prop.II','Prop.IB','T.score',
                           'mean.mass','R.C.slope','R.C.cor','R.C.R2','R.C.slope.R2',
                           'Prop.C.big.R.small')

level3_summary[is.na(level3_summary)] <- 0

save(level1_summary,level2_summary,level3_summary,file='EcoBuildersummary30nodes.RData')
