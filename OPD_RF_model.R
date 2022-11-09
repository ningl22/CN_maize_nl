#OPD RF model training and OPD simulation wiht multi-GCMs
library(randomForest)
library(rio)
library(plyr)
library(tidyverse)
library(caret)

#1===OPD-RF model===============================================================
df <- import('.../data_RF.xlsx')

#select data for OPD RF-model
data <-  select(df, SOM, GDD, Radn, Tmax, Tmin, Prec, OPD)

#K folder
CVgroup <- function(k, datasize, seed){  
  cvlist <- list()  
  set.seed(seed)  
  n <- rep(1:k, ceiling(datasize/k))[1:datasize]   
  temp <- sample(n, datasize) 
  x <- 1:k  
  dataseq <- 1: datasize   
  cvlist <- lapply(x, function(x) dataseq[temp==x])  
  return(cvlist)  
}

datasize <- nrow(data)
cvlist <- CVgroup(k = 5, datasize = datasize, seed =1234)

outfile <- data.frame()
for (i in 1:5){
  print(i)
  train <- data[-cvlist[[i]], ]
  test <- data[cvlist[[i]], ]
  fit <- randomForest(OPD~., train, importance=TRUE, mtry = 3, ntree=500)
  train.pre <-  predict(fit, test)
  tem <- data.frame(obs = test$OPD, pred = train.pre)
  outfile <- rbind(outfile, tem)
}

rsq <- cor(outfile$obs, outfile$pred)^2
rmse <- RMSE(outfile$obs, outfile$pred)
rrmse <- 100*rmse/mean(outfile$obs)



#2===simulation with multi-GCMs data============================================

#read files
base <- import('.../data_RF.xlsx')
df <- import('.../GCMs_simu.xlsx')

#def function for SOM modifing
lambda <- function(x){ifelse(x<20, 20, x)}

set.seed(1234)
train <- select(base, SOM, GDD, Radn, Tmax, Tmin, Prec, OPD)  #select data
fit <- randomForest(OPD~., train,  importance=TRUE, mtry = 3, ntree=500) #RF

decades <- c('2010s', '2030s')
GCM <- unique(df$GCM)

outfile <- data.frame()
for(g in GCM){
  print(g)
  f <- filter(df, GCM == g)
  out <- filter(f, decades == '2010s')
  out1 <- select(out, region, Station, ID, GCM)
  for (d in decades){
    print(d)
    f1 <- filter(f, decades == d)
    
    f1.1 <- select(f1, SOM, GDD, Radn, Tmax, Tmin, Prec)
    pre1 <- predict(fit, f1.1)
    out1[paste0(d, '.optd')] <- pre1
    
    #improve SOM
    f2 <- f1
    f2$SOM <- lapply(f2$SOM, lambda)
    f2.1 <- select(f2, SOM, GDD, Radn, Tmax, Tmin, Prec)
    f2.1$SOM <- as.numeric(f2.1$SOM)
    
    pre2 <- predict(fit, f2.1)
    out1[paste0(d, '.optdsom')] <- pre2
  }
  outfile <- rbind(outfile, out1)
}  
  
#export(outfile, paste0(save, 'optd_GCMs.xlsx'))  