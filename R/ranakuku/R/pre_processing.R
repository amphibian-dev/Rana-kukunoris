
library(lubridate)
library(magrittr)
library(dplyr)
library(randomForest)
library(pROC)
library(car)
library(ROCR)
#library(smbinning)
# function: bmp, dev.off
library(discretization)
# function: chiSq
library(glmnet)
# function: cv.glmnet, coef.glmnet
library(doMC)
# function: registerDoMC
registerDoMC(cores=4)
# fucntion: ks_stat, ks_plot
library(InformationValue)


get_target_str <- function(str,x,y){
  if(!is.numeric(a)){
    a <- as.numeric(gregexpr(x,str)[[1]][1])
  }
  if(!is.numeric(b)){
    b <- as.numeric(gregexpr(y,str)[[1]][1])
  }
  return(as.numeric(substr(str,a+1,b-1)))
}
