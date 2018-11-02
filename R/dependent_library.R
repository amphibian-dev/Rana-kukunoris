
library(lubridate)
library(magrittr)
library(dplyr)
library(randomForest)
library(pROC)
library(car)
library(ROCR)
library(smbinning)
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

