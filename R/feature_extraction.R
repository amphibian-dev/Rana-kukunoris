
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

#cross check
cross_extraction<-function(df,vars){

  df1 <- df[,c(vars)]

  tmp <- ""
  for (i in 1:length(df1)) {
   tmp <- paste( tmp,df1[,i],sep = "&")
  }

  return(tmp)
}

#newf1<- cross_extraction(data,c("educationdegree","corp_type","industrytype"))

#z_extraction 衍生 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mean_new <- function(data){
  tmp_mean<-mean(data,na.rm=T)
  res <- ifelse(data>=tmp_mean,1,0)
  return(res)
}
diff_new <- function(data){
  tmp_mean<-mean(data,na.rm=T)
  res <- (data-tmp_mean)/tmp_mean
  return(res)
}


z_extraction<-function(df,flag,charvars,numvars){
  # df <- data
  # charvars <- c("educationdegree")
  # numvars <- c("mortgagesum","id")
  #
  total <- c()
  k <- df[,charvars]%>%as.data.frame()
  nk <- df[,numvars]%>%as.data.frame()

  names(k) <- charvars
  names(nk) <- numvars

  if(flag=="train"){
    #train——衍生 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tmean <- c()

    tmp <- nk %>%cbind(group=k[,i])
    tmp_levels<-unique(tmp$group)
    part_1 <- c()
    part_2 <- c()
    for(level in tmp_levels){
      #level <- tmp_levels[1]
      tmp_data<-tmp%>%filter(group==level)%>%select(-group)
      tmp_data1 <- tmp_data%>%select(-id)

      mean <- apply(tmp_data1,2,mean,na.rm=T)
      mean%<>%as.data.frame()
      names(mean)[1]<-"mean"
      mean%<>%cbind(var=row.names(mean))
      mean%<>%mutate(fact=level)%>%mutate(group=names(k)[i])


      tmp_mean <- apply(tmp_data1,2,mean_new)
      tmp_diff <- apply(tmp_data1,2,diff_new)

      if(is.null(nrow(tmp_mean))){
        tmp_mean <- tmp_mean%>%t()
        tmp_mean %<>%cbind(id=as.character(tmp_data$id))
        tmp_diff <- tmp_diff%>%t()
        tmp_diff %<>%cbind(id=as.character(tmp_data$id))

      }else{
        tmp_mean%<>%cbind(id=as.character(tmp_data$id))
        tmp_diff%<>%cbind(id=as.character(tmp_data$id))
      }

      if(is.null(part_1)){
        tmp_mean%<>%as.data.frame()
        tmp_diff%<>%as.data.frame()
        part_1 <- tmp_mean
        part_2 <- tmp_diff
      }else{
        part_1%<>%rbind(tmp_mean)
        part_2%<>%rbind(tmp_diff)
      }
      if(is.null(tmean)){
        tmean <- mean
      }else{
        tmean%<>%rbind(mean)
      }
    }
    names(part_1)[1:(length(part_1)-1)] <- paste0("P1_",names(k)[i],"_",names(part_1)[1:(length(part_1)-1)])
    names(part_2)[1:(length(part_1)-1)] <- paste0("P2_",names(k)[i],"_",names(part_2)[1:(length(part_1)-1)])
    if(is.null(total)){
      total <- part_1
      total %<>% left_join(part_2)
    }else{
      total%<>%left_join(part_1)
      total%<>%left_join(part_2)
    }


    f_name<-names(total%>%select(-id))
    for(name in f_name){
      total[,name] <- as.numeric(as.character(total[,name]))
    }

    write.csv(tmean,file = "tmp_z_tmean.csv",row.names = FALSE)
  }else if(flag=="test"){
    #test——衍生 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    for(i in 1:length(k)){
      #i = 1
      name <- names(k)[i]
      tmean <- read.csv(file = "tmp_z_tmean.csv")
      tmp_mean <- tmean%>%filter(group==name)
      tmp_data <- nk %>%cbind(group=k[,i])

      tmp_res <-c()
      tmp_fact<-unique(tmp_data$group)
      for(fact1 in tmp_fact){

        ttmp_mean <- tmp_mean%>%filter(fact==fact1)
        ttmp_data <- tmp_data%>%filter(group==fact1)

        ttmp_res <-c()

        if(nrow(ttmp_mean)==0){
          print("Other")
          tmp_mean1 <- apply(ttmp_data%>%select(-group,-id),2,mean_new)
          tmp_diff1 <- apply(ttmp_data%>%select(-group,-id),2,diff_new)

          if(is.null(nrow(tmp_mean1))){
            ttmp_res <- tmp_mean1%>%t()
            ttmp_res %<>%cbind(tmp_diff1%>%t())
          }else{
            tmp_mean1%<>%as.data.frame()
            tmp_diff1%<>%as.data.frame()
            ttmp_res <- tmp_mean1%>%cbind(tmp_diff1)
          }

          ttmp_res%<>%as.data.frame()
          names(ttmp_res) <- c(paste0("P1_",name,"_",names(ttmp_data%>%select(-group,-id))),paste0("P2_",name,"_",names(ttmp_data%>%select(-group,-id))))

          ttmp_res%<>%cbind(id=ttmp_data$id)
        }else{
          for(var1 in ttmp_mean$var){
            #var1 = ttmp_mean$var[1]
            tttmp_mean <- ttmp_mean %>% filter(var==var1)
            tttmp_data <- ttmp_data[,var1]

            res1 <- ifelse(tttmp_data>=tttmp_mean$mean,1,0)
            res2 <- (tttmp_data-tttmp_mean$mean)/tttmp_mean$mean

            if(is.null(ttmp_res)){
              ttmp_res <- res1 %>%as.data.frame()
              names(ttmp_res)[length(ttmp_res)]<-paste0("P1_",name,"_",var1)
              ttmp_res %<>% cbind(res2)
              names(ttmp_res)[length(ttmp_res)]<-paste0("P2_",name,"_",var1)
            }else{
              ttmp_res%<>%cbind(res1)
              names(ttmp_res)[length(ttmp_res)]<-paste0("P1_",name,"_",var1)
              ttmp_res%<>%cbind(res2)
              names(ttmp_res)[length(ttmp_res)]<-paste0("P2_",name,"_",var1)
            }
          }
          ttmp_res%<>%cbind(id=ttmp_data$id)
        }

        #
        if(is.null(tmp_res)){
          tmp_res <- ttmp_res
        }else{
          tmp_res %<>% rbind(ttmp_res)
        }
      }
      if(is.null(total)){
        total <- tmp_res
      }else{
        total %<>% left_join(tmp_res)
      }
    }
  }

  return(total)
}


# newf2<- z_extraction(data,"train","educationdegree",c("mortgagesum","id"))
#
# newf2<- z_extraction(data,"test","educationdegree",c("mortgagesum","id"))



