#
# library(lubridate)
# library(magrittr)
# library(dplyr)
# library(randomForest)
# library(pROC)
# library(car)
# library(ROCR)
# library(smbinning)
# # function: bmp, dev.off
# library(discretization)
# # function: chiSq
# library(glmnet)
# # function: cv.glmnet, coef.glmnet
# library(doMC)
# # function: registerDoMC
# registerDoMC(cores=4)
# # fucntion: ks_stat, ks_plot
# library(InformationValue)


get_target_str <- function(str,x,y){
  if(!is.numeric(x)){
    a <- as.numeric(gregexpr(x,str)[[1]][1])
  }else{
    a <- x
  }
  if(!is.numeric(y)){
    b <- as.numeric(gregexpr(y,str)[[1]][1])
  }else{
    b <- y
  }

  return(as.numeric(substr(str,a+1,b-1)))
}





#yutong ----- tmp function -------

bin_row <- function(df,s1,s2,Total_bad,Total_Good){
  #s1 = 2
  #s2 = 3

  # Total_bad = 1135
  # Total_Good = 19279
  #
  # df= af_no_can_history_max
  names(df)[1] <- "name"
  df%<>%mutate(id = as.numeric(row.names(df)))
  df$s <- apply(df[,1],1,get_target_str,x="()",y=",")
  df$e <- apply(df[,1],1,get_target_str,x=",",y="]")

  df%<>%select(-name)
  df1 <- df[s1:s2,]
  df2 <- df[-c(s1:s2),]
  df1%<>%mutate(k=1)%>%group_by(k)%>%
    summarise(fre=sum(fre),bad=sum(bad),good=sum(good),id=min(id),s=min(s),e=max(e))%>%select(-k)%>%
    mutate(bad_rate = bad/fre,woe=(log((bad/Total_bad)/(good/Total_Good),base = exp(1))))%>%
    mutate(iv=((bad/Total_bad)-(good/Total_Good))*woe)

  df2%<>%rbind(df1)%>%arrange(id)

  df2%<>%mutate(id=as.numeric(row.names(df2)))
  return(df2)
}


bin_row_2 <- function(dfa,s1,s2,Total_bad,Total_Good){
  dfa1 <- dfa[s1:s2,]
  dfa2 <- dfa[-c(s1:s2),]
  dfa1 %<>% summarise(fre=sum(fre),bad=sum(bad),good=sum(good),id=min(id),s=min(s),e=max(e))%>%
    mutate(bad_rate = bad/fre,woe=(log((bad/Total_bad)/(good/Total_Good),base = exp(1))))%>%
    mutate(iv=((bad/Total_bad)-(good/Total_Good))*woe)
  dfa2 %<>% rbind(dfa1) %>% arrange(id)
  dfa2%<>%mutate(id=as.numeric(row.names(dfa2)))

  return(dfa2)
}

bin_row_factor<- function (df, factors, Total_bad, Total_Good)
{

  #factors <- c("bin2","bin4")
  # df<- td_x183 %>% as.data.frame()
  # factors <- c(1,3)
  # Total_bad <- 1000
  # Total_Good <- 10000

  names(df)[1] <- "name"
  if(class(factors)!="character"){
    factors <- df$name[factors]
  }


  df %<>% mutate(id = as.numeric(row.names(df)))
  #df %<>% select(-name)
  #df1 <- df[s1:s2, ]

  df1 <- df %>%filter(name %in% factors)

  df2 <- df %>%filter(!name %in% factors)
  df1 %<>% mutate(k = 1) %>% group_by(k) %>% summarise(fre = sum(fre),
                                                       bad = sum(bad), good = sum(good), id = min(id)) %>% select(-k) %>% mutate(bad_rate = bad/fre,
                                                                                                                                 woe = (log((bad/Total_bad)/(good/Total_Good), base = exp(1)))) %>%
    mutate(iv = ((bad/Total_bad) - (good/Total_Good)) * woe)%>%as.data.frame()


  tmp_df <- c()
  for (i in 1:(length(factors))) {
    if(i == 1){
      tmp_df <- df1
    }else{
      tmp_df %<>%rbind(df1)
    }
  }


  tmp_df$name <- factors
  df2 %<>% rbind(tmp_df) %>% arrange(id)
  #df2 %<>% mutate(id = as.numeric(row.names(df2)))
  return(df2)
}


#连续型变量转离散型 dataframe,quantile的边界值，是否删除原连续变量
num_transfer <- function(df, arr = c(seq(0,1,0.2),0.01,0.99),delete_source=T){
  arr <- arr[order(arr)]
  #df<- data
  for (tmpname in names(df)[!factor_col_check(df)]){
    tmplimit<- quantile(df[,tmpname],arr,na.rm = T)
    df$tmp <- findInterval(df[,tmpname],tmplimit)

    tmp_interval<-c()
    for (i in 1:(length(tmplimit)-1)) {
      tmp_interval[i] <- paste0(i,".","[",tmplimit[i],",",tmplimit[i+1],")")
    }
    tmp <-c(1:(length(tmplimit)-1))
    tmparr <- data_frame(tmp,tmp_interval)

    df%<>%left_join(tmparr)%>%select(-tmp)

    names(df)[length(df)] <- paste0(tmpname,"_interval")
  }

  if(delete_source==T){
    df <- df[,c(names(df)[col_check(df,"factor")],names(df)[col_check(df,"character")])]
  }
  return(df)
}
