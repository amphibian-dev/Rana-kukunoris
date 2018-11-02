
head(data)
#cross check
cross_extraction<-function(df,vars){

  df1 <- df[,c(vars)]
  
  tmp <- ""
  for (i in 1:length(df1)) {
   tmp <- paste( tmp,df1[,i],sep = "&")
  }
  
  return(tmp)
}

newf1<- cross_extraction(data,c("educationdegree","corp_type","industrytype"))

