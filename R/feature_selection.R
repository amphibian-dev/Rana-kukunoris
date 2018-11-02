##################自动计算vif ####################################    v5版本加入
# data中需要target
auto_vif<-function(data){
  library(car)
  data[data=="NaN"]<-0
  data[data=="Inf"]<-0
  data[is.na(data)]<-0
  target<- data$target
  lm1<- lm(target~.,data = data)
  
  after_delete<-na.omit(lm1$coefficients)
  m<-names(after_delete)[1]
  for (i in 2:length(after_delete)) {
    m<-paste0(m,"|",names(after_delete)[i])
  }
  m<-sub(" ","",m)
  col<-grep(m,names(data))
  
  data<- data[,col]
  while(1){
    
    lm<-lm(target~.,data)
    if(is.matrix(vif(lm))){
      tmp<-vif(lm)%>%as.data.frame()
      if(max(tmp$`GVIF^(1/(2*Df))`)>3){
        data<-data[,-grep(rownames(tmp[which.max(tmp$`GVIF^(1/(2*Df))`),]),names(data))]
      }else{
        break
      }
    }else{
      tmp<-vif(lm)
      if(max(tmp)>3){
        data<-data[,-grep(names(tmp)[which.max(tmp)],names(data))]
      }else{
        break
      }
    }
  }
  return(tmp)
}


