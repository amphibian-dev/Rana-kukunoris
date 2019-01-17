

#dt = dataframe , flag
yt_woe_iv <- function(dt,flag){

  # dt <- data %>% rename(target=target_m5)
  # va <- "signsum"
  # flag <- "numeric"

  Total_Good <- as.numeric(table(dt$target))[1]
  Total_bad <- as.numeric(table(dt$target))[2]

  woe_list <- list()
  if(flag=="numeric"|flag=="int"){

    if(flag=="numeric"){
      names<-names(dt)[col_check(dt,"numeric")]
    }else{
      names<-names(dt)[col_check(dt,"int")]
    }

    for (name in names) {
      q <- quantile(dt[,name],probs=c(1:20)*0.05)%>%as.data.frame()
      names(q)[1] <- "cuts"
      q %<>%group_by(cuts)%>%filter(row_number()==1)%>%ungroup()
      x <-(dim(q)[1])-1
      a <- q[c(1:x),]%>%as.matrix()
      cutPoint <- c(-Inf,a,Inf)

      cut <- cut(dt[,name],cutPoint)
      dt1 <- dt %>% mutate(cut)
      af <- dt1 %>% group_by(cut) %>% summarise(fre=n(),bad=sum(target),Good=as.numeric(table(target))[1])%>%
        mutate(bad_rate=bad/fre)%>%mutate(woe=(log((bad/Total_bad)/(Good/Total_Good),base = exp(1))))%>%
        mutate(iv=((bad/Total_bad) - (Good/Total_Good)) * woe)

      names(af)[1] <- "name"
      af %<>% mutate(id = as.numeric(row.names(af)))
      af$s <- apply(af[, 1], 1, get_target_str, x = "()", y = ",")
      af$e <- apply(af[, 1], 1, get_target_str, x = ",", y = "]")
      af %<>% select(-name)

      af$Total_bad <- Total_bad
      af$Total_Good <- Total_Good

      af[,name] <- af$id
      af%<>%select(-id)

      woe_list[[name]] <- af
    }}else{
      names<-names(dt)[col_check(dt,"factor")]

      for (i in names) {
        cat(i)
        af <- dt %>% group_by(dt[, i])%>%
          summarise(fre=n(),bad=sum(target),good=as.numeric(table(target))[1])%>%
          mutate(bad_rate=bad/fre)%>%
          mutate(woe=(log((bad/Total_bad)/(good/Total_Good),base = exp(1))))%>%
          mutate(iv=((bad/Total_bad) - (good/Total_Good)) * woe)

        af$Total_bad <- Total_bad
        af$Total_Good <- Total_Good

        names(af)[1] <- i
        names(af)[6] <- paste0(i,".woe")

        woe_list[[i]] <- af
      }

    }


  return(woe_list)
}
#df: dataframe, s1:start,s2:end
yt_bin_row<- function (df, s1, s2)
{
  # df <- df2
  # s1 <- 2
  # s2 <- 3
  Total_bad <- min(df$Total_bad)
  Total_Good <- min(df$Total_Good)
  name <- names(df)[length(df)]
  names(df)[length(df)] <- "id"

  names(df)[5] <-"woe"
  df1 <- df[s1:s2, ]
  df2 <- df[-c(s1:s2), ]
  df1 %<>% mutate(k = 1) %>% group_by(k) %>% summarise(fre = sum(fre),
                                                       bad = sum(bad), Good = sum(Good), id = min(id),Total_bad=min(Total_bad),Total_Good=min(Total_Good), s = min(s),
                                                       e = max(e)) %>% select(-k) %>% mutate(bad_rate = bad/fre,
                                                                                             woe = (log((bad/Total_bad)/(Good/Total_Good), base = exp(1)))) %>%
    mutate(iv = ((bad/Total_bad) - (Good/Total_Good)) * woe)
  df2 %<>% rbind(df1) %>% arrange(id)
  df2 %<>% mutate(id = as.numeric(row.names(df2)))

  df2[,name] <- df2$id
  df2%<>%select(-id)
  names(df2)[5] <- paste0(name,".woe")

  return(df2)
}

# error: df & bad $good
#df:dataframe, factors:c("bin2","bin4")
yt_bin_row_factor<- function (df, factors)
{
  #factors <- c("bin2","bin4")
  # df<- td_x183 %>% as.data.frame()
  # factors <- c(1,3)
  # Total_bad <- 1000
  # Total_Good <- 10000

  name <- names(df)[1]
  name1 <- names(df)[6]

  names(df)[1] <- "name"
  names(df)[6] <- "woe"

  if(class(factors)!="character"){
    factors <- df$name[factors]
  }


  df %<>% mutate(id = as.numeric(row.names(df)))
  #df %<>% select(-name)
  #df1 <- df[s1:s2, ]

  df1 <- df %>%filter(name %in% factors)

  df2 <- df %>%filter(!name %in% factors)
  df1 %<>% mutate(k = 1) %>% group_by(k) %>% summarise(fre = sum(fre),
                                                       bad = sum(bad), good = sum(good), Total_bad=min(Total_bad),Total_Good=min(Total_Good),id = min(id)) %>% select(-k) %>% mutate(bad_rate = bad/fre,
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

  names(df2)[1] <- name
  names(df2)[6] <- name1
  #df2 %<>% mutate(id = as.numeric(row.names(df2)))
  return(df2)
}

#df:dataframe, list:woe&cuts ,flag: numeric or factor
#update: numeric left.open = T,rightmost.closed = T
yt_replace_woe <- function(df,list,flag){

  # df <- data_01_zy
  # list <- woe_info
  if(flag=="numeric"|flag=="int"){
    for (name in names(list)) {
      #name <- "signsum"
      x <- unique(c(list[[name]]$s,list[[name]]$e))
      df[,name] <- findInterval(df[,name],x,left.open = T,rightmost.closed = T)

      df%<>%left_join(list[[name]][,c(5,11)])
    }

    tmpname <- setdiff(names(df),names(list))
  }
  else{
    for (name in names(list)){
      df %<>%left_join (list[[name]][,c(1,6)])
    }

    tmpname <- setdiff(names(df),names(list))
  }


  return(df[,tmpname])
}

#list_num,list_char
yt_json_cuts<- function(list_num,list_char,filename){
  # list_char <- list_char_woe
  # list_num <- list_num_woe

  tmp <- list()
  dif <- c(-Inf,Inf)

  for (name in names(list_num)) {
    tmp[[name]] <-setdiff(unique(c(list_num[[name]]$s,list_num[[name]]$e)),dif)
  }

  for (name in names(list_char)) {
    #name = "citylevel"
    tmpdata <- list_char[[name]]
    names(tmpdata)[1] <- "name"

    tmpdata$name <- enc2utf8(as.character(tmpdata$name))

    t<- tmpdata%>%group_by(bad_rate)%>%summarise(fre=n(),arr = ifelse(fre!=1,list(name),list(c(name,"stupid_factor"))))

    tmp[[name]]  <- t$arr
  }

  tmp <- sub(",\"stupid_factor\"","",toJSON(tmp))

  cat(toJSON(tmp), file = (con <- file(filename, "w", encoding = "UTF-8")))
  return(tmp)
}

yt_refit_fixbin <- function(df, bin_arr){

}

yt_refit_showbin <- function(df, scorecard){

}

#return character
get_target_str1 <- function(str,x,y){
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

  return(as.character(substr(str,a+1,b-1)))
}

#transform weipeng's json scorecard to dataframe
json_scorecard <- function(jsonsc){

  jsonsc1 <- unlist(jsonsc)
  jsonsc1%<>%as.data.frame()
  jsonsc1%<>%mutate(vars=row.names(jsonsc1))
  names(jsonsc1)[1] <- "value"

  jsonsc1$start <- NA
  jsonsc1$end <- NA
  jsonsc1$levels <- NA
  for (i in 1:nrow(jsonsc1)) {
    #i = 10
    jsonsc1$varsname[i] <- get_target_str1(jsonsc1$vars[i],0,"\\.")
    jsonsc1$content[i] <- get_target_str1(jsonsc1$vars[i],"\\.",nchar(jsonsc1$vars[i])+1)

    if (grepl("~",jsonsc1$content[i])) {
      jsonsc1$start[i] <- get_target_str1(jsonsc1$content[i],1,"~")
      jsonsc1$end[i] <- get_target_str1(jsonsc1$content[i],"~",")")
    }else{
      jsonsc1$levels[i] <- strsplit(jsonsc1$content[i],",")
    }

  }
  jsonsc1$start <- as.numeric(jsonsc1$start)
  jsonsc1$end <- as.numeric(jsonsc1$end)

  jsonsc1 %<>% group_by(varsname)%>%mutate(interval=1:n())%>%ungroup()
  return(jsonsc1)
}

# using the scorecard dataframe from(json_scorecard) in dataframe
# new_feature
use_scorecard<- function(df,scorecard) {
  # scorecard <- scorecard
  # df <- df
  #
  vars <- unique(scorecard$varsname)

  for (name in vars) {
    #name <- "VAR_49"
    if(is.numeric(df[,name])|is.integer(df[,name])){
      tmpsc<- scorecard%>%filter(varsname==name)
      tmplimit <- unique(tmpsc$start,tmpsc$end)
      df[,"interval"]<- findInterval(df[,name],tmplimit,left.open = FALSE,rightmost.closed =FALSE )
      df %<>%left_join(tmpsc%>%select(interval,content)) %>%select(-interval)
      names(df)[length(df)] <- paste0("new_",name)
    }else{
      tmpsc<- scorecard%>%filter(varsname==name)
      df$interval <- NA
      for (i in 1:nrow(tmpsc)) {
        df %<>%mutate(interval=ifelse(df[,name] %in% tmpsc$levels[i][[1]],tmpsc$interval[i],interval))
      }
      df %<>% left_join(tmpsc%>%select(interval,content)) %>%select(-interval)
      names(df)[length(df)] <- paste0("new_",name)
    }
  }
  return(df)
}

#fix the num of monthes we want to obvious
group_mth<- function(df1,mth=2){
  df1$fix_group <-NA
  i <- 1
  k <- 1
  while (i<=nrow(df1)) {
    for (j in 1:mth) {
      if(i > nrow(df1)){
        break
      }

      df1$fix_group[i] <- k
      i<-i+1

    }
    k <- k+1
  }

  return(df1)
}

#show the bin distribution by fixed mth and df from (use_scorecard)
yt_refit_showbin <- function(df,mth=3){
  #df <- data
  showtext_auto(enable=T)
  df <- df[,names(df)[grepl("new_|target|LOAN_MTH",names(df))]]
  df1 <-  df %>% group_by(LOAN_MTH)%>%summarise(fre=n()) %>%ungroup()%>%arrange(LOAN_MTH)
  mth_group<- group_mth(df1%>%select(-fre),mth)

  df %<>%left_join(mth_group)

  res <- NULL
  for (name in names(df)[grepl("new_",names(df))]) {
    #name <- "new_VAR_15"
    tmp_group <- df %>% group_by_at(c(name,"fix_group")) %>% summarise(fre=n(),bad=sum(target))%>%
      mutate(badrate=bad/fre) %>%mutate(vars = name)%>%ungroup()
    names(tmp_group)[1] <- "levels"

    ggplot(tmp_group, aes(x=fix_group, y=badrate, colour=levels,group=levels))+geom_line(size=1)+
      theme(text = element_text(family = 'SimSun'))
    ggsave(paste0(name,".png"), dpi=300)

    ggplot(tmp_group, aes(x=fix_group, y=fre, colour=levels,group=levels,fill = levels)) +
      theme(text = element_text(family = 'SimSun'))+ geom_bar(stat = "identity",colour="black")
    ggsave(paste0(name,"_fre.png"), dpi=300)

    if(is.null(res)){
      res <- tmp_group
    }else{
      res %<>%rbind(tmp_group)
    }
  }


  return(res)
}


#scorecard from (json_scorecard)
#var <- "VAR_15"
#numeric or int
#cut_arr <- c(8,16)
#character or factor
#cut_arr <- list(c("硕士及以上"),c("本科"),c("大专","中专/技校/高中"),c("高中以下","MISSING"))
yt_refit_fixbin <- function(scorecard,var,cut_arr){
  #var<-"VAR_30"
  #cut_arr <- c(8,16)
  #var <- "VAR_15"
  #cut_arr <- list(c("硕士及以上"),c("本科"),c("大专","中专/技校/高中"),c("高中以下","MISSING"))
  if(is.numeric(cut_arr)|is.integer(cut_arr)){
    old_scorecard <- scorecard %>% filter(varsname != var)
    new_scorecard <- data_frame(varsname=var,interval=c(1:(length(cut_arr)+1)))
    new_scorecard$start <- c(-Inf,cut_arr)
    new_scorecard$end <- c(cut_arr,Inf)
    new_scorecard$levels <- list(NULL)
    new_scorecard$vars <- NA
    new_scorecard$value <-NA
    new_scorecard$content <- paste0("[",new_scorecard$start," ~ ",new_scorecard$end,")")
  }else{
    old_scorecard <- scorecard %>% filter(varsname != var)
    new_scorecard <- data_frame(varsname=var,interval=c(1:length(cut_arr)))
    new_scorecard$start <- NA
    new_scorecard$end <- NA
    new_scorecard$levels <- cut_arr
    new_scorecard$vars <- NA
    new_scorecard$value <-NA
    new_scorecard$content <-NA
    for (i in 1:nrow(new_scorecard)) {
      for (j in 1:length(new_scorecard$levels[i][[1]])) {
        new_scorecard$content[i] <- paste0(new_scorecard$content[i],",",new_scorecard$levels[i][[1]][j])
      }
      new_scorecard$content[i] <- substr(new_scorecard$content[i] ,4,nchar(new_scorecard$content[i]))
    }
  }
  base <- nrow(old_scorecard)
  for (i in 1:nrow(new_scorecard)) {
    for (j in names(new_scorecard)) {
      old_scorecard[base+i,j] = new_scorecard[i,j]
    }

  }
  # new_scorecard %<>%rbind(old_scorecard)

  return(old_scorecard)
}

#
#df, varsname
#df from use_scorecard , varsname use the new_featurename
yt_refit_showdetail_num <- function(df,varsname,sonbin = 3 ,mth=3){
  # df <- data
  # varsname <- "new_VAR_49"
  #
  ovarname <- substr(varsname,5,nchar(varsname))
  tmpdata <-  df[,c(varsname,ovarname,"target","LOAN_MTH")]
  tmpdata1 <-  tmpdata %>% group_by(LOAN_MTH)%>%summarise(fre=n()) %>%ungroup()%>%arrange(LOAN_MTH)
  mth_group<- group_mth(tmpdata1%>%select(-fre),mth)

  tmpdata %<>%left_join(mth_group)

  #unique(tmpdata[,varsname] )
  limit <- NULL
  for (var in unique(tmpdata[,varsname])) {
    #var <- "[4 ~ 8)"
    tmpdata1 <- tmpdata %>% filter(tmpdata[,varsname]==var)
    tmplimit <- quantile(tmpdata1[,ovarname],seq(0,1,1/sonbin))
    if(is.null(limit)){
      limit <- tmplimit
    }else{
      limit <- c(limit,tmplimit)
    }

  }

  limit <- sort( unique(limit))
  limit2 <- data_frame(interval=c(1:length(limit)),start=c(limit),end=c(limit[2:length(limit)],Inf))
  limit2$interval2 <- paste0("[",limit2$start,",",limit2$end,")")
  limit2%<>%select(interval,interval2)
  tmpdata$interval <- findInterval(tmpdata[,ovarname],limit,left.open = FALSE,rightmost.closed =FALSE)
  tmpdata %<>%left_join(limit2)%>%select(-interval)

  # tmp_group <- df %>% group_by_at(c(name,"fix_group")) %>% summarise(fre=n(),bad=sum(target))%>%
  #   mutate(badrate=bad/fre) %>%mutate(vars = name)%>%ungroup()
  # names(tmp_group)[1] <- "levels"
  #
  # ggplot(tmp_group, aes(x=factor(fix_group), y=badrate, colour=levels,group=levels))+geom_line(size=1)+theme(text = element_text(family = 'SimSun'))
  # ggsave(paste0(name,".png"), dpi=300)

  tmp_group <- tmpdata %>% group_by_at(c("interval2","fix_group")) %>% summarise(fre=n(),bad=sum(target))%>%
    mutate(badrate=bad/fre) %>%mutate(vars = varsname)%>%ungroup()
  #
  # this function regenerates ggplot2 default colors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  #
  color_lty_cross = expand.grid(
    ltypes = 1:6,
    colors = gg_color_hue(10)
    ,stringsAsFactors = F)


  ggplot(tmp_group, aes(x=fix_group, y=badrate, col=as.character(interval2), lty = as.character(interval2))) +
    geom_line() + geom_point(size=3)
  #   scale_color_manual(values = color_lty_cross$colors[1:length(unique(tmp_group$interval))]) +
  #   scale_linetype_manual(values = color_lty_cross$ltypes[1:length(unique(tmp_group$interval))]) +
  #   theme_bw()
  #
  # ggplot(tmp_group, aes(x=fix_group, y=badrate, colour=interval,group=interval))+
  #   geom_line(size=1)+theme(text = element_text(family = 'wqy-microhei'))+geom_point(size=2)+
  #   theme_bw()
  #theme(legend.title=element_text(face="italic", family="Times", colour="red",size=14))
  #scale_fill_gradientn(colours=c("blue","green","yellow","red"))
  #scale_fill_brewer(palette="YlOrRd")
  ggsave(paste0(varsname,"_detail.png"), dpi=300)

  ggplot(tmp_group, aes(x=fix_group, y=fre, col=as.character(interval2), fill = as.character(interval2))) +
    geom_bar(stat = "identity",colour="black")#fill=as.factor(as.character(tmp_group$interval2)))

  ggsave(paste0(varsname,"_fre_detail.png"), dpi=300)
}

#df, varsname
#df from use_scorecard , varsname use the old featurename
yt_refit_showdetail_char <- function(df,varsname,sonbin = 3 ,mth=3){
  # df <- data
  # varsname <- "VAR_15"
  #
  #ovarname <- substr(varsname,5,nchar(varsname))
  tmpdata <-  df[,c(varsname,"target","LOAN_MTH")]
  tmpdata1 <-  tmpdata %>% group_by(LOAN_MTH)%>%summarise(fre=n()) %>%ungroup()%>%arrange(LOAN_MTH)
  mth_group<- group_mth(tmpdata1%>%select(-fre),mth)

  tmpdata %<>%left_join(mth_group)

  tmp_group <- tmpdata %>% group_by_at(c(varsname,"fix_group")) %>% summarise(fre=n(),bad=sum(target))%>%
    mutate(badrate=bad/fre) %>%mutate(vars = varsname)%>%ungroup()
  names(tmp_group)[1] <- "levels"
  tmp_group %<>%mutate(levels=as.character(levels))

  # this function regenerates ggplot2 default colors
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  #
  color_lty_cross = expand.grid(
    ltypes = 1:6,
    colors = gg_color_hue(10)
    ,stringsAsFactors = F)
  showtext_auto(enable=T)
  ggplot(tmp_group, aes(x=fix_group, y=badrate, col=levels, lty = levels)) +
    geom_line() + geom_point(size=3) +theme(text = element_text(family = 'wqy-microhei'))
  #   scale_color_manual(values = color_lty_cross$colors[1:length(unique(tmp_group$interval))]) +
  #   scale_linetype_manual(values = color_lty_cross$ltypes[1:length(unique(tmp_group$interval))]) +
  #   theme_bw()
  #
  # ggplot(tmp_group, aes(x=fix_group, y=badrate, colour=interval,group=interval))+
  #   geom_line(size=1)+theme(text = element_text(family = 'wqy-microhei'))+geom_point(size=2)+
  #   theme_bw()
  #theme(legend.title=element_text(face="italic", family="Times", colour="red",size=14))
  #scale_fill_gradientn(colours=c("blue","green","yellow","red"))
  #scale_fill_brewer(palette="YlOrRd")
  ggsave(paste0(varsname,"_detail.png"), dpi=300)

  ggplot(tmp_group, aes(x=fix_group, y=fre, col=levels, fill = levels)) +
    geom_bar(stat = "identity",colour="black")+theme(text = element_text(family = 'wqy-microhei'))#fill=as.factor(as.character(tmp_group$interval2)))

  ggsave(paste0(varsname,"_fre_detail.png"), dpi=300)
}

yt_refit_showcompare <- function(df,cutmth=201711){
  #df <- data
  showtext_auto(enable=T)
  df <- df[,names(df)[grepl("new_|target|LOAN_MTH",names(df))]]

  df %<>% mutate(fix_group = ifelse(LOAN_MTH <=cutmth,"train","test"))

  res <- NULL
  for (name in names(df)[grepl("new_",names(df))]) {
    #name <- "new_VAR_15"
    tmp_group <- df %>% group_by_at(c(name,"fix_group")) %>% summarise(fre=n(),bad=sum(target))%>%
      mutate(badrate=bad/fre) %>%mutate(vars = name)%>%ungroup()
    names(tmp_group)[1] <- "levels"

    ggplot(tmp_group, aes(x=fix_group, y=badrate, colour=levels,group=levels))+geom_line(size=1)+
      theme(text = element_text(family = 'SimSun'))
    ggsave(paste0(name,"_traintest.png"), dpi=300)

    if(is.null(res)){
      res <- tmp_group
    }else{
      res %<>%rbind(tmp_group)
    }
  }


  return(res)
}







