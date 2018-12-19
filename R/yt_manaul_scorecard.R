

#dt = dataframe , flag
yt_woe_iv <- function(dt,flag){

  # dt <- data %>% rename(target=target_m5)
  # va <- "signsum"
  # flag <- "numeric"

  Total_Good <- as.numeric(table(dt$target))[1]
  Total_bad <- as.numeric(table(dt$target))[2]

  woe_list <- list()
  if(flag=="numeric"|flag=="int"){
    names<-names(dt)[col_check(dt,"numeric")]

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
