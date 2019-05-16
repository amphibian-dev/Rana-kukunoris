

vintage_func <- function(data,group_var,vars){
  #vintage
  #group_var = 'loan_mon'
  #vars = c(m1,m2,m3,m4)

  fre_res <- c()
  for (var in vars) {
    # var <- "m7"
    # group_var <- "apply_mon"
    tmp <- data%>%filter_at(var,any_vars(!is.na(.)))%>%group_by_at(group_var)%>%
      summarise(fre=n())
    names(tmp)[2] <- paste0(var,"_fre")
    if(is.null(fre_res)){
      fre_res <- tmp
    }else{
      fre_res %<>%left_join(tmp)
    }
  }
  fre_res %<>%rbind( apply(fre_res,2,sum,na.rm=T))
  fre_res$loan_mon[nrow(fre_res)] <- "total"

  bad_res <- data%>%group_by_at(group_var)%>%
    summarise_at(grep("mob",names(data)),sum,na.rm=T)

  bad_res %<>%rbind( apply(bad_res,2,sum,na.rm=T))
  bad_res$loan_mon[nrow(bad_res)] <- "total"

  res <- fre_res %>% left_join(bad_res)

  for (i in 1:length(vars)) {
    res[paste0("mob_",i)] <-res[1+i+length(vars)]/res[i+1]
  }
  return(res)
}
########ks_计算##########################    v4版本加入
#input：必须含有 target（双值型 0 1），以及预测值 pre
#逻辑： 按照pre从小到大分20组，计算每组好坏样本数，以及累计好坏数，累计好坏比，计算diff
ks_value<-function(input){
  input%<>%arrange(pre)%>%mutate(fz=c(rep(1:20, each=nrow(input)/20), rep(20, nrow(input)%%20)))
  group<-input %>% group_by(fz)%>%
    summarise(sample_num=n(),real_bad=sum(target),max_pre=max(pre),min_pre=min(pre))%>%mutate(real_good=sample_num-real_bad)
  sum_bad<-0
  sum_good<-0
  add_up_bad <- c()
  add_up_good <- c()
  #max_pre <- c()
  #min_pre <- c()
  for(i in 1:20){
    sum_bad<- sum_bad+ group$real_bad[i]
    sum_good<- sum_good+ group$real_good[i]
    add_up_bad[i] = sum_bad
    add_up_good[i] = sum_good
  }
  sum_1<- sum(group$real_bad)
  sum_2<- sum(group$real_good)
  group%<>%cbind(add_up_bad)%>%cbind(add_up_good)%>%
    mutate(addup_bad_r=add_up_bad/sum_1,addup_good_r=add_up_good/sum_2)%>%
    mutate(diff= abs(addup_good_r-addup_bad_r))
  return(group)
}

ks_value_max<-function(input){
  input%<>%arrange(pre)%>%mutate(fz=c(rep(1:20, each=nrow(input)/20), rep(20, nrow(input)%%20)))
  group<-input %>% group_by(fz)%>%
    summarise(sample_num=n(),real_bad=sum(target))%>%mutate(real_good=sample_num-real_bad)
  sum_bad<-0
  sum_good<-0
  add_up_bad <- c()
  add_up_good <- c()
  for(i in 1:20){
    sum_bad<- sum_bad+ group$real_bad[i]
    sum_good<- sum_good+ group$real_good[i]
    add_up_bad[i] = sum_bad
    add_up_good[i] = sum_good
  }
  sum_1<- sum(group$real_bad)
  sum_2<- sum(group$real_good)
  group%<>%cbind(add_up_bad)%>%cbind(add_up_good)%>%
    mutate(addup_bad_r=add_up_bad/sum_1,addup_good_r=add_up_good/sum_2)%>%
    mutate(diff= abs(addup_good_r-addup_bad_r))
  return(max(group$diff))
}

##########################################
# function

##########################################

edd <- function (object, ...) UseMethod("edd")
#Missing Rate Check
edd.default <-function(object, ..., digits = max(3, getOption("digits") - 3))
{
  if(is.factor(object))
    return(edd.factor(object, ...))
  else if(is.matrix(object))
    return(edd.matrix(object, digits = digits, ...))

  value <- if(is.logical(object))# scalar or array!
    c("nobs"=length(object),"nmiss"=sum(is.na(object)),"nunique"=length(unique(object)),c(rep(NA,10)))
  else if(is.numeric(object)) {
    nas <- is.na(object)
    nobs=length(object)
    object <- object[!nas]
    qq <- stats::quantile(object,probs=c(0,0.01,0.05,0.25,0.50,0.75,0.95,0.99,1))
    qq <- signif(c(mean(object), qq[1L:9L]), digits)
    names(qq) <- c("mean_or_top1","min_or_top2","p1_or_top3","p5_or_top4","p25_or_top5","median_or_bot5","p75_or_bot4","p95_or_bot3","p99_or_bot2","max_or_bot1")
    c("nobs"=nobs,"nmiss"=sum(nas),"nunique"=length(unique(object)),qq)
  }
  #class(value) <- c("eddDefault", "table")
  return(t(data.frame(value)))
}

edd.factor <- function(object, maxsum = 100, ...)
{
  nas <- is.na(object)
  nobs<-length(object)
  ll <- levels(object)
  if(any(nas)) maxsum <- maxsum - 1
  tbl <- table(object[!nas])
  tt <- c(tbl)
  names(tt) <- dimnames(tbl)[[1L]]
  o <- sort(tt, decreasing = TRUE)
  b <- paste(names(o),o,sep="::")

  qq<-c(rep(NA,10))
  qq[1:(min(5,length(b)))] <- b[1:(min(5,length(b)))]
  qq[(11-(min(5,length(b)))):10] <- b[(length(b)+1-(min(5,length(b)))):length(b)]

  names(qq) <- c("mean_or_top1","min_or_top2","p1_or_top3","p5_or_top4","p25_or_top5","median_or_bot5","p75_or_bot4","p95_or_bot3","p99_or_bot2","max_or_bot1")
  value <- c("nobs"=nobs,"nmiss"=sum(nas),"nunique"=length(ll),qq)
  p <- t(data.frame(value))
  dimnames(p)[[1]]="value"
  return(p)
}

edd.matrix <- function(object, ...) {
  ## we do want this changed into separate columns, so use matrix method
  edd.data.frame(as.data.frame.matrix(object), ...)
}


edd.data.frame <-
  function(object, maxsum = 7, digits = max(3, getOption("digits") - 3), ...)
  {

    # compute results to full precision.
    #z <- t(data.frame(apply(X = object, 2,FUN = edd, maxsum = maxsum, digits = 12, ...)))
    zz <- lapply(X = as.list(object),FUN = edd, maxsum = maxsum, digits = 12)
    z <- data.frame(matrix(unlist(zz),nrow=length(zz),ncol=13,byrow=T))
    colnames(z) <- c("nobs","nmiss","nunique","mean_or_top1","min_or_top2","p1_or_top3","p5_or_top4","p25_or_top5","median_or_bot5","p75_or_bot4","p95_or_bot3","p99_or_bot2","max_or_bot1")
    rownames(z) <- names(zz)
    numind<-unlist(lapply(as.list(object),is.numeric))
    typenm<-numind
    typenm[numind]<-"numeric"
    typenm[!numind]<-"character"
    return(data.frame(cbind(typenm,z)))
  }

#########################################################################
# Utilization Packages Built For TMOB
# Written by Lei CHEN (lei.chen@operasolutions.com), this version Nov 11th, 2014.
# Questions and comments are welcome.
#########################################################################
#########################################################################
# Chi Merge
#########################################################################
value_new <-function (data, alpha,binum)
{
  i <- 1
  p1 <- 2
  p <- p1 - 1
  y <- as.integer(data[, p1])
  class <- dim(table(data[, p1]))
  discredata <- data
  threshold <- qchisq(1 - alpha, class - 1)
  cuts <- numeric()
  z <- sort(unique(data[, i]))
  if (length(z) <= 1)
    return(list(cuts = "", disc = discredata))
  if (length(z) <= binum){
    dff <- diff(z)/2
    lenz <- length(z)
    cutpoint <- z[1:(lenz - 1)] + dff
    midpoint <- unique(c(z[1], cutpoint, z[lenz]))
  }else{
    qtile <- sort(unique(quantile(data[, i],c(1:binum)/binum,na.rm=TRUE)))
    cutpoint <- qtile[1:(length(qtile)-1)]
    midpoint <- unique(c(min(data[, i],na.rm=TRUE),qtile))
    if (length(midpoint)==2){
      cutpoint=c(cutpoint,min(data[, i],na.rm=TRUE)+0.5)
      midpoint=unique(c(min(data[, i]),min(data[, i],na.rm=TRUE)+0.5,qtile))
    }
  }
  a <- cut(data[, i], breaks = midpoint, include.lowest = TRUE)
  b <- table(a, data[, p1])
  b <- as.array(b)
  pop_cut <- sum(b)/20
  repeat {
    m <- dim(b)[1]
    if (length(dim(b)) < 2 || m < 2)
      break
    test <- numeric()
    for (k in 1:(m - 1)) {
      d <- b[c(k, k + 1), ]
      test[k] = chiSq(d)
    }
    k <- which.min(test)
    if (test[k] > threshold && m < 6)
      break
    b[k + 1, ] <- b[k, ] + b[k + 1, ]
    cutpoint <- cutpoint[-k]
    midpoint <- midpoint[-(k + 1)]
    b <- b[-k, ]
  }

  repeat {
    m <- dim(b)[1]
    if (length(dim(b)) < 2 || m < 2)
      break
    test <- numeric()
    for (k in 1:(m - 1)) {
      d <- b[c(k, k + 1), ]
      test[k] = chiSq(d)
    }
    pop_r <- rowSums(b)
    k <- which.min(pop_r)
    if (pop_r[k] > pop_cut)
      break
    if (k==m){
      k = k-1
    }else{
      if (k>1){
        if (test[k]>test[k-1]){
          k = k-1
        }
      }
    }

    b[k + 1, ] <- b[k, ] + b[k + 1, ]
    cutpoint <- cutpoint[-k]
    midpoint <- midpoint[-(k + 1)]
    b <- b[-k, ]
  }

  cuts <- cutpoint
  discredata[, i] <- cut(data[, i], breaks = midpoint, include.lowest = TRUE,label=FALSE)
  discredata[which(is.na(discredata[, i])), i] <- length(midpoint)
  #discredata[, i] <- cut(data[, i], breaks = midpoint, include.lowest = TRUE)
  cutp <- midpoint[-1]
  if (length(cutp)>=1){
    cutp <- cutp[-length(cutp)]
  }
  return(list(cuts = cutp, mids=midpoint,disc = discredata))
}

chiM_C<-function (data, alpha = 0.05, binum = 10000)
{
  mc <- getOption("mc.cores", 8)
  p <- dim(data)[2]
  discredata <- data
  cutp <- list()

  chiM_out <- mclapply(1:(p-1), function(x){return(value_new(data[,c(x,p)], alpha,binum))}, mc.cores = mc)
  cutp<-lapply(c(1:(p-1)),function(x){return(chiM_out[[x]]$cuts)})
  discredata<-do.call('cbind',lapply(c(1:(p-1)),function(x){return(chiM_out[[x]]$disc[[1]])}))
  discredata<-data.frame(discredata)
  names(discredata)<-names(data)[1:(p-1)]
  discredata[names(data)[p]]<-data[,p]
  gc()
  return(list(cutp = cutp, Disc.data = discredata))
}

chiM_C_windows<-function (data, alpha = 0.05, binum = 10000)
{
  mc <- getOption("mc.cores", 1)
  p <- dim(data)[2]
  discredata <- data
  cutp <- list()

  chiM_out <- mclapply(1:(p-1), function(x){return(value_new(data[,c(x,p)], alpha,binum))}, mc.cores = mc)
  cutp<-lapply(c(1:(p-1)),function(x){return(chiM_out[[x]]$cuts)})
  discredata<-do.call('cbind',lapply(c(1:(p-1)),function(x){return(chiM_out[[x]]$disc[[1]])}))
  discredata<-data.frame(discredata)
  names(discredata)<-names(data)[1:(p-1)]
  discredata[names(data)[p]]<-data[,p]
  gc()
  return(list(cutp = cutp, Disc.data = discredata))
}

IV_Calc_windows <- function(dataset,tgt,sig,scoring="N"){
  num_ind <- sapply(dataset[,sig],is.numeric)
  if (sum(num_ind)>0) {
    ds <- dataset[c(sig[num_ind],tgt)]
    ds[,tgt] <- factor(ds[,tgt])
    temp <- chiM_C_windows(ds,alpha=0.1,100)
    disc <- temp$Disc.data
    print("chi-merge ready")
    if (sum(!num_ind)>0){
      disc <- cbind(dataset[sig[!num_ind]],disc)
      print("Done")
    }
  } else{
    disc <- dataset[c(sig,tgt)]
  }
  #disc <- data.frame(sapply(disc,factor))

  woe_model <- woe_gen(disc[,sig],disc[,tgt],0.5)
  print("WOE Calculation Done")

  woe_calc <-function(pred){
    woe_value <- woe_model$woe[[pred]]
    pop_count <- woe_model$Pop[[pred]]
    # add tar
    tar_count <- woe_model$Tar[[pred]]
    woe_out <- data.frame(cbind(woe_value,pop_count))
    # add tar
    woe_out <- cbind(woe_out,tar_count)
    woe_out$Var_Name <- pred

    if(pred%in%sig[num_ind]){
      bin_cat=c("",temp$cutp[[which(sig[num_ind]==pred)]],"","")
      woe_out$Bin <- sapply(rownames(woe_out),function(i) return(paste(sprintf("%02d",as.integer(i)),".(",bin_cat[as.integer(i)],",",bin_cat[as.integer(i)+1],"]")))
      woe_out$Bin_Show <- sapply(rownames(woe_out),function(i) return(paste(sprintf("%02d",as.integer(i)),".(",round(as.numeric(bin_cat[as.integer(i)]),2),",",round(as.numeric(bin_cat[as.integer(i)+1]),2),"]")))
    }else {
      woe_out$Bin <- rownames(woe_out)
      woe_out$Bin_Show <- rownames(woe_out)
    }

    rownames(woe_out) <- NULL
    return(woe_out)
  }

  mc <- getOption("mc.cores", 1)
  woe_mst <- do.call("rbind",mclapply(names(woe_model$woe), woe_calc, mc.cores = mc))

  if (scoring=="Y"){
    for (var in sig){
      disc[,paste(var,"_WOE",sep="")] <- as.vector(woe_model$woe[[var]][disc[,var]])
    }
  }
  gc()
  if (scoring=="Y"){
    return(list("DT"=disc, "IV"=woe_model$IV, "WOE"=woe_mst))
  }else{
    return(list("IV"=woe_model$IV, "WOE"=woe_mst))
  }
}
#########################################################################
# WOE Calculation
#########################################################################
computewoes <- function(x, y, adj){
  #print(names(x))
  #print(Sys.time())
  if(! is.factor(x)) x <- as.factor(x)
  if(! is.factor(y)) y <- as.factor(y)
  if(sum(table(x,y)==0) > 0) warning("At least one empty cell (class x level) does exist")
  # class wise event rates
  xtab <- table(y, x)
  # correction for empty class levels
  xtab[which(xtab == 0)] <- xtab[which(xtab == 0)] + adj
  ncl      <- table(x)
  # compute woes for alle classes
  fxgegy <- xtab
  fy  <- rowSums(fxgegy)
  for(i in 1:2) fxgegy[i,] <- fxgegy[i,] / fy[i]
  woes <- log(fxgegy[1,]/fxgegy[2,])
  if(any(fxgegy[2,] == 0)) warning("Empty cells result in infinite woes, zeroadj should be specified > 0!")

  # add IV
  # difference of class wise relative frequencies
  bdiff <- fxgegy[1,] - fxgegy[2,]
  # calculate information value
  IV <- sum(bdiff * woes)
  out <- c(woes,IV)
  return(out)

  #print(woes)
}

computepop <- function(x, y, adj){
  if(! is.factor(x)) x <- as.factor(x)
  return(table(x))

  #print(woes)
}

#compute target
computetar <- function(x, y, adj){
  if(! is.factor(x)) x <- as.factor(x)
  if(! is.factor(y)) y <- as.factor(y)
  return(table(y,x)[1,])

  #print(woes)
}

#########################################################################
# WOE Calculation
#########################################################################
woe_gen <- function(x, grouping, zeroadj = 0){

  x.woes <- lapply(x, computewoes, y = grouping, adj = zeroadj)
  x.pop <- lapply(x, computepop, y = grouping, adj = zeroadj)
  # add target count
  x.tar <- lapply(x, computetar, y = grouping, adj = zeroadj)

  # separate woes and IVs
  IVs <-  sapply(x.woes, function(x) return(x[length(x)]))
  x.woes <- lapply(x.woes, function(x) return(x[-length(x)]))
  #x.pop <- lapply(x.woes, function(x) return(x$pop[:]))
  res <- list("woe" = x.woes, "IV" = IVs, "Pop"=x.pop, "Tar"=x.tar)
  class(res) <- "woe"

  return(res)
}

#########################################################################
# Information Value Calculation
#########################################################################
IV_Calc <- function(dataset,tgt,sig,scoring="N"){
  num_ind <- sapply(dataset[,sig],is.numeric)
  if (sum(num_ind)>0) {
    ds <- dataset[c(sig[num_ind],tgt)]
    ds[,tgt] <- factor(ds[,tgt])
    temp <- chiM_C(ds,alpha=0.1,100)
    disc <- temp$Disc.data
    print("chi-merge ready")
    if (sum(!num_ind)>0){
      disc <- cbind(dataset[sig[!num_ind]],disc)
      print("Done")
    }
  }	else{
    disc <- dataset[c(sig,tgt)]
  }
  #disc <- data.frame(sapply(disc,factor))

  woe_model <- woe_gen(disc[,sig],disc[,tgt],0.5)
  print("WOE Calculation Done")

  woe_calc <-function(pred){
    woe_value <- woe_model$woe[[pred]]
    pop_count <- woe_model$Pop[[pred]]
    # add tar
    tar_count <- woe_model$Tar[[pred]]
    woe_out <- data.frame(cbind(woe_value,pop_count))
    # add tar
    woe_out <- cbind(woe_out,tar_count)
    woe_out$Var_Name <- pred

    if(pred%in%sig[num_ind]){
      bin_cat=c("",temp$cutp[[which(sig[num_ind]==pred)]],"","")
      woe_out$Bin <- sapply(rownames(woe_out),function(i) return(paste(sprintf("%02d",as.integer(i)),".(",bin_cat[as.integer(i)],",",bin_cat[as.integer(i)+1],"]")))
      woe_out$Bin_Show <- sapply(rownames(woe_out),function(i) return(paste(sprintf("%02d",as.integer(i)),".(",round(as.numeric(bin_cat[as.integer(i)]),2),",",round(as.numeric(bin_cat[as.integer(i)+1]),2),"]")))
    }else {
      woe_out$Bin <- rownames(woe_out)
      woe_out$Bin_Show <- rownames(woe_out)
    }

    rownames(woe_out) <- NULL
    return(woe_out)
  }

  mc <- getOption("mc.cores", 8)
  woe_mst <- do.call("rbind",mclapply(names(woe_model$woe), woe_calc, mc.cores = mc))

  if (scoring=="Y"){
    for (var in sig){
      disc[,paste(var,"_WOE",sep="")] <- as.vector(woe_model$woe[[var]][disc[,var]])
    }
  }
  gc()
  if (scoring=="Y"){
    return(list("DT"=disc, "IV"=woe_model$IV, "WOE"=woe_mst))
  }else{
    return(list("IV"=woe_model$IV, "WOE"=woe_mst))
  }
}

#########################################################################
# Entropy
#########################################################################
uni.cond.entropy <- function(class, attr) {
  # Verify that the two vectors have same length
  if (length(attr) != length(class)) stop("Two vectors must have same length")
  h <- computewoes(attr,class,adj=0.5)
  # Result
  return(h[[length(h)]])
}


# A function that takes a probability distribution vector as input
#   and produce the entropy value of that distribution
# Note: Assume 0lg0 = 0 to make the algorithm work
entropy <- function(dist) {
  # Verify that none of the probabilities is negative
  if (sum(dist < 0) > 0) stop("Invalid probability distribution: Negative value")

  # Verify that sum of probabilities in the input distribution is 1
  if (sum(dist) != 1) {
    print(dist)
    stop("Invalid probability distribution: Sum not equal to 1")
  }
  # Compute a vector of entropy components
  ent.comps <- -dist * log2(dist)

  # Exemption: 0lg0 = 0
  ent.comps[is.nan(ent.comps)] <- 0

  # Output sum of vector elements as the result
  sum(ent.comps)
}

#########################################################################
# Split Info
#########################################################################
Sp_INFO_Calc <- function(dataset,tgt,sig,cutp){
  dataset$Cat <- 0
  dataset[which(dataset[sig]>cutp),"Cat"] <- 1
  if (length(unique(dataset$Cat))==1){
    return(-10)
  }
  uni.cond.entropy(dataset[[tgt]], dataset$Cat)
}

Sp_INFO <- function(dataset,tgt,sig,cutp){
  info_list <- unlist(lapply(cutp, function(i) return(Sp_INFO_Calc(dataset,tgt,sig,i))))
  return(list(max_gain=max(info_list),indx=which.max(info_list)))
}

classifyInstance<-function(obs, tree, sig_L, sig_R){
  answer = NULL
  node = tree
  cut_l <- list()
  cut_l[[sig_L]] <- c(NA,NA)
  cut_l[[sig_R]] <- c(NA,NA)
  while(is.null(answer)){
    #print(node$type)
    #print(node$cutoff)
    if(node$type != "leaf"){

      if(obs[[node$split_var]]<=node$cutoff){
        #print("Left")
        cut_l[[node$split_var]][2] <- node$cutoff
        node = node$branch[[1]]

      }else{
        #print("Right")
        cut_l[[node$split_var]][1] <- node$cutoff
        node = node$branch[[2]]
      }
      #print(cut_l)
    }else{
      cut_l[[sig_L]] <- as.character(cut_l[[sig_L]])
      cut_l[[sig_L]][is.na(cut_l[[sig_L]])] <- ""
      cut_l[[sig_R]] <- as.character(cut_l[[sig_R]])
      cut_l[[sig_R]][is.na(cut_l[[sig_R]])] <- ""
      answer <- paste(paste("(",cut_l[[sig_L]][1],",",cut_l[[sig_L]][2],"]",sep=""),paste("(",cut_l[[sig_R]][1],",",cut_l[[sig_R]][2],"]",sep=""),sep="|")
    }
  }

  return(answer)
}
#########################################################################
# 2D Discretization
#########################################################################
Disc_2D_Rec <- function(dataset,tgt,sig_L,sig_R,cutp_L,cutp_R,num_limit,depth=0){
  root = list(type="leaf",label="class")
  #print(depth)
  #base cases
  classes = length(unique(dataset[[tgt]]))
  ifelse(length(cutp_L)==0,INFO_L <- list(max_gain=-10,indx=1),INFO_L <- Sp_INFO(dataset,tgt,sig_L,cutp_L))
  ifelse(length(cutp_R)==0,INFO_R <- list(max_gain=-10,indx=1),INFO_R <- Sp_INFO(dataset,tgt,sig_R,cutp_R))
  if(nrow(dataset)<num_limit || classes==1 || (length(cutp_L)==0 && length(cutp_R)==0) || max(INFO_L$max_gain,INFO_R$max_gain) < 1e-3||depth>3){
    return(root)
    #print("OK")
  }

  #main algorithm

  if(INFO_L$max_gain>INFO_R$max_gain){
    index <- which(dataset[[sig_L]]<=cutp_L[INFO_L$indx])
    if(length(index)<num_limit||(nrow(dataset)-length(index))<num_limit){
      return(root)
    }
    root$type="node"
    root$branch = list()
    root$split_var <- sig_L
    root$cutoff <- cutp_L[INFO_L$indx]
    root$branch[[1]] <- Disc_2D_Rec(dataset[index,],tgt,sig_L,sig_R,cutp_L[1:(INFO_L$indx-1)],cutp_R,num_limit,depth+1)
    root$branch[[2]] <- Disc_2D_Rec(dataset[-index,],tgt,sig_L,sig_R,cutp_L[-c(1:INFO_L$indx)],cutp_R,num_limit,depth+1)
  }else{
    index <- which(dataset[[sig_R]]<=cutp_R[INFO_R$indx])
    if(length(index)<num_limit||(nrow(dataset)-length(index))<num_limit){
      return(root)
    }
    root$type="node"
    root$branch = list()
    root$split_var <- sig_R
    root$cutoff <- cutp_R[INFO_R$indx]
    root$branch[[1]] <- Disc_2D_Rec(dataset[index,],tgt,sig_L,sig_R,cutp_L,cutp_R[1:(INFO_R$indx-1)],num_limit,depth+1)
    root$branch[[2]] <- Disc_2D_Rec(dataset[-index,],tgt,sig_L,sig_R,cutp_L,cutp_R[-c(1:INFO_R$indx)],num_limit,depth+1)
  }
  return(root)
}

Disc_2D <- function(dataset,tgt,sig_L,sig_R,binum = 100){
  if (length(unique(dataset[,sig_L]))<=binum){
    cutp_L <- sort(unique(dataset[,sig_L]))
  }else{
    cutp_L <- sort(unique(c(min(dataset[,sig_L]),quantile(dataset[,sig_L],c(1:binum)/binum,na.rm=TRUE))))
  }
  cutp_L <- cutp_L[-c(1,length(cutp_L))]

  if (length(unique(dataset[,sig_R]))<=binum){
    cutp_R <- sort(unique(dataset[,sig_R]))
  }else{
    cutp_R <- sort(unique(c(min(dataset[,sig_R]),quantile(dataset[,sig_R],c(1:binum)/binum,na.rm=TRUE))))
  }
  cutp_R <- cutp_R[-c(1,length(cutp_R))]
  Disc_Out <- Disc_2D_Rec(dataset,tgt,sig_L,sig_R,cutp_L,cutp_R,nrow(dataset)/20,0)
  var_bin  = apply(dataset[,c(sig_L,sig_R)],1,function(r) classifyInstance(r,Disc_Out,sig_L,sig_R))
  var_bin <- data.frame(var_bin)
  names(var_bin) <- paste(sig_L,"|",sig_R,"_WOE",sep="")
  return(list(disc_dat=var_bin,cut_tree=Disc_Out))
}
#########################################################################
# Information Value Calculation For Two Dimension Effect
#########################################################################
IV_Calc_2D <- function(dataset,tgt,sig_L,sig_R){
  Disc_Mst <- NULL
  Tree_Mst <- list()
  for (sig_l in sig_L){
    for (sig_r in sig_R){
      Disc_Out <- Disc_2D(dataset,tgt,sig_l,sig_r,20)
      Tree_Mst[paste(sig_l,sig_r,sep="|")] <- Disc_Out$cut_tree
      ifelse(is.null(Disc_Mst),Disc_Mst <- Disc_Out$disc_dat,Disc_Mst <- cbind(Disc_Mst,Disc_Out$disc_dat))
    }
  }
  #disc <- data.frame(sapply(disc,factor))

  woe_model <- woe_gen(Disc_Mst,dataset[,tgt],0.5)
  print("WOE Calculation Done")
  woe_mst <- NULL
  for (pred in names(woe_model$woe)){
    woe_value <- woe_model$woe[[pred]]
    pop_count <- woe_model$Pop[[pred]]
    woe_out <- data.frame(cbind(woe_value,pop_count))
    woe_out$Var_Name <- pred
    woe_out$Bin <- rownames(woe_out)
    rownames(woe_out) <- NULL
    ifelse(is.null(woe_mst),woe_mst <- woe_out,woe_mst <- rbind(woe_mst,woe_out))
  }

  return(list("DT"=Disc_Mst, "IV"=woe_model$IV, "WOE"=woe_mst, "Tree_Mst"=Tree_Mst))
  #return(list("IV"=woe_model$IV, "WOE"=woe_mst))
}

#########################################################################
# Variable Normalization
#########################################################################
normalize <- function(dataset,append_these = names(dataset)[grep("^Signal_",names(dataset))]) {
  for (var in append_these){
    dataset[,var]=as.numeric(dataset[,var])
    print(var)

    print(mean(dataset[,var],na.rm=TRUE))
    a=ifelse(mean(dataset[,var],na.rm=TRUE)==0,1,mean(dataset[,var],na.rm=TRUE))
    dataset[,var]=(dataset[,var]-a)/sd(dataset[,var],na.rm=TRUE)
  }
  return(dataset)
}

#########################################################################
# Cluster Assignment
#########################################################################
km.predict = function(km, newdata){
  nclusters = nrow(km$centers)
  distance = matrix(0, nrow(newdata), nclusters)
  for( k in 1:nclusters ) {
    sqdist = ( t(newdata)-km$centers[k,] )**2
    distance[,k] = apply(sqdist,2,sum)
  }
  list(clusters=apply(distance, 1, which.min))
}

#########################################################################
# GMM Scoring
#########################################################################
gmm.predict = function(gmm, newdata){
  es<-estep(modelName="VVV",data=newdata,parameters=gmm)
  list(clusters=apply(es$z, 1, which.max))
}

#########################################################################
# Outlier Treatment
#########################################################################
out_treat <- function(dataset,append_these = names(dataset)[grep("^Signal_",names(dataset))],bound=FALSE) {
  bound_new <- data.frame(varname= character(0), LBound= numeric(0), UBound=numeric(0))
  for (var in append_these){
    print(var)
    if (bound==FALSE){
      qnt <- quantile(dataset[,var], probs=c(.05, .95), na.rm = TRUE)
      H <- 0.15 * max(IQR(dataset[,var], na.rm = TRUE),4)
      LB <- qnt[1] - H
      UB <- qnt[2] + H
      bound_new <- rbind(bound_new,data.frame(varname= var, LBound= LB, UBound= UB))
    }
    else{
      bd <- bound[which(bound$varname==var),]
      LB <- bd[[2]]
      UB <- bd[[3]]
    }
    dataset[,var][dataset[,var] < LB] <- LB
    dataset[,var][dataset[,var] > UB] <- UB

  }
  if (bound==FALSE){
    bound <- bound_new
  }
  return(list("ds"=dataset,"bd"=bound))
}

###################################################################
#TAC treatment
###################################################################
l_align <- function(x,n_len=8){
  temp <- as.character(x)
  if (nchar(temp)<n_len){
    for (i in c(1:(n_len-nchar(temp)))){
      temp <- paste(0,temp,sep="")
    }
  }
  return(temp)
}

str_num <- function(x){
  as.integer(substr(x,2,nchar(x)))
}
#########################################################################
# Reason Code Generation
#########################################################################
ftopk <- function(x,top=1){
  res=names(x)[order(x,decreasing=TRUE)]
  res=res[!res%in%c("CurrentCycle","preAvgVsCurr_sum_call_cnt")][1:top]
  paste(res,collapse=";",sep="")
}

#########################################################################
# Data Selection
#########################################################################
d_sel <- function(ds_f,name_list,sep="|"){
  f <- read.table(ds_f, header = TRUE, sep = sep, nrows = 1)
  if(any(!name_list%in%names(f))) stop("At least one variable does not exist")
  idx <- seq(length(names(f)))[names(f)%in%name_list]
  cmd <- paste('cut -d ',"\"",sep,"\""," -f",paste(idx,collapse=",")," ",ds_f,">",ds_f,"_temp",sep="")
  system(cmd)
  ds <- read.table(paste(ds_f,"temp",sep="_"), header = TRUE, sep = sep, stringsAsFactors = FALSE)
  return(ds)
}

#########################################################################
# Combination Iteration
#########################################################################
comb_loop <- function(cad_list,num){
  if (num<=1){
    return(cad_list)
  }else{
    comb_lst <- comb_loop(cad_list,num-1)
    return(c(outer(cad_list,comb_lst,FUN=paste,sep=",")))
  }
}

#########################################################################
# Combination Iteration
#########################################################################
comb_loop_t <- function(num_list){
  x <- greedy_c(num_list[1])
  if (length(num_list)<=1){
    return(x)
  }else{
    comb_lst <- comb_loop_t(num_list[-1])
    return(c(outer(x,comb_lst,FUN=paste,sep=",")))
  }
}
#########################################################################
# Cell Level Profiling
#########################################################################
cell_perf <- function(dataset,tgt="tgt",score_list,cell_indx,t_point=t_point){
  cell_arr <- as.integer(unlist(strsplit(cell_indx,",")))
  score_num <- min(length(score_list),as.integer(length(cell_arr)/2))
  score_list <- paste(score_list[c(1:score_num)],"_bin",sep="")
  eval(parse(text=paste("sub_mst <- subset(dataset,",paste(c(paste(score_list,cell_arr[2*c(1:score_num)-1],sep=">="),paste(score_list,cell_arr[2*c(1:score_num)],sep="<=")),collapse="&"),")",sep="")))
  summ <- aggregate(as.formula(paste("cnt~",tgt,sep="")),data=sub_mst,FUN=sum)
  res <- data.frame(t(as.vector(summ$cnt)))
  names(res) <- summ[[tgt]]
  for (i in c(1:score_num)){
    res[score_list[i]] <- paste("(",round(t_point[[score_list[i]]][cell_arr[2*i-1]],2),",",round(t_point[[score_list[i]]][cell_arr[2*i]+1],2),"]",sep="")
  }
  res$Total <- sum(summ$cnt)
  res$Paid_Feature_Candidate <- sum(sub_mst[which(sub_mst$UITT_SITT_ind==0),"cnt"])
  res$UnPaid_Feature_Candidate <- sum(sub_mst[which(sub_mst$ind_AutoPay==0),"cnt"])
  res$Add_Fearure <- sum(sub_mst[which(sub_mst$UITT_SITT_ind==0&sub_mst$paid_feature_ind_next==1),"cnt"])
  res$Add_UnPaid_Feature <- sum(sub_mst[which(sub_mst$ind_AutoPay==0&sub_mst$unpaid_engage_ind_next==1),"cnt"])
  return(res)
}

#########################################################################
# Get Cut Bins
#########################################################################
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
get_cut <- function(woe_mapping){
  return(sort(unique(as.numeric(unlist(strsplit(gsub(pattern = "\\]|\\s+", replacement = "",substring(woe_mapping,6)),","))))))
}

#########################################################################
# Scoring WOE
#########################################################################
score_woe <- function(dataset,sig=sig,woe_dict=woe_dict){
  woe_mapping <- woe_dict[which(woe_dict$Var_Name==sig,),c("Bin","woe_value")]
  if (is.numeric(dataset[[sig]])){
    indx <- which(!is.na(dataset[[sig]]))
    dataset$Bin <- nrow(woe_mapping)
    cutpoints <- get_cut(woe_mapping$Bin)
    if (length(cutpoints)==0){
      sig_woe <- data.frame(rep(0,nrow(dataset)))
      names(sig_woe) <- paste(sig,"_WOE",sep="")
      return(sig_woe)
    }
    cutpoints <- c(min(min(cutpoints)-1,min(dataset[[sig]],na.rm=TRUE)-1),cutpoints,max(max(cutpoints)+1,max(dataset[[sig]],na.rm=TRUE)+1))
    dataset[indx,"Bin"] <- cut(dataset[indx,sig], breaks = cutpoints, include.lowest = TRUE,label=FALSE)
    dataset$Bin <- as.character(dataset$Bin)
    woe_mapping$Bin <- substr(woe_mapping$Bin,2,2)
  }else{
    dataset$Bin <- dataset[[sig]]
  }
  map <- woe_mapping$woe_value
  names(map) <- woe_mapping$Bin
  sig_woe <- data.frame(as.vector(map[dataset$Bin]))
  names(sig_woe) <- paste(sig,"_WOE",sep="")
  rm(dataset);gc()
  return(sig_woe)
}
#########################################################################
# LRT Model
#########################################################################
LRT <- function(dataset,tgt="tgt",sig_c,out_f=out_f){
  mu1 <- apply(dataset[which(dataset[tgt]=="0"),sig_c],2,mean)
  mu2 <- apply(dataset[which(dataset[tgt]=="1"),sig_c],2,mean)
  R1 <- cov(dataset[which(dataset[tgt]=="0"),sig_c])
  R2 <- cov(dataset[which(dataset[tgt]=="1"),sig_c])
  P2 <- mean(as.numeric(dataset[[tgt]]))
  P1 <- 1-P2
  print(P1)
  save(sig_c,P1,P2,mu1,mu2,R1,R2,file=out_f)
}

#########################################################################
# Modelling With WOE Transformation
#########################################################################
p_model <- function(dataset,tgt="cnt",sig=sig_n,num_ind=num_ind,out_cat="Mst",IV_cut=0){
  print (out_cat)
  # Split into test and control
  set.seed(4096)
  #  train <- sample(1:dim(dataset)[1],round(dim(dataset)[1]*0.7))
  train <- c(sample(which(dataset[tgt]=="0"),round(length(which(dataset[tgt]=="0"))*0.7)),sample(which(dataset[tgt]=="1"),round(length(which(dataset[tgt]=="1"))*0.7)))
  dataset$tc_ind <- "Test"
  dataset[train,"tc_ind"] <- "Train"

  # Build Target Label
  weight_local <- floor(max(table(dataset[[tgt]]))/table(dataset[[tgt]]))
  dataset$wt_local <- as.vector(weight_local[dataset[[tgt]]])

  # Build WOE Variable
  WOE_T <- IV_Calc(dataset,tgt,sig,"Y")
  disc_b <- WOE_T$DT
  temp <- cbind(dataset,disc_b[,names(disc_b)[grep("_WOE",names(disc_b))]])
  WOE_Out <- WOE_T$WOE
  WOE_Out$Target <- out_cat
  IV_Out <- data.frame(WOE_T$IV)
  names(IV_Out) <- out_cat

  # Normalization
  sig_all <- c(sig[num_ind],names(dataset)[grep("_Log",names(dataset))],names(disc_b)[grep("_WOE",names(disc_b))])
  sig_all <- sig_all[grep("_WOE",sig_all)]
  #  sig_all <- sig_all[!sig_all%in%c("pctDownChargedEIPAmt","lauchMonth","phoneLteInd")]
  s <- scale(temp[,sig_all],center=TRUE,scale=TRUE)
  s[is.na(s)] <- 0
  rm(disc_b)
  #rm(temp)

  # signal select: cross valdation by IV and correlation
  corr_s <- cor(s)
  #  print("before signal select:")
  #  print(length(sig_all))
  write.csv(corr_s, paste("Cor_",IV_cut,"_",out_cat,".csv",sep=""))
  #  sig_all_sel<-signal_sel(IV_Out,corr_s,0.9)
  #  print("after signal select:")
  #  print(length(sig_all_sel))
  #    print(sig_all)
  sig_all_sel <- sig_all
  # Training
  idx <- which(dataset[[tgt]]=="0")
  dataset[[tgt]] <- "0"
  dataset[idx,tgt] <- "1"
  cvfit = cv.glmnet(s[train,sig_all_sel],dataset[train,tgt], family = "binomial", type.measure = "class", weights=dataset[train,"wt_local"],pmax=11, alpha = 0.9)

  # Out Parameter
  para <- coef(cvfit, s = "lambda.min")
  PARA_Out <- data.frame(as.matrix(para))
  #  print(PARA_Out)
  names(PARA_Out) <- out_cat

  # GINI Index
  prob_mst <- predict(cvfit, newx = s[,sig_all_sel], type = "response", s = "lambda.min")
  prob_mst <- data.frame(prob_mst)
  names(prob_mst) <- out_cat
  prof_mst <- dataset[,c(tgt,"tc_ind")]
  prof_mst <- cbind(prof_mst,prob_mst)
  for (tc_cat in unique(prof_mst$tc_ind)){
    mst_sub <- prof_mst[which(prof_mst$tc_ind==tc_cat),]
    mst_sub <- mst_sub[order(-mst_sub[[out_cat]]),]
    indx <- which(mst_sub[[tgt]]=="1")
    mst_sub[[tgt]] <- 0
    mst_sub[indx,tgt] <- 1
    TP<-cumsum(mst_sub[[tgt]])/sum(mst_sub[[tgt]])
    FP<-cumsum(1-mst_sub[[tgt]])/sum(1-mst_sub[[tgt]])
    print(sprintf("%s,%s,GINI Index:%0.4f\n",tc_cat,out_cat,sum((FP[2:length(FP)]-FP[1:length(FP)-1])*TP[2:length(TP)])*2-1))
    print(sprintf("%s,%s,TARGET Rate:%0.4f\n",tc_cat,out_cat,sum(mst_sub[[tgt]])/length(mst_sub[[tgt]])))
    ### KS value
    print(sprintf("%s,%s,K-S Index:%0.4f\n",tc_cat,out_cat,ks_stat(actuals=dataset[which(dataset$tc_ind==tc_cat),tgt], predictedScores=prof_mst[which(prof_mst$tc_ind==tc_cat),out_cat])))
  }

  s_center <- attr(s,"scaled:center")
  s_scale  <- attr(s,"scaled:scale")
  save(s_center,s_scale,cvfit,file=paste("Mdl_",IV_cut,"_",out_cat,".RData",sep=""))
  # LRT Model
  if (out_cat=="Churn"){
    temp[sig_all] <- s
    indx <- which(abs(PARA_Out[,1])>0)
    var_s <- row.names(PARA_Out)[indx][-1]
    LRT(temp,tgt=tgt,sig_c=var_s,out_f="Mdl_LRT.RData")
  }

  rm(temp);rm(dataset);rm(prof_mst);gc()
  return(list(IV = IV_Out, WOE = WOE_Out, PARA = PARA_Out, Score = prob_mst))
}


signal_sel<- function(IV_Out,corr_s,th){
  sig_sel<-c(rownames(corr_s)[1],rownames(corr_s)[grep("_WOE",rownames(corr_s))])
  for (i in c(2:dim(corr_s)[1])){
    if(length(grep("_WOE",rownames(corr_s)[i]))==0){
      flag<-0
      flag_iv<-0
      sig_sel_diff<-c()
      if(length(grep("_WOE",sig_sel))>0){
        for (j in sig_sel[-grep("_WOE",sig_sel)]){
          if (i !=j &(abs(corr_s[i,j])>=th | is.na(corr_s[i,j]))){
            flag<-1
            if(IV_Out[rownames(corr_s)[i],]>IV_Out[j,]){
              sig_sel_diff<-c(sig_sel_diff,j,paste(gsub(" ","",j),"_WOE",sep=""))
            }else{
              flag_iv <- 1
              sig_sel_diff<-c(sig_sel_diff,paste(gsub(" ","",rownames(corr_s)[i]),"_WOE",sep=""))
            }
          }
        }
      }else{
        for (j in sig_sel){
          if (i !=j & (abs(corr_s[i,j])>=th | is.na(corr_s[i,j]))){
            flag<-1
            if(IV_Out[rownames(corr_s)[i],]>IV_Out[j,]){
              sig_sel_diff<-c(sig_sel_diff,j,paste(gsub(" ","",j),"_WOE",sep=""))
            }else{
              flag_iv <- 1
              sig_sel_diff<-c(sig_sel_diff,paste(gsub(" ","",rownames(corr_s)[i]),"_WOE",sep=""))
            }
          }
        }
      }
      if(flag==0 | flag_iv==0){
        sig_sel<-union(sig_sel,rownames(corr_s)[i])
        #print(rownames(corr_s)[i])
      }
      #print(sig_sel_diff)
      sig_sel<-setdiff(sig_sel, sig_sel_diff)
    }
  }
  return (sig_sel)
}

# linear modelName
lm_model <- function(dataset,tgt="cnt",sig=sig_n,num_ind=num_ind,out_cat="Mst"){
  print (out_cat)
  # Split into test and control
  set.seed(4096)
  #  train <- sample(1:dim(dataset)[1],round(dim(dataset)[1]*0.7))
  train <- c(sample(which(dataset[tgt]=="0"),round(length(which(dataset[tgt]=="0"))*0.7)),sample(which(dataset[tgt]=="1"),round(length(which(dataset[tgt]=="1"))*0.7)))
  dataset$tc_ind <- "Test"
  dataset[train,"tc_ind"] <- "Train"

  # Build Target Label
  weight_local <- floor(max(table(dataset[[tgt]]))/table(dataset[[tgt]]))
  dataset$wt_local <- as.vector(weight_local[dataset[[tgt]]])

  # Build WOE Variable
  WOE_T <- IV_Calc(dataset,tgt,sig,"Y")
  disc_b <- WOE_T$DT
  temp <- cbind(dataset,disc_b[,names(disc_b)[grep("_WOE",names(disc_b))]])
  WOE_Out <- WOE_T$WOE
  WOE_Out$Target <- out_cat
  IV_Out <- data.frame(WOE_T$IV)
  names(IV_Out) <- out_cat

  # Normalization
  sig_all <- c(sig[num_ind],names(dataset)[grep("_Log",names(dataset))],names(disc_b)[grep("_WOE",names(disc_b))])
  #  sig_all <- sig_all[grep("_WOE",sig_all)]
  sig_all <- sig_all[!sig_all%in%c("pctDownChargedEIPAmt","lauchMonth","phoneLteInd")]
  s <- scale(temp[,sig_all],center=TRUE,scale=TRUE)
  s[is.na(s)] <- 0
  rm(disc_b)
  #rm(temp)

  # signal select: cross valdation by IV and correlation
  corr_s <- cor(s)
  print("before signal select:")
  print(length(sig_all))
  write.csv(corr_s, "corr_s.csv")
  sig_all_sel<-signal_sel(IV_Out,corr_s,0.9)
  print("after signal select:")
  print(length(sig_all_sel))
  #print(sig_all)
  #sig_all_sel <- sig_all
  # Training
  #  idx <- which(dataset[[tgt]]=="0")
  #  dataset[[tgt]] <- "0"
  #  dataset[idx,tgt] <- "1"
  #  cvfit = cv.glmnet(s[train,sig_all_sel],dataset[train,tgt], family = "binomial", type.measure = "class", weights=dataset[train,"wt_local"],pmax=11)
  lmfit <- lm(formula = CntTetSubsCurMth_WOE ~ accountSize, data = as.data.frame(s[train,sig_all_sel]))
  print(summary(lmfit))
  return(lmfit)
}

####
#WOE 界限提取
get_woe_new <- function(WOE_train){
  tmp_name<-unique(WOE_train$Var_Name)
  res<-c()
  for(name in tmp_name){
    tmp_woe<-WOE_train%>%filter(Var_Name==name)
    if(grepl("\\(",tmp_woe$Bin[1])&grepl("\\]",tmp_woe$Bin[1])){
      for(i in 1:nrow(tmp_woe)){
        s<-gregexpr("\\(",tmp_woe$Bin_Show[i])
        e<-gregexpr("\\,",tmp_woe$Bin_Show[i])
        s1<-gregexpr("\\,",tmp_woe$Bin_Show[i])
        e1<-gregexpr("\\]",tmp_woe$Bin_Show[i])
        tmp_woe[i,"start"]<-as.numeric(substr(tmp_woe$Bin_Show[i],s[[1]][1]+1,e[[1]][1]-1))
        tmp_woe[i,"end"]<-as.numeric(substr(tmp_woe$Bin_Show[i],s1[[1]][1]+1,e1[[1]][1]-1))
      }
    }else{
      tmp_woe[,name]<-tmp_woe$Bin
    }
    res[[name]]<-tmp_woe
  }
  return(res)
}
####  woe 替换
replace_woe <- function(data,woe){
  factor_flag<-factor_col_check(data)
  factor_names<-names(data)[factor_flag]
  num_names <- setdiff(names(data),factor_names)
  for(name in factor_names){
    tmp_woe<-woe[[name]]
    data%<>%left_join(tmp_woe[,c(name,"woe_value")])
    names(data)[length(data)]<-paste0("WOE_",name)
  }
  for(name in num_names){
    #name<-"var_55"
    tmp_woe<-woe[[name]]
    #tmp_woe<-woe[["var_55"]]
    tmp_data <- data[,name]%>%as.data.frame()
    names(tmp_data)[1]<-"var"
    tmp_data$woe<-NA
    for(i in 1:nrow(tmp_woe)){
      if(i==1){
        tmp_data%<>%mutate(woe=ifelse(var<=tmp_woe$end[1],tmp_woe$woe_value[1],woe))
      }else if(!is.na(tmp_woe$start[i])){
        if (!is.na(tmp_woe$end[i])){
          tmp_data%<>%mutate(woe=ifelse(var>tmp_woe$start[i]&var<=tmp_woe$end[i],tmp_woe$woe_value[i],woe))
        }else{
          tmp_data%<>%mutate(woe=ifelse(var>tmp_woe$start[i],tmp_woe$woe_value[i],woe))
        }
      }else if(is.na(tmp_woe$end[i])&is.na(tmp_woe$start[i])){
        tmp_data%<>%mutate(woe=ifelse(is.na(var),tmp_woe$woe_value[nrow(tmp_woe)],woe))
      }
    }
    data%<>%cbind(tmp_data%>%select(woe))
    names(data)[length(data)]<-paste0("WOE_",name)
  }
  return(data)
}


#检查返回为因子型的变量
factor_col_check<-function(data){
  factor_col <- c()
  for(i in 1:length(data)){
    if(class(data[,i])=="factor")
      factor_col[i]<- T
    else
      factor_col[i]<- FALSE
  }

  return (factor_col)
}

col_check<-function(data,string){
  factor_col <- c()
  for(i in 1:length(data)){
    if(class(data[,i])==string)
      factor_col[i]<- T
    else
      factor_col[i]<- FALSE
  }

  return (factor_col)
}


# 等间隔 切割点
fixstep <- function(x,n,limit=0.01){

  tmpmax <- quantile(x,(1-limit))
  tmpmin <- quantile(x,limit)
  tmpstep <- (tmpmax-tmpmin)/n
  x1 <- tmpmin+tmpstep*(1:n-1)
  x1[1] <- -Inf
  x1[length(x1)+1] <- Inf

  return(x1)
}

# PSI caculation
PSI <- function(df,r,b){
  #
  #判断：index <= 0.1，无差异；0.1< index <= 0.25，需进一步判断；
  #0.25 <= index，有显著位移，模型需调整。
  #(r-b)*ln(r/b)
  df$diff = (df[,r]-df[,b])*exp(df[,r]/df[,b])

  return(df)
}
