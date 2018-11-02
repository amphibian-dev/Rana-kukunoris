get_target_str <- function(str,x,y){
  if(!is.numeric(a)){
    a <- as.numeric(gregexpr(x,str)[[1]][1])
  }
  if(!is.numeric(b)){
    b <- as.numeric(gregexpr(y,str)[[1]][1])
  }
  return(as.numeric(substr(str,a+1,b-1)))
}