##Infix function to select range of variables based on name. Inspired by SAS "-" operator
`%--%` <- function(x,y){
  s <- getEnvi(1)
  #s <- as.list(sys.call(-1))
  val <- get(deparse(s[[2]]))
  match(x,names(val)):match(y,names(val))
}
#Helper function to get the environment
#Note - must use sys.call() because "match.call does not support primitive functions (where argument matching is normally positional)."
getEnvi <- function(i) {
  if(any(sapply(as.list(sys.call(-i)), function(x) "[.data.frame" == x)) | 
     any(sapply(as.list(sys.call(-i)), function(x) "[.data.table" == x))) { 
    as.list(sys.call(-i))}
  
  else getEnvi(i+2)
}

###Usage:
#library(data.table)
#df <- data.frame(day = c(1,2,3), x = c(1,2,3), price = c(1,2,3), price2 = c(1,2,3), price3 = c(1,2,3), y= c(1,2,3))
#df[, c("day" %--% "price", "price3" %--% "y")]

#dt <- as.data.table(df)
#dt[, c("day" %--% "price", "price3" %--% "y")]

#-------------------------------------------------------------------------------------------------------------------------------------#
##Infix function to "safely" add ggplot themes together. This only adds themes that are not already set and thus can be considered "safe".
##I.e. a theme element will only be added if it is not already NULL
`%+safe%` <- function(e1, e2){
  if (!is.theme(e1) || !is.theme(e2)) {
    stop("%+replace% requires two theme objects", call. = FALSE)
  }

  not_in_e1 <- names(e2)[!names(e2) %in% names(e1)]
  e1[not_in_e1] <- e2[not_in_e1]
  e1
}
