###Doesn't work with gtable objects. Need to extract the object you want first then it will work.
##I.e. if p is a gtable then smartFind(list = p, map = list("grobs",1)) wont work. But p = p[["grobs"]], map = list(1) will....
smartFind <- function(list, map){

  #You are at end of map and find a match, return the element
  if ((length(map) == 1 & is.character(map[[1]]) & map[[1]] %in% names(list)) |
      (length(map) == 1 & is.integer(map[[1]])   & map[[1]] <= length(list))){

    if(is.character(map[[1]])) matches <- which(names(list) == map[[1]])
    else if(is.integer(map[[1]])) matches <- map[[1]]
    
    if (length(matches) == 1) return(list[[map[[1]]]])
    else return(list[matches])
  } 
  
  #Your object contains a list and there is a match - extract elements that match and unlist/unname them
  else if( (is.character(map[[1]]) && (any(sapply( list[ which(names(list) == map[[1]]) ] ,is.list)) & map[[1]] %in% names(list))) |
           (is.integer(map[[1]]) && (any( sapply(list[map[[1]] ], is.list))) ) ){

    if (is.character(map[[1]])) new_list1 <- list[ which(names(list) == map[[1]]) ]
    else if (is.integer(map[[1]])) new_list1 <- list[ map[[1]] ]
    
    new_list1_unname <- unname( new_list1[ which(sapply(new_list1,is.list)) ] )
    new_list <- unlist(new_list1_unname, recursive=F)
    
    smartFind(list = new_list, map = map[-1])
  }
  
  #There are more lists, but your MAP cant find anything - keep looking!! Only supports character right now
  else if ( (is.character(map[[1]]) && any(sapply(list,is.list)) & !map[[1]] %in% names(list)) |
            (is.integer(map[[1]])   && any(sapply(list,is.list)) & !map[[1]] <= length(list))){
    
    new_list1_unname <- unname( list[ which(sapply(list,is.list)) ] )
    new_list <- unlist(new_list1_unname, recursive=F)
    
    smartFind(list = new_list, map = map)
  }
  
  #No more lists to search
  else if ( !any(sapply(list,is.list)) ){
    return(NULL)
  }
  
}

##Check 1 - simple list & map
# ls <- list(a=1,b=2)
# smartFind(list = ls, map = list("a"))
# smartFind(list = ls, map = list("c"))

##Check 2 - first nested list
# ls <- list(a=1,b=list(c=17,d=4))
# smartFind(list = ls, map = list("b","c"))

##Check 3 - More than two elements with same name
# ls <- list(a=1,b=list(c=17,d=4, list(d=100)), b=list(c=list(f=10), list(f=100)), c=list(d=list(d=100), c= 12))
# smartFind(list = ls, map = list("b", "c", "f"))
# smartFind(list = ls, map = list("b", 1L))
# smartFind(list = ls, map = list("d"))

##Other checks
# smartFind(list = list(1, list(2,3,list(3,4,5))), map = c(3L,1L))
# 

##GgplotGrob example - finding elements within nested lists
# library(ggplot2)
# p <- ggplotGrob(ggplot(data = data.frame(x=1:10,y=11:20)) + geom_point(aes(x=x, y=y)))
# 
# smartFind(list = p[["grobs"]], map = list( 3L, "children", 2L, "grobs", 1L, "children", 1L, "y"))
# smartFind(list = p[["grobs"]], map = list( 3L, "children","y")) 
