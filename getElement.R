getElement <- function(inList, elements){
  Reduce("[[", c(list(inList), elements))
}

#Sample usage
# nested_list = list(a = list(b = 1, c = 2, d = list(a = 2, b = 3)))
# getElement(nested_list, list("a","d",2))
# getElement(nested_list, c("a","b"))
