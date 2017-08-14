#-------------------------------------------------------------------------------------------------------------------------------------#
##Infix function to "safely" add ggplot themes together. This only adds themes that are not already set and thus can be considered "safe".
##I.e. a theme element will only be added if it is not already NULL
`%+safe%` <- function(e1, e2){
  if (!is.theme(e1) || !is.theme(e2)) {
    stop("%+safe% requires two theme objects", call. = FALSE)
  }

  not_in_e1 <- names(e2)[!names(e2) %in% names(e1)]
  e1[not_in_e1] <- e2[not_in_e1]
  e1
}
