compareInstanceTemplateDistribution <- function(x, y){
  sub <- function(x, y){
    if (isTRUE(x)){
      if (is.na(y)) return(TRUE) else return(x != y)
    } 
    
    if (isFALSE(x)){
      if (is.na(y)) return(FALSE) else return(x != y)
    }
  }
  
  mapply(sub, x, y, SIMPLIFY = TRUE)
}
