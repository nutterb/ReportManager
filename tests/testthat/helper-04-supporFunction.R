randomVarchar <- function(len = 10){
  x <- sample(x = c(LETTERS, letters, 0:9, " "), 
              size = len, 
              replace = TRUE)
  
  paste0(x, collapse = "")
}
