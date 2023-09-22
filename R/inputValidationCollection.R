#' @name inputValidationCollection
#' @title Create an Input Validation Collection 
#' 
#' @description Creates an objet to assist in collecting input validation 
#'   findings and reporting them to the user. 
#' 
#' @export

inputValidationCollection <- function(){
  is_ok <- TRUE
  msg <- character(0)
  
  list(
    is_ok = function() is_ok,
    invalidate = function(message){
      is_ok <<- FALSE
      msg <<- c(msg, message)
    }, 
    report = function() paste0(sprintf("* %s", msg), collapse = "\n")
  )
}
