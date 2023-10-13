#' @name compareValues
#' @title Compare Values with Consideration for Missing Values
#' 
#' @description Determines if there are pair-wise differences between
#'   two vectors while accounting for missing values. 
#'   
#' @param current `atomic`. Values to be compared against `new`.
#' @param new `atomic`. Values to be compared against `current`.

compareValues <- function(current, new){
    p1 <- is.na(current) & !is.na(new)
    p2 <- !is.na(current) & is.na(new)
    p3 <- vapply(current != new, isTRUE, logical(1))
    
    p1 | p2 | p3
}
