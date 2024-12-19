#' @name dataframeToHtml
#' @title Convert a Data Frame to HTML for Display in Shiny
#' 
#' @description Convert a data frame to HTML code
#' 
#' @param dframe `data.frame`. The data frame to convert to HTML

dataframeToHtml <- function(dframe){
  nms <- names(dframe)
  nms <- sprintf("<td><b>%s</b></td>", 
                 nms)
  nms <- paste0(nms, collapse = "")
  nms <- sprintf("<tr>%s</tr>", nms)
  
  content <- lapply(dframe, 
                    function(x) sprintf("<td>%s</td>", x))
  content <- as.data.frame(content)
  content <- apply(content, 
                   MARGIN = 1, 
                   FUN = paste0, 
                   collapse = "")
  content <- sprintf("<tr>%s</tr>", content)
  
  tab <- c(nms, content)
  tab <- paste0(tab, collapse = "")
  tab <- sprintf("<table>%s</table>", tab)
  
  tab
}
