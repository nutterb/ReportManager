RM_datatable <- function(data, 
                         rownames = FALSE, 
                         selection = "none", 
                         filter = "top", 
                         class = "compact cell-border", 
                         ...){
  DT::datatable(data, 
                rownames = rownames, 
                selection = selection, 
                filter = filter, 
                class = class, 
                ...)
}