# -------------------------------------------------------------------
# File Name: 
# Description: 

#' @name launch_application
#' @title Launch the Reports Manager Application
#' 
#' @description Starts the Reports Manager Shiny Application
#' 
#' @param app_dir \code{character} with a maximum length of 1. Path to the 
#'   directory with the Reports Manager application
#' @param ... Additional arguments to pass to \code{\link[shiny]{runApp}}
#' 
#' @author Benjamin Nutter
#' @export

launch_application <- function(app_dir = character(0), ...){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = app_dir, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  if (length(app_dir) == 0){
    app_dir <- system.file("Application", 
                           package = "ReportManager")  
  }
  
  tryCatch(
    {
      shiny::runApp(appDir = app_dir, 
                    ...)
    },
    error = function(cond){
      warning("Reports Manager Application failed to launch with error: ", cond)
    }
  )
}

# -------------------------------------------------------------------
# DateTime      UserName      Notes
# 2021-09-03    bnutter       File created.
# 