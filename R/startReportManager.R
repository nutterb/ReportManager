#' @name startReportManager
#' @title Start the Report Manager Application
#' 
#' @description Starts the Report Manager Application
#' 
#' @param email `character(1)`. May be either `"live"`, `"off"` or an 
#'   email address. When `"live"`, emails generated by the application are 
#'   sent in accordance with the configurations in the database. When 
#'   `"off"`, no e-mails are sent. If the user provides an e-mail address, 
#'   all e-mails will be sent to the address provided.
#' @param ... Arguments to pass to [shiny::runApp()], such as 
#'   `launch.browser` or `port`.
#'   
#' @export

startReportManager <- function(email, 
                               ...){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = email, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Preserve original options settings ------------------------------
  orig_email_opt <- getOption("ReportManager_email")
  
  on.exit({
    options(ReportManager_email = orig_email_opt)
  })
  
  # Set options and run ---------------------------------------------
  options(ReportManager_email = email)
  
  shiny::runApp(system.file("Application", 
                            package = "ReportManager"), 
                ...)
}
