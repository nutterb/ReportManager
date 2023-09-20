#' @name initializeReportManagerDatabase
#' @title Intialize the Report Manager Database
#'
#' @description Retrieves the SQL code to define the Report Manager 
#'   database tables and runs it through the appropriate connection. 
#'   
#' @param filename `character(1)`. A filename of SQL code. Usually 
#'   one found in `system.file("Sql", package = "ReportManager")`
#'   
#' @export

initializeReportManagerDatabase <- function(filename){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = filename, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assertFileExists(x = filename, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------

  conn <- connectToReportManager()
  
  on.exit({ DBI::dbDisconnect(conn) })
  
  
  # Determine start and finish of individual statements
    
  sql_code <- readLines(filename)
  
  statement_end <- which(grepl("^[)];", sql_code))
  statement_start <- c(1, head(statement_end + 1, -1))
  

  for (i in seq_along(statement_start)){
    statement <- sql_code[statement_start[i] : statement_end[i]]
    statement <- paste0(statement, collapse = " ")
    result <- DBI::dbSendStatement(conn, 
                                   statement)
    DBI::dbClearResult(result)
    
  }
}
