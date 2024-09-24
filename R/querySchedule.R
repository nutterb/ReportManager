#' @name querySchedule 
#' @title Retrieve Schedule Objects from the Database
#' 
#' @description Executes a query to get the Schedule objects from the 
#'   database. 
#'   
#' @param oid `integerish(0/1)`. When length 0, all schedule objects are 
#'   returned. When length 1, only the schedule matching `oid` is returned.
#'   
#' @export

querySchedule <- function(oid = numeric(0)){
  
  # Argument validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sql_server" = .querySchedule_statement_sqlServer, 
                      "sqlite" = .querySchedule_statement_sqlite)
  
  if (length(oid) > 0){
    statement <- sprintf("%s WHERE [OID] = ?", 
                         statement)
  }
  
  param_list <- list(oid)
  param_list <- param_list[lengths(param_list) > 0]
  
  Schedule <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    Schedule$IsActive <- as.logical(Schedule$IsActive)
    Schedule$IsPeriodToDate <- as.logical(Schedule$IsPeriodToDate)
  }
  
  Schedule
}

# Unexported --------------------------------------------------------

.querySchedule_statement_sqlServer <- "
  SELECT [OID], 
         [ScheduleName], 
         [Frequency],
         [FrequencyUnit],
         [OffsetOverlap], 
         [OffsetOverlapUnit],
         [IsActive], 
         [IsPeriodToDate]
  FROM dbo.[Schedule]
"

.querySchedule_statement_sqlite <- "
  SELECT [OID], 
         [ScheduleName], 
         [Frequency],
         [FrequencyUnit],
         [OffsetOverlap], 
         [OffsetOverlapUnit],
         [IsActive], 
         [IsPeriodToDate]
  FROM [Schedule]
"
