#' @name queryDateReportingFormat
#' @title Retrieve DateReportingFormat Objects from the Database
#' 
#' @description Executes a query to retrieve DataReportingFormat objects
#'   from the database. 
#'   
#' @param oid `integerish(0/1)`. The OID of the DataReportingFormat object
#'   to return. When length 0, all objects are returned.
#' 
#' @export

queryDateReportingFormat <- function(oid = numeric(0)){
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
                      "sql_server" = .queryDateReportingFormat_statement_sqlServer, 
                      "sqlite" = .queryDateReportingFormat_statement_sqlite)
  
  if (length(oid) > 0){
    statement <- sprintf("%s WHERE [OID] = %s", 
                         statement, 
                         oid)
  }
  
  DateReportingFormat <- DBI::dbGetQuery(conn, 
                                         statement)
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    DateReportingFormat$IsActive <- as.logical(DateReportingFormat$IsActive)
  }
  
  DateReportingFormat
}

# Unexported --------------------------------------------------------

.queryDateReportingFormat_statement_sqlServer <- "
  SELECT [OID], 
         [FormatName], 
         [Description],
         [FormatCode],
         [IncrementStart],
         [IncrementStartUnit],
         [IncrementEnd], 
         [IncrementEndUnit],
         [IsActive]
  FROM dbo.[DateReportingFormat]
"

.queryDateReportingFormat_statement_sqlite <- "
  SELECT [OID], 
         [FormatName], 
         [Description],
         [FormatCode],
         [IncrementStart],
         [IncrementStartUnit],
         [IncrementEnd], 
         [IncrementEndUnit],
         [IsActive]
  FROM [DateReportingFormat]
"
