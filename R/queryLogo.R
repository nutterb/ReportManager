#' @name queryLogo
#' @title Retrieve FileArchive Objects Marked as Logos
#' 
#' @description Retrieves the FileArchive objects marked as Logos. These 
#'   can be made available to display with the title of the report.
#'   
#' @export

queryLogo <- function(){
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .queryLogo_sqlite, 
                      "sql_server" = .queryLogo_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  DBI::dbGetQuery(conn, 
                  statement)
}

# Unexported --------------------------------------------------------

.queryLogo_sqlite <- 
  "SELECT [OID], 
      [Description],
      [FileName],
      [FileExtension], 
      [CreatedDateTime]
    FROM [FileArchive]
    WHERE [IsLogo] = 1"

.queryLogo_sqlServer <- 
  "SELECT [OID], 
      [Description],
      [FileName],
      [FileExtension], 
      [CreatedDateTime]
    FROM dbo.[FileArchive]
    WHERE [IsLogo] = 1"
