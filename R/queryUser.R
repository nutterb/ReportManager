#' @name queryUser
#' @title Fetch the User Table
#' 
#' @description Fetch the User table from the database.
#' 
#' @param oid `integerish(0/1)`. The OID of the Report User to fetch
#'   from the database. By default (`character(0)`), all records are 
#'   returned.  
#' 
#' @export

queryUser <- function(oid = numeric(0)){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .queryUser_sqlite, 
                      "sql_server" = .queryUser_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(oid) > 0){
    statement <- paste0(statement, " WHERE OID = ", oid)
  }
  
  User <- DBI::dbGetQuery(conn, statement)
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    User$IsInternal <- as.logical(User$IsInternal)
    User$IsActive <- as.logical(User$IsActive)
  }
  
  User
}

# Unexported --------------------------------------------------------

.queryUser_sqlite <- 
  "SELECT [OID], 
      [LastName],
      [FirstName],
      [LoginId],
      [EmailAddress], 
      [IsInternal], 
      [IsActive]
    FROM [User]"

.queryUser_sqlServer <- 
  "SELECT [OID], 
      [LastName], 
      [FirstName], 
      [LoginId],
      [[EmailAddress], 
      [IsInternal], 
      [IsActive]
    FROM dbo.[User]"