#' @name queryRole
#' @title Query Roles From the ReportManager Database
#' 
#' @description Retrieves the roles from the database. 
#' 
#' @param oid `integerish(0/1)`. When length is 0, all roles are returned. 
#'   Otherwise, only the requested role is returned.
#'   
#' @export

queryRole <- function(oid = numeric(0)){
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
                      "sqlite" = .queryRole_sqlite, 
                      "sql_server" = .queryRole_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(oid) > 0){
    statement <- paste0(statement, " WHERE [OID] = ", oid)
  }
  
  Role <- DBI::dbGetQuery(conn, statement)
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    Role$IsActive <- as.logical(Role$IsActive)
  }
  
  Role
}

# Unexported --------------------------------------------------------

.queryRole_sqlServer = 
  "SELECT [OID], 
          [RoleName], 
          [RoleDescription], 
          [IsActive]
   FROM dbo.[Role]"

.queryRole_sqlite <- 
  "SELECT [OID], 
          [RoleName], 
          [RoleDescription], 
          [IsActive]
   FROM [Role]"