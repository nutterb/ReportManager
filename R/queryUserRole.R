#' @name queryUserRole
#' @title Retrieve User Role Assignments
#' 
#' @description Executes a query to retrieve the UserRole assignments from the
#'   database. 
#'   
#' @param oid `integerish(0/1)`. When length 0, all records are returned. 
#'   Otherwise, only the requested record is returned. 
#'
#' @export

queryUserRole <- function(oid = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sql_server" = .queryUserRole_statement_sqlServer, 
           "sqlite"     = .queryUserRole_statement_sqlite)
  
  if (length(oid) > 0){
    statement <- paste0(statement, " WHERE UR.[OID] = ", oid)
  }
  
  UserRole <- DBI::dbGetQuery(conn, statement)
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    UserRole$IsActive <- as.logical(UserRole$IsActive)
    UserRole$IsActiveUser <- as.logical(UserRole$IsActiveUser)
    UserRole$IsActiveRole <- as.logical(UserRole$IsActiveRole)
  }
  
  UserRole
}


# Unexported --------------------------------------------------------

.queryUserRole_statement_sqlServer <- "
  SELECT UR.[OID], 
         UR.[ParentUser], 
         UR.[ParentRole], 
         UR.[IsActive], 
         U.[LastName], 
         U.[FirstName], 
         U.[IsActive] AS IsActiveUser,
         R.[RoleName], 
         R.[IsActive] AS IsActiveRole
  FROM dbo.[UserRole] UR
    LEFT JOIN dbo.[User] U
      ON UR.[ParentUser] = U.[OID]
    LEFT JOIN dbo.[Role] R
      ON UR.[ParentRole] = U.[OID]
"

.queryUserRole_statement_sqlite <- "
  SELECT UR.[OID], 
         UR.[ParentUser], 
         UR.[ParentRole], 
         UR.[IsActive], 
         U.[LastName], 
         U.[FirstName], 
         U.[IsActive] AS IsActiveUser,
         R.[RoleName], 
         R.[IsActive] AS IsActiveRole
  FROM [UserRole] UR
    LEFT JOIN [User] U
      ON UR.[ParentUser] = U.[OID]
    LEFT JOIN [Role] R
      ON UR.[ParentRole] = U.[OID]
"
