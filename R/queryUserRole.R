#' @name queryUserRole
#' @title Retrieve User Role Assignments
#' 
#' @description Executes a query to retrieve the UserRole assignments from the
#'   database. 
#'   
#' @param oid `integerish(0/1)`. When length 0, all records are returned. 
#'   Otherwise, the UserRole.OID will be included in the WHERE clause. 
#' @param user_oid `integerish(0/1)`. When length 1, the User.OID is included
#'   in the WHERE clause. Otherwise, the User OID is not considered in the
#'   query.
#' @param role_oid `integerish(0/1)`. When length 1, the Role.OID is included 
#'   in the WHERE clause. Otherwise, the Role OID is not considered in the 
#'   query.
#'
#' @export

queryUserRole <- function(oid = numeric(0), 
                          user_oid = numeric(0), 
                          role_oid = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = user_oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = role_oid, 
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
  
  where <- 
    c(if (length(oid)) sprintf("UR.[OID] = ?", oid) else character(0), 
      if (length(user_oid)) sprintf("U.[OID] = ?", user_oid) else character(0), 
      if (length(role_oid)) sprintf("R.[OID] = ?", role_oid) else character(0))
  
  if (length(where) > 0 ){
    where <- sprintf("WHERE %s", 
                     paste0(where, collapse = " AND "))
    statement <- paste(statement, where, sep = "\n")
  }
  
  param_list <- list(oid, user_oid, role_oid)
  param_list <- param_list[lengths(param_list) > 0]
  
  UserRole <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
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
         R.[RoleName], 
         U.[IsActive] AS IsActiveUser,
         R.[IsActive] AS IsActiveRole
  FROM dbo.[UserRole] UR
    LEFT JOIN dbo.[User] U
      ON UR.[ParentUser] = U.[OID]
    LEFT JOIN dbo.[Role] R
      ON UR.[ParentRole] = R.[OID]
"

.queryUserRole_statement_sqlite <- "
  SELECT UR.[OID], 
         UR.[ParentUser], 
         UR.[ParentRole], 
         UR.[IsActive], 
         U.[LastName], 
         U.[FirstName], 
         R.[RoleName],
         U.[IsActive] AS IsActiveUser,
         R.[IsActive] AS IsActiveRole
  FROM [UserRole] UR
    LEFT JOIN [User] U
      ON UR.[ParentUser] = U.[OID]
    LEFT JOIN [Role] R
      ON UR.[ParentRole] = R.[OID]
"
