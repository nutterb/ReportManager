#' @name queryUserSignature 
#' @title Retrieve User Signature Files
#' 
#' @description Returns one or more User Signature Files from the database.
#' 
#' @param user_oid `integerish(0/1)`. The OID of the user. If the length is zero, 
#'   all records are returned.
#'   
#' @export

queryUserSignature <- function(user_oid = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = user_oid, 
                              max.len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .queryUserSignature_sqlite, 
                      "sql_server" = .queryUserSignature_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(user_oid) > 0){
    statement <- paste0(statement, " WHERE [OID] = ?")
  }
  
  param_list <- list(user_oid)
  param_list <- param_list[lengths(param_list) > 0]
  
  UserSignature <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  UserSignature
}

# Unexported --------------------------------------------------------

.queryUserSignature_sqlServer = 
  "SELECT [OID], 
          [ParentUser], 
          [FileName], 
          [FileExtension],
          [FileSize], 
          [FileContent]
   FROM dbo.[UserSignature]"

.queryUserSignature_sqlite <- 
  "SELECT [OID], 
          [ParentUser], 
          [FileName], 
          [FileExtension],
          [FileSize], 
          [FileContent]
   FROM [UserSignature]"
