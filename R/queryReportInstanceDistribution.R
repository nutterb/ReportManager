#' @name queryReportInstanceDistribution
#' @title Retrieve ReportInstanceDistribution Objects
#' 
#' @description Provides the user with an interface to retrieve 
#'   ReportInstanceDistribution objects.
#'   
#' @param oid `integerish(0/1)`. When length is 1, the OID of the 
#'   ReportInstanceDistribution to be retrieved. Otherwise, all 
#'   records are returned.
#' @param parent_report_instance `integerish(0/1)`. When length is 1, the 
#'   OID of the ReportInstance to include in the query clause. Otherwise, 
#'   all records are returned.
#' @param parent_user `integerish(0/1)`. When length is 1, the OID
#'   of the User to include in the query clause. Otherwise, all 
#'   records are returned. 
#' @param parent_role `integerish(0/1)`. When length is 1, the OID
#'   of the Role to include in the query clause. Otherwise, all 
#'   records are returned.
#'   
#' @details The WHERE clause for the query is constructed using any of the
#'   `oid`, `parent_report_instance`, `parent_user` or `parent_role` arguments 
#'   that have a non-zero length. The query returns all records that 
#'   satisfy all of the conditions.  The ReportInstanceDistribution object 
#'   should only have either a ParentUser or ParentRole value. If both 
#'   `parent_user` and `parent_role` are given, it is expected that no
#'   rows will be returned.
#'   
#' @export

queryReportInstanceDistribution <- function(oid = numeric(0), 
                                            parent_report_instance = numeric(0),
                                            parent_user = numeric(0),
                                            parent_role = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_instance, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_user, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_role, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sql_server" = .queryReportInstanceDistribution_statement_sqlServer, 
           "sqlite"     = .queryReportInstanceDistribution_statement_sqlite)
  
  where <- 
    c(if (length(oid)) "OID = ?" else character(0), 
      if (length(parent_report_instance)) "ParentReportInstance = ?" else character(0), 
      if (length(parent_user)) "ParentUser = ?" else character(0), 
      if (length(parent_role)) "ParentRole = ?" else character(0))
  
  if (length(where) > 0 ){
    where <- sprintf("WHERE %s", 
                     paste0(where, collapse = " AND "))
    statement <- paste(statement, where, sep = "\n")
  }
  
  param_list <- list(oid, parent_report_instance, parent_user, parent_role)
  param_list <- param_list[lengths(param_list) > 0]
  
  ReportInstanceDistribution <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    ReportInstanceDistribution$IsActive <- as.logical(ReportInstanceDistribution$IsActive)
  }

  ReportInstanceDistribution
}


# Unexported --------------------------------------------------------

.queryReportInstanceDistribution_statement_sqlServer <- "
  SELECT [OID], 
         [ParentReportInstance],
         [ParentUser],
         [ParentRole], 
         [IsActive]
  FROM dbo.[ReportInstanceDistribution]
"

.queryReportInstanceDistribution_statement_sqlite <- "
  SELECT [OID], 
         [ParentReportInstance], 
         [ParentUser],
         [ParentRole], 
         [IsActive]
  FROM [ReportInstanceDistribution]
"
