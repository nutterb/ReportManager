#' @name queryFileArchive
#' @title Retrieve FileARchive Records
#' 
#' @description Provides the user an interface to retrieve FileArchive
#'   objects from the database. 
#'   
#' @param oid `integerish(0/1)`. The OID of the FileArchive object to retrieve.
#' @param parent_report_template `integerish(0/1)`. The OID of the 
#'   ReportTemplate object with which the file is associated.
#' @param parent_report_instance `integerish(0/1)`. The OID of the 
#'   ReportInstance object with which the file is associated. 
#'   
#' @export

queryFileArchive <- function(oid = numeric(0), 
                             parent_report_template = numeric(0), 
                             parent_report_instance = numeric(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_instance, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sql_server" = .queryFileArchive_statement_sqlServer, 
           "sqlite"     = .queryFileArchive_statement_sqlite)
  
  where <- 
    c(if (length(oid)) "[OID] = ?" else character(0), 
      if (length(parent_report_template)) "[ParentReportTemplate] = ?" else character(0), 
      if (length(parent_report_instance)) "[ParentReportInstance] = ?" else character(0))
  
  if (length(where) > 0 ){
    where <- sprintf("WHERE %s", 
                     paste0(where, collapse = " AND "))
    statement <- paste(statement, where, sep = "\n")
  }
  
  param_list <- list(oid, parent_report_template, parent_report_instance)
  param_list <- param_list[lengths(param_list) > 0]
  
  FileArchive <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    FileArchive$IsLogo <- as.logical(FileArchive$IsLogo)
  }
  
  FileArchive
}

# Unexported --------------------------------------------------------

.queryFileArchive_statement_sqlServer <- "
  SELECT [OID], 
         [ParentReportTemplate], 
         [ParentReportInstance], 
         [Description], 
         [CreatedDateTime], 
         [IsLogo], 
         [FileName],
         [FileExtension], 
         [FileSize]
  FROM dbo.[FileArchive]
"

.queryFileArchive_statement_sqlite <- "
  SELECT [OID], 
         [ParentReportTemplate], 
         [ParentReportInstance], 
         [Description], 
         [CreatedDateTime], 
         [IsLogo], 
         [FileName],
         [FileExtension], 
         [FileSize]
  FROM [FileArchive]
"
