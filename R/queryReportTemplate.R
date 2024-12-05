#' @name queryReportTemplate
#' @title Retrieve Report Template Settings
#'
#' @description Provides the user with an interface to retrieve configuration
#'   settings of ReportTemplate objects. This will only return the settings
#'   in the primary ReportTemplate object. Other functions will give access
#'   to settings that are related through other tables. 
#'   
#' @param oid `integerish(0/1)`. The OID of the ReportTemplate object for 
#'   which data is to be retrieved. Use `numeric(0)` to return all templates.
#'   
#' @export

queryReportTemplate <- function(oid = numeric(0)){
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
                      "sqlite" = .queryReportTemplate_sqlite, 
                      "sql_server" = .queryReportTemplate_sqlServer,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  if (length(oid) > 0){
    statement <- paste0(statement, " WHERE [OID] = ?")
  }
  
  param_list <- list(oid)
  param_list <- param_list[lengths(param_list) > 0]
  
  ReportTemplate <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        .dots = param_list))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    ReportTemplate$IsSignatureRequired <- as.logical(ReportTemplate$IsSignatureRequired)
    ReportTemplate$IsActive <- as.logical(ReportTemplate$IsActive)
    ReportTemplate$IncludeTableOfContents <- as.logical(ReportTemplate$IncludeTableOfContents)
  }
  
  ReportTemplate
}

# Unexported --------------------------------------------------------

.queryReportTemplate_sqlite <- "
  SELECT [OID], 
         [Title],
         [TitleSize],
         [TemplateName],
         [TemplateDirectory], 
         [TemplateFile], 
         [IncludeTableOfContents],
         [DefaultEmailText],
         [IsSignatureRequired], 
         [IsActive],
         [LogoFileArchive], 
         [DateReportingFormat], 
         [SupportingDataFile]
  FROM [ReportTemplate]
"

.queryReportTemplate_sqlServer <- "
  SELECT [OID], 
         [Title],
         [TitleSize],
         [TemplateName],
         [TemplateDirectory], 
         [TemplateFile], 
         [IncludeTableOfContents],
         [DefaultEmailText],
         [IsSignatureRequired], 
         [IsActive], 
         [LogoFileArchive], 
         [DateReportingFormat],
         [SupportingDataFile]
  FROM dbo.[ReportTemplate]
"
