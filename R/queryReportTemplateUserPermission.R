#' @name queryReportTemplateUserPermission
#' @title Retrieve the Permissions Granted a User on a Report Template
#' 
#' @description Enables the user to retrieve the permissions granted to a user
#'   on a selected report template. Typically this is used to determine if 
#'   the current user has permission to perform actions on a template.
#'   
#' @param parent_report_template `integerish(1)` The OID of the Report Template
#'   for which permissions are being checked.
#' @param parent_user `integerish(1)`. The OID of the User for whom permissions
#'   are being checked.
#'   
#' @export

queryReportTemplateUserPermission <- function(parent_report_template, 
                                              parent_user){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"),
                      "sqlite" = queryReportTemplateUserPermission_sqlite,
                      "sql_server" = queryReportTemplateUserPermission_sqlServer, 
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  Permission <- 
    DBI::dbGetQuery(
      conn,
      DBI::sqlInterpolate(
        conn,
        statement, 
        parent_report_template = parent_report_template, 
        parent_user = parent_user
      )
    )

  if (getOption("RM_sql_flavor") == "sqlite"){
    Permission$CanView <- as.logical(Permission$CanView)
    Permission$CanAddNotes <- as.logical(Permission$CanAddNotes)
    Permission$CanEditNarrative <- as.logical(Permission$CanEditNarrative)
    Permission$CanSubmit <- as.logical(Permission$CanSubmit)
    Permission$CanStartRevision <- as.logical(CanStartRevision)
  }
  
  Permission
}

# Unexported --------------------------------------------------------

queryReportTemplateUserPermission_sqlite <- "
SELECT RTP.[ParentReportTemplate],
	UR.[ParentUser],
	CAST(MAX(RTP.[CanView] + 0) AS BIT) AS CanView, 
	CAST(MAX(RTP.[CanAddNotes] + 0) AS BIT) AS CanAddNotes,
	CAST(MAX(RTP.[CanEditNarrative] + 0) AS BIT) AS CanEditNarrative, 
	CAST(MAX(RTP.[CanSubmit] + 0) AS BIT) AS CanSubmit, 
	CAST(MAX(RTP.[CanStartRevision] + 0) AS BIT) AS CanStartRevision
FROM ReportTemplatePermission RTP
	LEFT JOIN UserRole UR
		ON RTP.[ParentRole] = UR.[ParentRole]
  WHERE RTP.[ParentReportTemplate] = ?parent_report_template
  	AND RTP.[IsActive] = 1
  	AND UR.[ParentUser] = ?parent_user
GROUP BY RTP.[ParentReportTemplate],  
	UR.[ParentUser]
"

queryReportTemplateUserPermission_sqlServer <- "
SELECT RTP.[ParentReportTemplate],
	UR.[ParentUser],
	CAST(MAX(RTP.[CanView] + 0) AS BIT) AS CanView, 
	CAST(MAX(RTP.[CanAddNotes] + 0) AS BIT) AS CanAddNotes,
	CAST(MAX(RTP.[CanEditNarrative] + 0) AS BIT) AS CanEditNarrative, 
	CAST(MAX(RTP.[CanSubmit] + 0) AS BIT) AS CanSubmit, 
	CAST(MAX(RTP.[CanStartRevision] + 0) AS BIT) AS CanStartRevision
FROM dbo.ReportTemplatePermission RTP
	LEFT JOIN dbo.UserRole UR
		ON RTP.[ParentRole] = UR.[ParentRole]
  WHERE RTP.[ParentReportTemplate] = ?parent_report_template
  	AND RTP.[IsActive] = 1
  	AND UR.[ParentUser] = ?parent_user
GROUP BY RTP.[ParentReportTemplate],  
	UR.[ParentUser]
"
