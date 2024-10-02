makeTemplatePermissionData <- function(report_template_oid){
  conn <- connectToReportManager()
  on.exit(DBI::dbDisconnect(conn))

  DBI::dbGetQuery(
    conn, 
    DBI::sqlInterpolate(
      conn, 
      .makeTemplatePermissionData_statement(), 
      report_template_oid = report_template_oid
    )
  )
}

.makeTemplatePermissionData_statement <- function(flavor = getOption("RM_sql_flavor")){
  schema <- if (flavor == "sql_server") "dbo." else ""
  
  paste0("SELECT RTP.OID, 
    R.RoleName,
    RTP.CanView, 
    RTP.CanAddNotes, 
    RTP.CanEditNarrative,
    RTP.CanSubmit, 
    RTP.CanStartRevision, 
    RTP.IsActive
  FROM ", schema, "ReportTemplatePermission RTP
    LEFT JOIN ", schema, "[Role] R
      ON RTP.ParentRole = R.OID
  WHERE RTP.ParentReportTemplate = ?report_template_oid
    AND R.IsActive = 1")
}
