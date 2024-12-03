#' @name queryReportInstanceSignature
#' @title Retrieve Signatures on Report Instances
#' 
#' @description Provides the user with the tools to retrieve signature events
#' for a report instance. 
#' 
#' @param report_instance_oid `integerish(1)`. The OID of the report instance
#'   for which signatures are being retrieved.
#'   
#' @details Data returned ordered in the order in which the signing role 
#'   will appear on the report. The most recent signature for each role is
#'   sorted to the top. Inactive signature roles are sorted to the bottom. 
#'   The most recent signature is identified where `MostRecentSignature` equals
#'   1.
#'   
#' @export

queryReportInstanceSignature <- function(report_instance_oid){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()

  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  ReportInstance <- queryReportInstance(report_instance_oid)
  
  # If a report instance isn't yet defined, we don't want the query to fail.
  # Use a negative number to get the query to return an empty data frame.
  report_template_oid <- 
    if (nrow(ReportInstance) == 0) -1 else ReportInstance$ParentReportTemplate
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .queryReportInstanceSignature_sqlite, 
           "sql_server" = .queryReportInstanceSignature_sqlServer, 
           stop(sprintf("Query not defined for SQL flavor '%s'", 
                        getOption("RM_sql_flavor"))))
  
  Signature <-
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn,
        statement,
        report_instance_oid = report_instance_oid
      )
    )
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    Signature$IsActive <- as.logical(Signature$IsActive)
  }

  Signature
}

# Unexported --------------------------------------------------------

.queryReportInstanceSignature_sqlite <- "
SELECT RI.[ParentReportTemplate],
	RI.[OID] AS ParentReportInstance, 
	RTS.[ParentRole],
	RIS.ParentReportTemplateSignature,
	RTS.OID AS ReportTemplateSignatureOID,
	R.[RoleName], 
	RTS.[Order],
	RIS.[ParentUser], 
	RIS.[SignatureName],
	RIS.[SignatureDateTime], 
	CAST(CASE WHEN RIS.IsSigned IS NULL THEN 0 ELSE RIS.IsSigned END AS BIT) AS IsSigned,
	RTS.[IsActive],
	ROW_NUMBER() OVER (PARTITION BY RTS.[ParentRole] 
	                   ORDER BY RIS.[SignatureDateTime] DESC) AS MostRecentSignature
FROM [ReportTemplateSignature] RTS
	LEFT JOIN [ReportTemplate] RT
		ON RTS.[ParentReportTemplate] = RT.[OID]
	LEFT JOIN [ReportInstance] RI
		ON RT.[OID] = RI.[ParentReportTemplate]
	LEFT JOIN [ReportInstanceSignature] RIS
		ON RI.[OID] = RIS.[ParentReportInstance]
			AND RIS.[ParentReportTemplateSignature] = RTS.[OID]
	LEFT JOIN [Role] R
		ON RTS.[ParentRole] = R.[OID]
WHERE RI.[OID] = ?report_instance_oid
ORDER BY RTS.[IsActive] DESC, 
	RTS.[Order], 
	RIS.[SignatureDateTime] DESC
"

.queryReportInstanceSignature_sqlServer <- "
SELECT RI.[ParentReportTemplate],
	RI.[OID] AS ParentReportInstance, 
	RTS.[ParentRole],
	RIS.ParentReportTemplateSignature,
	RTS.OID AS ReportTemplateSignatureOID,
	R.[RoleName], 
	RTS.[Order],
	RIS.[ParentUser], 
	RIS.[SignatureName],
	RIS.[SignatureDateTime], 
	CAST(CASE WHEN RIS.IsSigned IS NULL THEN 0 ELSE RIS.IsSigned END AS BIT) AS IsSigned,
	RTS.[IsActive],
	ROW_NUMBER() OVER (PARTITION BY RTS.[ParentRole] 
	                   ORDER BY RIS.[SignatureDateTime] DESC) AS MostRecentSignature
FROM dbo.[ReportTemplateSignature] RTS
	LEFT JOIN dbo.[ReportTemplate] RT
		ON RTS.[ParentReportTemplate] = RT.[OID]
	LEFT JOIN dbo.[ReportInstance] RI
		ON RT.[OID] = RI.[ParentReportTemplate]
	LEFT JOIN dbo.[ReportInstanceSignature] RIS
		ON RI.[OID] = RIS.[ParentReportInstance]
			AND RIS.[ParentReportTemplateSignature] = RTS.[OID]
	LEFT JOIN dbo.[Role] R
		ON RTS.[ParentRole] = R.[OID]
WHERE RI.[OID] = ?report_instance_oid
ORDER BY RTS.[IsActive] DESC, 
	RTS.[Order], 
	RIS.[SignatureDateTime] DESC
"
