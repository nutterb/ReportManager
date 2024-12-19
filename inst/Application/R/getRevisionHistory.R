getRevisionHistory <- function(report_instance_oid){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sqlite" = .getRevisionHistory_sqlite, 
           "sql_server" = .getRevisionHistory_sqlServer, 
           stop(sprintf("Query not defined for SQL flavor '%s'", 
                        getOption("RM_sql_flavor"))))
  
  Revision <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        report_instance_oid = report_instance_oid
      )
    )

  Revision$IsSubmission <- as.logical(Revision$IsSubmission)
  
  Revision$Revision <- cumsum(Revision$IsSubmission)
  Revision$Revision[!Revision$IsSubmission] <- NA_real_
  Revision$EventType <- ifelse(Revision$IsSubmission, 
                               "Submission", 
                               "Revision")
  Revision <- Revision[c("EventType", "User", "EventDateTime", "Revision")]
  
  Revision <- Revision[order(Revision$EventDateTime, 
                             decreasing = TRUE), ]
  Revision
}

.getRevisionHistory_sqlServer <- "
DECLARE @report_instance_oid INT = ?report_instance_oid;

WITH Submission 
AS
(
	SELECT U.LastName + ', ' + U.FirstName AS [User],
		RIG.PreviewDateTime AS EventDateTime, 
		1 AS IsSubmission
	FROM dbo.ReportInstance RI
		LEFT JOIN dbo.ReportInstanceGeneration RIG
			ON RI.OID = RIG.ParentReportInstance
				AND RIG.IsSubmission = 1
		LEFT JOIN dbo.[User] U
			ON RIG.ParentUser = U.OID
	WHERE RI.OID = @report_instance_oid
), 
Revision 
AS
(
	SELECT U.LastName + ', ' + U.FirstName AS [User], 
		RIR.EventDateTime, 
		0 AS IsSubmission
	FROM dbo.ReportInstance RI
		LEFT JOIN dbo.ReportInstanceRevision RIR
			ON RI.OID = RIR.ParentReportInstance
		LEFT JOIN dbo.[User] U
			ON RIR.ParentUser = U.OID
	WHERE RI.OID = @report_instance_oid
)

(SELECT * FROM Submission
UNION ALL
SELECT * FROM Revision)
ORDER BY EventDateTime
"
	 
.getRevisionHistory_sqlite <- "
DECLARE @report_instance_oid INT = ?report_instance_oid;

WITH Submission 
AS
(
	SELECT U.LastName + ', ' + U.FirstName AS [User],
		RIG.PreviewDateTime AS EventDateTime, 
		1 AS IsSubmission
	FROM ReportInstance RI
		LEFT JOIN ReportInstanceGeneration RIG
			ON RI.OID = RIG.ParentReportInstance
				AND RIG.IsSubmission = 1
		LEFT JOIN [User] U
			ON RIG.ParentUser = U.OID
	WHERE RI.OID = @report_instance_oid
), 
Revision 
AS
(
	SELECT U.LastName + ', ' + U.FirstName AS [User], 
		RIR.EventDateTime, 
		0 AS IsSubmission
	FROM ReportInstance RI
		LEFT JOIN ReportInstanceRevision RIR
			ON RI.OID = RIR.ParentReportInstance
		LEFT JOIN [User] U
			ON RIR.ParentUser = U.OID
	WHERE RI.OID = @report_instance_oid
)

(SELECT * FROM Submission
UNION ALL
SELECT * FROM Revision)
ORDER BY EventDateTime
"