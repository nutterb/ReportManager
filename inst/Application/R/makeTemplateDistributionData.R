makeTemplateDistributionData <- function(report_template_oid){
  conn <- connectToReportManager()
  on.exit(DBI::dbDisconnect(conn))

  DBI::dbGetQuery(
    conn, 
    DBI::sqlInterpolate(
      conn, 
      .makeTemplateDistributionData_statement(), 
      report_template_oid = report_template_oid
    )
  )
}

.makeTemplateDistributionData_statement <- function(flavor = getOption("RM_sql_flavor")){
  schema <- if (flavor == "sql_server") "dbo." else ""

  paste0("WITH DistUser
AS
(
	SELECT RTD.ParentUser 
	FROM ", schema, "ReportTemplateDistribution RTD
		LEFT JOIN ", schema, "[User] U
			ON RTD.ParentUser = U.OID
	WHERE RTD.ParentReportTemplate = ?report_template_oid
		AND RTD.ParentUser IS NOT NULL
		AND RTD.IsActive = 1 
), 

DistRole
AS
(
	SELECT UR.ParentUser
	FROM ", schema, "ReportTemplateDistribution RTD
		LEFT JOIN ", schema, "UserRole UR
			ON RTD.ParentRole = UR.ParentRole
	WHERE RTD.ParentReportTemplate = ?report_template_oid
		AND RTD.ParentRole IS NOT NULL
		AND RTD.IsActive = 1
), 

UserListStack
AS
(
	SELECT * FROM DistRole 
	UNION ALL
	SELECT * FROM DistUser
),

UserList
AS
(
	SELECT DISTINCT ParentUser FROM UserListStack
),

RoleCount
AS
(
	SELECT ParentUser, 
		'Role (' + CAST(COUNT(ParentUser) AS VARCHAR(8)) + ')' AS NRole
	FROM DistRole
	GROUP BY ParentUser
)

SELECT UL.ParentUser, 
	U.LastName, 
	U.FirstName, 
	U.EmailAddress,
	CASE WHEN DU.ParentUser IS NULL AND RC.ParentUser IS NULL THEN ''
	     WHEN DU.ParentUser IS NOT NULL AND RC.ParentUser IS NULL THEN 'Indiv.'
		 WHEN DU.ParentUser IS NULL AND RC.ParentUser IS NOT NULL THEN RC.NRole 
		 WHEN DU.ParentUser IS NOT NULL AND RC.ParentUser IS NOT NULL THEN 'Indiv. / ' + RC.Nrole 
		 ELSE '' END AS DistributeBy
FROM UserList UL
	LEFT JOIN DistUser DU
		ON UL.ParentUser = DU.ParentUser
	LEFT JOIN RoleCount RC
		ON UL.ParentUser = RC.ParentUser
	LEFT JOIN ", schema, "[User] U
		ON UL.ParentUser = U.OID")
}
