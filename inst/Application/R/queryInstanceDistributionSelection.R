queryInstanceDistributionSelection <- function(report_instance_oid){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  Selection <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        .queryInstanceDistributionSelection_statement(), 
        report_instance_oid = report_instance_oid
      )
    )
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    Selection$IsActiveInstance <- as.logical(Selection$IsActiveInstance)
    Selection$IsActiveTemplate <- as.logical(Selection$IsActiveTemplate)
    Selection$IsActive <- as.logical(Selection$IsActive)
  }
  
  Selection
}

.queryInstanceDistributionSelection_statement <- function(flavor = getOption("RM_sql_flavor")){
  schema <- if(flavor == "sql_server") "dbo." else ""
  
  paste0("DECLARE @report_instance_oid INT = ?report_instance_oid;

WITH JointSelection
AS
(
	SELECT RID.OID AS ReportInstanceDistributionOID, 
		RTD.OID AS ReportTemplateDistributionOID,
		RTD.ParentReportTemplate, 
		RTD.ParentUser, 
		RTD.ParentRole, 
		RTD.IsActive AS IsActiveTemplate, 
		RID.IsActive AS IsActiveInstance, 
		CASE WHEN RID.IsActive IS NULL THEN RTD.IsActive
			 WHEN RID.IsActive <> RTD.IsActive THEN RID.IsActive
			 ELSE RTD.IsActive END AS IsActive,
		CASE WHEN RTD.ParentUser IS NOT NULL THEN 'Indiv.' ELSE 'Role' END AS DistributeBy
	FROM ", schema, "ReportInstanceDistribution RID
	  LEFT JOIN ", schema, "ReportInstance RI
		ON RID.ParentReportInstance = RI.OID
			AND RI.OID = @report_instance_oid
	  FULL JOIN ", schema, "ReportTemplateDistribution RTD
		ON RI.ParentReportTemplate = RTD.ParentReportTemplate
			AND ((RID.ParentUser IS NULL AND RID.ParentRole = RTD.ParentRole) 
					OR RID.ParentRole IS NULL and RID.ParentUser = RTD.ParentUser)
),

JointWithUser
AS
(
	SELECT JS.ReportInstanceDistributionOID, 
		JS.ReportTemplateDistributionOID, 
		JS.ParentReportTemplate, 
		CASE WHEN JS.ParentUser IS NOT NULL THEN JS.ParentUser
			 ELSE UR.ParentUser END AS ParentUser, 
		JS.ParentRole, 
		JS.IsActiveTemplate, 
		JS.IsActiveInstance, 
		JS.IsActive, 
		JS.DistributeBy
	FROM JointSelection JS
		LEFT JOIN ", schema, "UserRole UR
			ON JS.ParentRole = UR.ParentRole
				AND UR.IsActive = 1
)

SELECT JWU.ReportInstanceDistributionOID, 
	JWU.ReportTemplateDistributionOID, 
	JWU.ParentReportTemplate, 
	JWU.ParentUser, 
	JWU.ParentRole, 
	JWU.IsActiveTemplate, 
	JWU.IsActiveInstance, 
	JWU.IsActive, 
	JWU.DistributeBy,
	U.LastName, 
	U.FirstName, 
	U.EmailAddress, 
	U.IsInternal
FROM JointWithUser JWU
	LEFT JOIN ", schema, "[User] U
		ON JWU.ParentUser = U.OID
WHERE  U.IsActive = 1")
}