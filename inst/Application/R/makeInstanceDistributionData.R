makeReportInstanceDistributionData <- function(report_instance_oid){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  InstanceDistribution <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        .makeReportInstanceDistributionData_statement(), 
        report_instance_oid = report_instance_oid
      )
    )
  
  InstanceDistribution$IsActive <- 
    ifelse(is.na(InstanceDistribution$IsActiveInstance),
           InstanceDistribution$IsActiveTemplate, 
           InstanceDistribution$IsActiveInstance)
  
  InstanceDistribution$RefId <- seq_len(nrow(InstanceDistribution))
  
  InstanceDistribution$IncludeInTest <- rep(FALSE, 
                                            nrow(InstanceDistribution))
  
  
  InstanceDistribution
}

.makeReportInstanceDistributionData_statement <- function(flavor = getOption("RM_sql_flavor")){
  schema <- if(flavor == "sql_server") "dbo." else ""
  
  paste0("DECLARE @report_instance_oid INT = ?report_instance_oid;

DECLARE @report_template_oid INT = (SELECT ParentReportTemplate FROM dbo.ReportInstance WHERE OID = 1);

WITH InstanceDistributeRole
AS
(
	SELECT RI.ParentReportTemplate, 
		RID.ParentReportInstance, 
		RID.OID AS DistributionOID,
		RID.ParentRole, 
		RID.IsActive AS IsActiveInstance, 
		NULL AS IsActiveTemplate,
		'Instance' AS DistributeBy
	FROM ", schema, "ReportInstanceDistribution RID
		LEFT JOIN ", schema, "ReportInstance RI
			ON RID.ParentReportInstance = RI.OID
	WHERE RID.ParentReportInstance = @report_instance_oid
		AND RID.ParentRole IS NOT NULL
), 

TemplateDistributeRole
AS
(
	SELECT RTD.ParentReportTemplate AS ParentReportTemplate, 
		NULL AS ParentReportInstance, 
		RTD.OID AS DistributionOID,
		RTD.ParentRole, 
		NULL AS IsActiveInstance,
		RTD.IsActive AS IsActiveTemplate, 
		'Template' AS DistributeBy
	FROM ", schema, "ReportTemplateDistribution RTD
	WHERE RTD.ParentReportTemplate = @report_template_oid
		AND RTD.ParentRole IS NOT NULL
),

DistributionRole
AS
(
	SELECT * FROM InstanceDistributeRole
	UNION ALL
	SELECT * FROM TemplateDistributeRole
), 

RecipientRole
AS
(
	SELECT U.LastName, 
		U.FirstName, 
		U.EmailAddress, 
		U.IsInternal, 
		DR.IsActiveInstance,
		DR.IsActiveTemplate, 
		DR.DistributionOID, 
		'Role' AS DistributeBy, 
		DR.ParentReportInstance, 
		DR.ParentReportTemplate, 
		U.OID AS UserOID
	FROM DistributionRole DR
		LEFT JOIN ", schema, "UserRole UR
			ON DR.ParentRole = UR.ParentRole
		LEFT JOIN ", schema, "[User] U
			ON UR.ParentUser = U.OID
	WHERE U.IsActive = 1
), 




InstanceDistributeIndiv
AS
(
	SELECT RI.ParentReportTemplate, 
		RID.ParentReportInstance, 
		RID.OID AS DistributionOID,
		RID.ParentUser, 
		RID.IsActive AS IsActiveInstance, 
		NULL AS IsActiveTemplate,
		'Instance' AS DistributeBy
	FROM ", schema, "ReportInstanceDistribution RID
		LEFT JOIN ", schema, "ReportInstance RI
			ON RID.ParentReportInstance = RI.OID
	WHERE RID.ParentReportInstance = @report_instance_oid
		AND RID.ParentUser IS NOT NULL
), 

TemplateDistributeIndiv
AS
(
	SELECT RTD.ParentReportTemplate AS ParentReportTemplate, 
		NULL AS ParentReportInstance, 
		RTD.OID AS DistributionOID,
		RTD.ParentUser, 
		NULL AS IsActiveInstance,
		RTD.IsActive AS IsActiveTemplate, 
		'Template' AS DistributeBy
	FROM ", schema, "ReportTemplateDistribution RTD
	WHERE RTD.ParentReportTemplate = @report_template_oid
		AND RTD.ParentUser IS NOT NULL
),

DistributionIndiv
AS
(
	SELECT * FROM InstanceDistributeIndiv
	UNION ALL
	SELECT * FROM TemplateDistributeIndiv
),

RecipientUser
AS
(
	SELECT U.LastName, 
		U.FirstName, 
		U.EmailAddress, 
		U.IsInternal,  
		DI.IsActiveInstance,
		DI.IsActiveTemplate,
		DI.DistributionOID, 
		'Indiv.' AS DistributeBy, 
		DI.ParentReportInstance, 
		DI.ParentReportTemplate, 
		U.OID AS UserOID
	FROM DistributionIndiv DI
		LEFT JOIN ", schema, "[User] U
			ON DI.ParentUser = U.OID
	WHERE U.IsActive = 1
)

SELECT * FROM RecipientUser
UNION ALL
SELECT * FROM RecipientRole")
}