#' @name queryInstanceDistributionSelection
#' @title Retrieve the Distribution List for a Report Instance
#' 
#' @description While the distribution may be set at a template level, 
#'   each instance of the report template may modify that distribution list. 
#'   This function executes a query to combine the template-level and 
#'   instance-level distribution list.
#'   
#' @param report_instance_oid `integerish(1).` The OID of the report instance.
#' 
#' @export

queryInstanceDistributionSelection <- function(report_instance_oid){
  
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
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
		CASE WHEN RTD.ParentUser IS NULL THEN RID.ParentUser ELSE RTD.ParentUser END ParentUser, 
		CASE WHEN RTD.ParentRole IS NULL THEN RID.ParentRole ELSE RTD.ParentRole END ParentRole,  
		RTD.IsActive AS IsActiveTemplate, 
		RID.IsActive AS IsActiveInstance, 
		CONVERT(BIT, CASE WHEN RTD.IsActive IS NULL AND RID.IsActive IS NULL THEN 0 
		     WHEN RTD.IsActive IS NOT NULL AND RID.IsActive IS NULL THEN RTD.IsActive
			 WHEN RTD.IsActive IS NULL AND RID.IsActive IS NOT NULL THEN RID.IsActive
			 WHEN RID.IsActive <> RTD.IsActive THEN RID.IsActive
			 ELSE RTD.IsActive END) AS IsActive,
		CASE WHEN RTD.ParentRole IS NULL AND RID.ParentRole IS NULL THEN 'Indiv.' ELSE 'Role' END AS DistributeBy
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