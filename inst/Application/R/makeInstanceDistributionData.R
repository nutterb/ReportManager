makeReportInstanceDistributionData <- function(report_instance_oid){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  InstanceDistribution <- 
    queryInstanceDistributionSelection(report_instance_oid)
  
  if (nrow(InstanceDistribution) > 0){
    InstanceDistribution <- split(InstanceDistribution, 
                                  InstanceDistribution$ParentUser)
    
    InstanceDistribution <- lapply(InstanceDistribution, 
                                   .mRIDD_determineDistribution)
    
    InstanceDistribution <- do.call("rbind", 
                                    InstanceDistribution)  
  } else {
    InstanceDistribution$DistributeByTemplate <- character(0)
    InstanceDistribution$DistributeByInstance <- character(0)
  }

  InstanceDistribution$RefId <- seq_len(nrow(InstanceDistribution))
  
  InstanceDistribution$IncludeInTest <- rep(FALSE, 
                                            nrow(InstanceDistribution))
  
  InstanceDistribution[c("RefId", 
                         "LastName", "FirstName", "EmailAddress", 
                         "IsInternal", "IncludeInTest", "IsActive", 
                         "DistributeByTemplate", 
                         "DistributeByInstance")]
}

.mRIDD_makeDistributionString <- function(indiv = FALSE, 
                                          roles = 0){
  if (!indiv && roles == 0){
    ""
  } else if (!indiv && roles > 0){
    sprintf("Role (%s)", roles)
  } else if (indiv && roles == 0){
    "Indiv."
  } else {
    sprintf("Indiv. / Role (%s)", roles)
  }
}

.mRIDD_determineDistribution <- function(dist){
  dist_base <- dist[1, 
                    c("LastName", "FirstName", "EmailAddress", 
                      "IsInternal")]
  
  dist_base$IsActive <- any(dist$IsActive)
  
  dist <- dist[dist$IsActive, ]
  
  dist_base$DistributeByTemplate <- 
    .mRIDD_makeDistributionString(
      indiv = "Indiv." %in% dist$DistributeBy[!is.na(dist$ParentReportTemplate)], 
      roles = sum(dist$DistributeBy[!is.na(dist$ParentReportTemplate)] %in% "Role")
    )
  
  dist_base$DistributeByInstance <- 
    .mRIDD_makeDistributionString(
      indiv = "Indiv." %in% dist$DistributeBy[!is.na(dist$ReportInstanceDistributionOID)], 
      roles = sum("Role" %in% dist$DistributeBy[!is.na(dist$ReportInstanceDistributionOID)])
    )
  
  dist_base
}
