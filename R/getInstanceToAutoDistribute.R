#' @name getInstanceToAutoDistribute
#' @title Determine if Report Instances are Scheduled for Automatic Distribution
#' 
#' @description Returns a data frame with the instances that are scheduled
#'   for automatic distribution but have not yet been distributed. 
#'   
#' @param distribution_run_time \code{POSIXct(1)} The time at which the 
#'   distribution is being run.
#'   
#' @return
#' Returns a data frame with the columns:
#' 
#' \tabular{ll}{
#'  \code{OID} \tab The OID of the ReportTemplateAutoDistribute object. \cr
#'  \code{ParentReportTemplateOID} \tab The OID of the ReportTemplate object. \cr
#'  \code{TemplateName} \tab The name of the Report Template. \cr
#'  \code{IsAddToArchive} \tab \code{logical} indicating if the report is added to the archive when distributed. \cr
#'  \code{ReportFormat} \tab Either "html" or "pdf". The delivery format for the report. \cr
#'  \code{AutoDistributeDate} \tab The most recent qualifying time for the report's distribution. \cr
#'  \code{ReportInstanceOID} \tab The OID of the ReportInstance object.
#' }
#'   
#' @export

getInstanceToAutoDistribute <- function(distribution_run_time = Sys.time()){
  # Argument validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_posixct(x = distribution_run_time, 
                            len = 1, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  
  # Current User
  User <- queryUser()
  
  event_user <- User$OID[User$LoginId %in% Sys.info()["login"]]
  
  distribution_run_time <- format(distribution_run_time)
  distribution_run_time <- as.POSIXct(distribution_run_time, tz = "UTC")
  
  # Get the report templates that have auto reporting configured and update
  # their instance schedules. This must be done before AutoDistributeConfig, 
  # which has joins/conditions on ReportInstance.StartDate. We won't get 
  # eligible report templates back if we don't update the schedule first.
  Template <- .gITAD_getTemplateForScheduleUpdate()
  
  lapply(Template$ParentReportTemplate,
         updateReportInstanceSchedule, 
         event_user = event_user)

  # Get the potential instances for reporting.
  AutoDistributeConfig <- .gITAD_config()
  
  # Reduce to unique configuration settings
  config_col <- c("OID", 
                  "ParentReportTemplate", 
                  "TemplateName", 
                  "StartDateTime", 
                  "DelayAfterInstanceEnd", 
                  "DelayUnits", 
                  "Frequency", 
                  "FrequencyUnit", 
                  "CurrentOrLastInstance", 
                  "ReportFormat",  
                  "IsAddToArchive", 
                  "IsDistributeInternalOnly", 
                  "IsEmbedHtml")
  
  Config <- AutoDistributeConfig[config_col]
  Config <- Config[!duplicated(Config), ]
  Config <- Config[order(Config$OID), ]
  
  # Separate data for the instances of each reporting template 
  
  InstanceData <- split(AutoDistributeConfig, 
                        AutoDistributeConfig$OID, 
                        drop = TRUE)

  # Determine the distribution schedule for each template
  DistributeSchedule <- mapply(.gITAD_getAutoDistributeSchedule, 
                               delay_after_instance = Config$DelayAfterInstanceEnd, 
                               delay_units = Config$DelayUnits, 
                               frequency = Config$Frequency, 
                               unit = Config$FrequencyUnit, 
                               start_date_time = Config$StartDateTime,
                               data = InstanceData,
                               SIMPLIFY = FALSE)
  
  DistributeSchedule <- 
    lapply(DistributeSchedule, 
           function(DS, distribution_run_time){
             DS <- DS[DS$AutoDistributeDate < distribution_run_time, , drop = FALSE]
             tail(DS, 1) 
           }, 
           distribution_run_time)
  
  DistributeSchedule <- do.call("rbind", 
                                DistributeSchedule)
  
  Config <- 
    merge(Config, 
          DistributeSchedule, 
          by = c("OID"), 
          all.x = TRUE)

  # identify the instance ready to be distributed automatically
  DistributeInstance <- mapply(.gITAD_getInstanceToDistribute, 
                               data = InstanceData[as.character(Config$OID)], 
                               current_or_last = Config$CurrentOrLastInstance, 
                               auto_distribute_date = Config$AutoDistributeDate, 
                               MoreArgs = list(distribution_run_time = distribution_run_time), 
                               SIMPLIFY = FALSE)
  DistributeInstance <- unname(DistributeInstance)

  # Add the instance information to the Configuration Data
  Config <- cbind(Config, do.call("rbind", DistributeInstance))

  # Return the Configuration Data needed to distribute reports
  Config <- Config[!Config$WasAutoDistributed, ]
  Config <- Config[c("OID", 
                     "ParentReportTemplate", 
                     "TemplateName", 
                     "IsAddToArchive",
                     "ReportFormat",
                     "AutoDistributeDate", 
                     "ReportInstanceOID", 
                     "IsDistributeInternalOnly", 
                     "IsEmbedHtml")]
  
  Config
}

# Unexported --------------------------------------------------------

.gITAD_getTemplateForScheduleUpdate <- function(){
  conn <- connectToReportManager()
  
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .gITAD_getTemplateForScheduleUpdate_sqlite_statement, 
                      "sql_server" = .gITAD_getTemplateForScheduleUpdate_sqlServer_statement,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  DBI::dbGetQuery(
    conn, 
    statement
  )
}

.gITAD_getTemplateForScheduleUpdate_sqlServer_statement <- "
SELECT AD.ParentReportTemplate
FROM dbo.AutoDistribution AD
WHERE AD.IsActive = 1
"

.gITAD_getTemplateForScheduleUpdate_sqlite_statement <- "
SELECT AD.ParentReportTemplate
FROM AutoDistribution AD
WHERE AD.IsActive = 1
"
  
.gITAD_config <- function(){
  conn <- connectToReportManager()
  
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .gITAD_config_sqlite_statement, 
                      "sql_server" = .gITAD_config_sqlServer_statement,
                      stop(sprintf("Query not defined for SQL flavor '%s'", 
                                   getOption("RM_sql_flavor"))))
  
  DBI::dbGetQuery(
    conn,
    statement
  )
}

.gITAD_config_sqlServer_statement <- "
SELECT 
  AD.OID, 
  AD.ParentReportTemplate, 
  RT.TemplateName,
  AD.StartDateTime, 
  AD.DelayAfterInstanceEnd, 
  AD.DelayUnits, 
  RTS.ParentSchedule, 
  AD.CurrentOrLastInstance,
  S.Frequency,
  S.FrequencyUnit,
  AD.IsAddToArchive, 
  RI.StartDateTime, 
  RI.EndDateTime, 
  RIAD.AutoDistributeDateTime, 
  RI.OID as ReportInstanceOID, 
  AD.ReportFormat, 
  AD.IsDistributeInternalOnly, 
  AD.IsEmbedHtml
FROM dbo.AutoDistribution AD
  LEFT JOIN dbo.ReportInstance RI
  	ON AD.ParentReportTemplate = RI.ParentReportTemplate
  LEFT JOIN dbo.ReportInstanceAutoDistribution RIAD
    ON RI.OID = RIAD.ParentReportInstance
  LEFT JOIN dbo.ReportTemplateSchedule RTS
  	ON AD.ParentReportTemplate = RTS.OID
  LEFT JOIN dbo.Schedule S
    ON RTS.ParentSchedule = S.OID
  LEFT JOIN dbo.ReportTemplate RT
    ON AD.ParentReportTemplate = RT.OID
WHERE AD.StartDateTime <= RI.EndDateTime
  AND RI.IsScheduled = 1"

.gITAD_config_sqlite_statement <- "
SELECT 
  AD.OID, 
  AD.ParentReportTemplate, 
  RT.TemplateName,
  AD.StartDateTime, 
  AD.DelayAfterInstanceEnd, 
  AD.DelayUnits, 
  RTS.ParentSchedule, 
  AD.CurrentOrLastInstance,
  S.Frequency,
  S.FrequencyUnit,
  AD.IsAddToArchive, 
  RI.StartDateTime, 
  RI.EndDateTime, 
  RIAD.AutoDistributeDateTime, 
  RI.OID as ReportInstanceOID, 
  AD.ReportFormat, 
  AD.IsDistributeInternalOnly, 
  AD.IsEmbedHtml
FROM AutoDistribution AD
  LEFT JOIN ReportInstance RI
  	ON AD.ParentReportTemplate = RI.ParentReportTemplate
  LEFT JOIN ReportInstanceAutoDistribution RIAD
    ON RI.OID = RIAD.ParentReportInstance
  LEFT JOIN ReportTemplateSchedule RTS
  	ON AD.ParentReportTemplate = RTS.OID
  LEFT JOIN Schedule S
    ON RTS.ParentSchedule = S.OID
  LEFT JOIN ReportTemplate RT
    ON AD.ParentReportTemplate = RT.OID
WHERE AD.StartDateTime <= RI.EndDateTime
  AND RI.IsScheduled = 1"


.gITAD_getAutoDistributeSchedule <- function(delay_after_instance, 
                                             delay_units, 
                                             data, 
                                             frequency, 
                                             unit, 
                                             start_date_time){
  auto_distribute_date <- 
      .gITAD_afterInstanceEndSchedule(delay_after_instance, 
                                      delay_units, 
                                      data)

  data.frame(OID = unique(data$OID), 
             AutoDistributeDate = rep(auto_distribute_date, 
                                      length(unique(data$OID))), 
             stringsAsFactors = FALSE)
}

.gITAD_afterInstanceEndSchedule <- function(delay_after_instance, 
                                            delay_units, 
                                            data){
  frequency_type <- tolower(delay_units)
  if (frequency_type == "minute") frequency_type <- "min"
  frequency <- delay_after_instance

  sched <- vapply(data$EndDateTime, 
                  function(ed){
                    seq(from = ed, 
                        by = sprintf("%s %s", frequency, frequency_type), 
                        length.out = 2)[2]
                  }, 
                  numeric(1))
  sched <- as.POSIXct(sched, 
                      origin = lubridate::origin, 
                      tz = "UTC")
  
  sched
}


.gITAD_getInstanceToDistribute <- function(data, 
                                           current_or_last, 
                                           auto_distribute_date, 
                                           distribution_run_time){
  if (current_or_last == "LastCompleted"){
    data <- data[data$EndDateTime < auto_distribute_date, ]
    data <- data[order(data$EndDateTime), ]
    data <- tail(data, 1)
  } else {
    data$IsCurrent <- unlist(mapply(bgcapp::IsInReportPeriod, 
                                    start_report_period = data$StartDateTime, 
                                    end_report_period = data$EndDateTime, 
                                    MoreArgs = list(start_date = distribution_run_time, 
                                                    end_date = distribution_run_time)))
    data <- data[data$IsCurrent, ]
  }
  data$WasAutoDistributed <- !is.na(data$AutoDistributeDateTime)

  data <- data[c("ReportInstanceOID", "AutoDistributeDateTime", "WasAutoDistributed")]
  
  # In case no instance is returned, return a dummy frame to keep the 
  # lists the same length.
  if (nrow(data) == 0){
    data <- data.frame(ReportInstanceOID = NA_real_, 
                       AutoDistributeDateTime = NA_real_,
                       WasAutoDistributed = TRUE)
  }
 
  data
}

#*********************************************************************
#* Changes:
#*
#* DATE        USER     DESCRIPTION
#* 06/12/2024  bnutter  LITR_24_0325 - accomodate YTD format reports
#* 09/07/2023  bnutter  LITR_23_0073 - Configuring Automatic Distribution
#*
#*********************************************************************
