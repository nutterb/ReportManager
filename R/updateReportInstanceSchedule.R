#' @name updateReportInstanceSchedule
#' @title Update the Report Instance Schedule of a Report Template
#' 
#' @description Retrieves the schedule information of a report template, 
#'   determines the report schedule, and adds any unrecorded instances 
#'   to the database.
#'   
#' @param report_template_oid `integerish(1)`. The OID of the report template
#'   for which the schedule should be updated.
#' @param event_user `integerish(1)`. the User.OID of the user performing 
#'   the action. 
#'   
#' @export

updateReportInstanceSchedule <- function(report_template_oid, 
                                         event_user){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = report_template_oid, 
                               len = 1, 
                               add = coll)
  
  checkmate::assert_integerish(x = event_user, 
                               len = 1, 
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  ReportTemplate <- queryReportTemplate(oid = report_template_oid)
  
  ScheduleInfo <- 
    .updateReportInstanceSchedule_ScheduleInfo(report_template_oid, 
                                               conn)
  
  ReportInstanceSchedule <- 
    makeReportInstanceSchedule(frequency = ScheduleInfo$Frequency, 
                               frequency_unit = ScheduleInfo$FrequencyUnit, 
                               offset = ScheduleInfo$OffsetOverlap, 
                               offset_unit = ScheduleInfo$OffsetOverlapUnit, 
                               schedule_type = ScheduleInfo$ScheduleName, 
                               start_date = ScheduleInfo$StartDateTime, 
                               end_date = Sys.time(), 
                               is_offset = !ScheduleInfo$IsPeriodToDate, 
                               index_date = ScheduleInfo$IndexDateTime)
  
  CurrentInstance <- 
    queryReportInstance(report_template_oid = report_template_oid)
  
  MissingInstance <- 
    merge(CurrentInstance[c("StartDateTime", "EndDateTime", "OID")], 
          ReportInstanceSchedule, 
          by.x = c("StartDateTime", "EndDateTime"), 
          by.y = c("StartDate", "EndDate"), 
          all.y = TRUE)
  MissingInstance <- MissingInstance[is.na(MissingInstance$OID), ]
  
  invisible({
    mapply(
      FUN = addEditReportInstance, 
      start_time = MissingInstance$StartDateTime, 
      end_time = MissingInstance$EndDateTime, 
      MoreArgs = list(parent_report_template = report_template_oid, 
                      is_signature_required = ReportTemplate$IsSignatureRequired, 
                      is_scheduled = TRUE, 
                      instance_title = "", 
                      is_submitted = FALSE, 
                      event_user = event_user))
  })
}

# Unexported --------------------------------------------------------

.updateReportInstanceSchedule_ScheduleInfo <- function(report_template_oid, 
                                                       conn){
  Schedule <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        switch(getOption("RM_sql_flavor"), 
               "sql_server" = .updateReportInstanceSchedule_ScheduleInfoStatement_sql_server, 
               .updateReportInstanceSchedule_ScheduleInfoStatement_sqlite),
        report_template_oid = report_template_oid
      )
    )
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    Schedule$IsPeriodToDate <- as.logical(Schedule$IsPeriodToDate)
    
    if (!inherits(Schedule$StartDateTime, "POSIXct")){
      Schedule$StartDateTime <- as.POSIXct(Schedule$StartDateTime, 
                                           tz = "UTC")
    }
    
    if (!inherits(Schedule$IndexDateTime, "POSIXct")){
      Schedule$IndexDateTime <- as.POSIXct(Schedule$IndexDateTime, 
                                           tz = "UTC")
    }
  }
  
  Schedule
}


.updateReportInstanceSchedule_ScheduleInfoStatement_sqlite <- "
SELECT RT.OID AS ReportTemplateOID, 
	RT.Title, 
	RTS.StartDateTime, 
	RTS.IndexDateTime, 
	S.Frequency, 
	S.FrequencyUnit, 
	S.IsPeriodToDate, 
	S.OffsetOverlap, 
	S.OffsetOverlapUnit, 
	S.ScheduleName
FROM ReportTemplate RT
	LEFT JOIN ReportTemplateSchedule RTS
		ON RT.OID = RTS.ParentReportTemplate
	LEFT JOIN Schedule S
		ON RTS.ParentSchedule = S.OID
WHERE RT.OID = ?report_template_oid
"

.updateReportInstanceSchedule_ScheduleInfoStatement_sql_server <- "
SELECT RT.OID AS ReportTemplateOID, 
	RT.Title, 
	RTS.StartDateTime, 
	RTS.IndexDateTime, 
	S.Frequency, 
	S.FrequencyUnit, 
	S.IsPeriodToDate, 
	S.OffsetOverlap, 
	S.OffsetOverlapUnit, 
	S.ScheduleName
FROM dbo.ReportTemplate RT
	LEFT JOIN dbo.ReportTemplateSchedule RTS
		ON RT.OID = RTS.ParentReportTemplate
	LEFT JOIN dbo.Schedule S
		ON RTS.ParentSchedule = S.OID
WHERE RT.OID = ?report_template_oid
"