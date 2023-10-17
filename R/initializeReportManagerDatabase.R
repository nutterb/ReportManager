#' @name initializeReportManagerDatabase
#' @title Intialize the Report Manager Database
#'
#' @description Retrieves the SQL code to define the Report Manager 
#'   database tables and runs it through the appropriate connection. 
#'   
#' @param filename `character(1)`. A filename of SQL code. Usually 
#'   one found in `system.file("Sql", package = "ReportManager")`
#' @inheritParams addEditUser
#' @param include `character`. A subset of `c("User", "Role", "UserRole")`. 
#'   Designates which objects to pre-populate for the user interface testing.
#'   
#' @details `last_name`, `first_name`, `login_id`, and `email` are 
#'   the values assigned for the first user in the database. This user
#'   will be given the UserAdministrator and ReportAdministrator roles, 
#'   allowing them to add other users and configure reports.
#'   
#' @export

initializeReportManagerDatabase <- function(filename, 
                                            last_name, 
                                            first_name, 
                                            login_id, 
                                            email){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = filename, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assertFileExists(x = filename, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------

  conn <- connectToReportManager()

  on.exit({ DBI::dbDisconnect(conn) })
  
  
  # Determine start and finish of individual statements
    
  sql_code <- readLines(filename)
  
  statement_end <- which(grepl("^[)];", sql_code))
  statement_start <- c(1, utils::head(statement_end + 1, -1))
  

  for (i in seq_along(statement_start)){
    statement <- sql_code[statement_start[i] : statement_end[i]]
    statement <- paste0(statement, collapse = " ")
    result <- DBI::dbSendStatement(conn, 
                                   statement)
    DBI::dbClearResult(result)
  }
  
  # Add the First User ----------------------------------------------
  addEditUser(last_name = last_name, 
              first_name = first_name, 
              login_id = login_id, 
              email = email, 
              is_internal = TRUE, 
              is_active = TRUE, 
              event_user = 1)
  
  # Add the essential roles -----------------------------------------
  
  addEditRole(role_name = "UserAdministrator", 
              role_description = "Users with this role can manage users and user roles.", 
              is_active = TRUE, 
              event_user = 1)
  
  addEditRole(role_name = "ReportAdministrator", 
              role_description = "Users with this role can manage report templates.", 
              is_active = TRUE, 
              event_user = 1)
  
  addEditRole(role_name = "ReportSubmission", 
              role_description = "Users with this role have permission to submit any report.", 
              is_active = TRUE, 
              event_user = 1)
  
  # Assign the First User to the essential roles --------------------
  
  addEditUserRole(parent_user = 1, 
                  parent_role = 1, 
                  event_user = 1)
  
  addEditUserRole(parent_user = 1, 
                  parent_role = 2, 
                  event_user = 1)
  
  addEditUserRole(parent_user = 1, 
                  parent_role = 3, 
                  event_user = 1)
  
  # Populate a Few Common Schedules ---------------------------------
  
  addEditSchedule(schedule_name = "None", 
                  frequency = 0, 
                  frequency_unit = "Day", 
                  offset_overlap = 0, 
                  offset_overlap_unit = "None", 
                  event_user = 1)
  
  addEditSchedule(schedule_name = "Daily", 
                  frequency = 1, 
                  frequency_unit = "Day", 
                  event_user = 1)
  
  addEditSchedule(schedule_name = "Weekly", 
                  frequency = 1, 
                  frequency_unit = "Week", 
                  event_user = 1)
  
  addEditSchedule(schedule_name = "Monthly", 
                  frequency = 1, 
                  frequency_unit = "Month", 
                  event_user = 1)
  
  addEditSchedule(schedule_name = "Quarterly", 
                  frequency = 3, 
                  frequency_unit = "Month", 
                  event_user = 1)
  
  addEditSchedule(schedule_name = "Semi-annually", 
                  frequency = 6, 
                  frequency_unit = "Month", 
                  event_user = 1)
  
  addEditSchedule(schedule_name = "Annually", 
                  frequency = 1, 
                  frequency_unit = "Year", 
                  event_user = 1)
  
  addEditSchedule(schedule_name = "Rolling Three Months", 
                  frequency = 1, 
                  frequency_unit = "Month", 
                  offset_overlap = 3, 
                  offset_overlap_unit = "Month", 
                  event_user = 1)
  
  # Populate a Few Common Date Formats ------------------------------
  
  addEditDateReportingFormat(format_name = "Date", 
                             description = "15 Jul 2000", 
                             format_code = "%d %b %Y", 
                             increment_end = -1, 
                             event_user = 1)
  addEditDateReportingFormat(format_name = "DateTime", 
                             description = "15 Jul 2000 16:30:25", 
                             format_code = "%d %b %Y %H:%M:%S", 
                             increment_end = -1, 
                             event_user = 1)
  addEditDateReportingFormat(format_name = "DateTime H", 
                             description = "15 Jul 2000 16:00", 
                             format_code = "%d %b %Y %H:00", 
                             increment_end = -1, 
                             event_user = 1)
  addEditDateReportingFormat(format_name = "DateTime HM", 
                             description = "15 Jul 2000 16:30", 
                             format_code = "%d %b %Y %H:%M", 
                             increment_end = -1, 
                             event_user = 1)
  
  # Populate Disclaimers and Footers
  
  addEditDisclaimer(title = "Preliminary Data", 
                    disclaimer = "Data contained within this report are both preliminary and unofficial. These data are for internal use only and do not meet the reporting requirements for official correspondence.", 
                    event_user = 1)
  
  addEditFooter(title = "Internal use", 
                footer = "For internal use only.", 
                event_user = 1)
  addEditFooter(title = "Official use", 
                footer = "For official use only.", 
                event_user = 1)
  addEditFooter(title = "Not a record", 
                footer = "Not a record.", 
                event_user = 1)
  addEditFooter(title = "Becomes a record", 
                footer = "Becomes a record upon completion.", 
                event_user = 1)
}

#' @rdname initializeReportManagerDatabase
#' @export
# Database For Testing the UI ---------------------------------------

initializeUiTestingDatabase <- function(filename, 
                                        include = c("User", 
                                                    "Role", 
                                                    "UserRole", 
                                                    "ReportTemplate", 
                                                    "ReportTemplateDisclaimer", 
                                                    "ReportTemplateFooter", 
                                                    "ReportTemplateSchedule", 
                                                    "ReportTemplateSignature", 
                                                    "ReportTemplateDistribution"),
                                        last_name = "Testing-LastName", 
                                        first_name = "Testing-FirstName", 
                                        login_id = Sys.info()["user"], 
                                        email = "email@somewhere.net"){
  
  initializeReportManagerDatabase(filename, 
                                  last_name = last_name, 
                                  first_name = first_name, 
                                  login_id = login_id, 
                                  email = email)
  
  if ("User" %in% include){
    mapply(addEditUser, 
           last_name = TestUser$last_name, 
           first_name = TestUser$first_name, 
           login_id = TestUser$login_id, 
           email = TestUser$email, 
           is_internal = TestUser$is_internal, 
           is_active = TestUser$is_active, 
           MoreArgs = list(event_user = 1))
  }
  
  if ("Role" %in% include){
    mapply(addEditRole, 
           role_name = TestRole$role_name, 
           role_description = TestRole$role_description, 
           is_active = TestRole$is_active, 
           MoreArgs = list(event_user = 1))
  }
  
  if ("UserRole" %in% include){
    mapply(addEditUserRole, 
           parent_user = TestUserRole$parent_user, 
           parent_role = TestUserRole$parent_role, 
           is_active = TestUserRole$is_active, 
           MoreArgs = list(event_user = 1))
  }
  
  if ("ReportTemplate" %in% include){
    mapply(addEditReportTemplate, 
           title = TestReportTemplate$Title, 
           template_directory = TestReportTemplate$TemplateDirectory, 
           template_file = TestReportTemplate$TemplateFile, 
           title_size = TestReportTemplate$TitleSize, 
           is_signature_required = TestReportTemplate$IsSignatureRequired, 
           is_active = TestReportTemplate$IsActive, 
           logo_oid = TestReportTemplate$LogFileArchive,
           MoreArgs = list(event_user = 1))
    
    if ("ReportTemplateSchedule" %in% include){
      mapply(addEditReportTemplateSchedule, 
             parent_report_template = TestReportTemplateSchedule$ParentReportTemplate,
             parent_schedule = TestReportTemplateSchedule$ParentSchedule,
             start_date = TestReportTemplateSchedule$StartDateTime,
             MoreArgs = list(event_user = 1))
    }
    
    if ("ReportTemplateDisclaimer" %in% include){
      mapply(addEditReportTemplateDisclaimer, 
             parent_report_template = TestReportTemplateDisclaimer$ParentReportTemplate,
             parent_disclaimer = TestReportTemplateDisclaimer$ParentDisclaimer,
             order = TestReportTemplateDisclaimer$Order,
             MoreArgs = list(event_user = 1))
    }
    
    if ("ReportTemplateFooter" %in% include){
      mapply(addEditReportTemplateFooter, 
             parent_report_template = TestReportTemplateFooter$ParentReportTemplate,
             parent_footer = TestReportTemplateFooter$ParentFooter,
             order = TestReportTemplateFooter$Order,
             MoreArgs = list(event_user = 1))
    }
    
    if ("ReportTemplateSignature" %in% include){
      mapply(addEditReportTemplateSignature, 
             parent_report_template = TestReportTemplateSignature$ParentReportTemplate,
             parent_signature = TestReportTemplateSignature$ParentSignature,
             order = TestReportTemplateSignature$Order,
             MoreArgs = list(event_user = 1))
    }
  }
}



# Unexported - Purge Database ---------------------------------------
# For assistance with clearing the database to set up testing

purgeReportManagerDatabase <- function(sql_flavor = getOption("RM_sql_flavor")){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  if (sql_flavor == "sql_server"){
    res <- DBI::dbSendStatement(conn, .purgeConstraintQuery)
    DBI::dbClearResult(res)
  }
  
  dropTable <- function(table, conn){
    is_sql_server <- sql_flavor == "sql_server"
    
    tables <- DBI::dbListTables(conn, 
                                schema = if (is_sql_server) "dbo" else NULL)
    
    if (table %in% tables){
      result <- DBI::dbSendStatement(conn, 
                                     sprintf("DROP TABLE %s[%s]", 
                                             if (is_sql_server) "dbo." else "",
                                             table))
      DBI::dbClearResult(result)
    }
  }
  
  Tables <- DBI::dbListTables(conn, schema = "dbo")
  lapply(Tables, 
         dropTable, 
         conn)
}

.purgeConstraintQuery <- "
  DECLARE @sql NVARCHAR(MAX);
  SET @sql = N'';
  
  SELECT @sql = @sql + N'
  ALTER TABLE ' + QUOTENAME(s.name) + N'.'
  + QUOTENAME(t.name) + N' DROP CONSTRAINT '
  + QUOTENAME(c.name) + ';'
  FROM sys.objects AS c
  INNER JOIN sys.tables AS t
  ON c.parent_object_id = t.[object_id]
  INNER JOIN sys.schemas AS s 
  ON t.[schema_id] = s.[schema_id]
  WHERE c.[type] IN ('D','C','F','PK','UQ')
  ORDER BY c.[type];
  
  EXEC sys.sp_executesql @sql;
"

# Unexported - Test Data --------------------------------------------

# Users -------------------------------------------------------------

TestUser <- 
  data.frame(last_name = c("Manager", 
                           "Manager", 
                           "Manager", 
                           "Reviewer", 
                           "Reviewer", 
                           "Support", 
                           "Oversight", 
                           "Employee"), 
             first_name = c("General", 
                            "Production", 
                            "Quality",
                            "Production", 
                            "Quality", 
                            "Admin", 
                            "Government", 
                            "Retired"), 
             login_id = c("genmanager", 
                          "productionmanager", 
                          "qualitymanager",
                          "productionreviewer", 
                          "qualityreviewer", 
                          "adminsupport", 
                          "oversight", 
                          "retiredemployee"), 
             is_internal = c(rep(TRUE, 6), FALSE, TRUE), 
             is_active = c(rep(TRUE, 7), FALSE), 
             stringsAsFactors = FALSE)
TestUser$email <- sprintf("%s@business.com", 
                          TestUser$login_id)
TestUser$email[7] <- "oversight@government.gov"

# Roles -------------------------------------------------------------

TestRole <- 
  data.frame(
    role_name = c("Management", 
                  "Production Report Pepare", 
                  "Production Shift Managers",
                  "Quality Report Review", 
                  "Administrative Support", 
                  "Government Oversight", 
                  "Obsolete Role"), 
    role_description = c("General Managers and Department Level Managers", 
                         "Individuals Authorized to Prepare Report for the External Client", 
                         "Production Shift Managers and Team Leads", 
                         "Authorized to review reports prior to client submission.",
                         "Administrative support personnel",
                         "Government oversight group.", 
                         "Inventory management specialists"), 
    is_active = c(rep(TRUE, 6), FALSE),
    stringsAsFactors = FALSE)

# UserRole Assignment -----------------------------------------------

TestUserRole <- 
  data.frame(
    parent_user = c(2, 3, 3, 4, 4, 4,  5, 6, 7, 8, 9), 
    parent_role = c(4, 4, 5, 5, 6, 10, 6, 7, 8, 9, 10), 
    is_active = rep(TRUE, 11)
  )

# Template Definition -----------------------------------------------

TestReportTemplate <- 
  data.frame(Title = c("First Report Template", "Second Report Template"),
             TitleSize = c("Large", "Huge"), 
             TemplateDirectory = c("SampleReport", "SampleReport"),
             TemplateFile = c("00-Template.Rmd", "00-Template.Rmd"),
             IsSignatureRequired = c(FALSE, TRUE), 
             IsActive = c(TRUE, TRUE), 
             LogoFileArchive = rep(NA, 2), 
             stringsAsFactors = FALSE)

# ReportTemplateDisclaimer Definition -------------------------------

TestReportTemplateDisclaimer <- 
  data.frame(ParentReportTemplate = 1, 
             ParentDisclaimer = 1, 
             Order = 1, 
             stringsAsFactors = FALSE)

# ReportTemplateFooter Definition -----------------------------------

TestReportTemplateFooter <- 
  data.frame(ParentReportTemplate = c(1, 2), 
             ParentFooter = c(2, 3), 
             Order = c(1, 1), 
             stringsAsFactors = FALSE)

# ReportTemplateSchedule Definition ---------------------------------

TestReportTemplateSchedule <- 
  data.frame(ParentReportTemplate = c(1, 2), 
             ParentSchedule = c(1, 4), 
             StartDateTime = rep(as.POSIXct("2023-01-01", tz = "UTC"), 2), 
             stringsAsFactors = FALSE)

# ReportTemplateSignature Definition --------------------------------

TestReportTemplateSignature <- 
  data.frame(ParentReportTemplate = c(1, 1, 1, 2, 2), 
             ParentRole = c(5, 4, 2, 3, 1), 
             Order = c(3, 2, 1, 1, 2), 
             stringsAsFactors = FALSE)
