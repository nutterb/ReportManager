#' @name initializeReportManagerDatabase
#' @title Intialize the Report Manager Database
#'
#' @description Retrieves the SQL code to define the Report Manager 
#'   database tables and runs it through the appropriate connection. 
#'   
#' @param filename `character(1)`. A filename of SQL code. Usually 
#'   one found in `system.file("Sql", package = "ReportManager")`
#' @inheritParams addEditUser
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
  
  addEditSchedule(schedule_name = "None", 
                  frequency = 0, 
                  frequency_unit = "Day", 
                  offset_overlap = 0, 
                  offset_overlap_unit = "None", 
                  event_user = 1)
  
  # Populate a Few Common Date Formats ------------------------------
  
  addEditDateReportingFormat(format_name = "Date", 
                             description = "Date only (15 Jan 2000)", 
                             format_code = "%d %b %Y", 
                             increment_end = -1, 
                             event_user = 1)
  addEditDateReportingFormat(format_name = "DateTime", 
                             description = "Date and time (15 Jan 2000 16:30:25)", 
                             format_code = "%d %b %Y %H:%M:%S", 
                             increment_end = -1, 
                             event_user = 1)
  addEditDateReportingFormat(format_name = "DateTime H", 
                             description = "Date and time, hour only (15 Jan 2000 16:00)", 
                             format_code = "%d %b %Y %H:00", 
                             increment_end = -1, 
                             event_user = 1)
  addEditDateReportingFormat(format_name = "DateTime HM", 
                             description = "Date and time, hour and minute (15 Jan 2000 16:30)", 
                             format_code = "%d %b %Y %H:%M", 
                             increment_end = -1, 
                             event_user = 1)
  
}
