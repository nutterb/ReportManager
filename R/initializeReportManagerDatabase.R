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
                                        include = c("User", "Role", "UserRole"),
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
           event_user = 1)
  }
}

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
