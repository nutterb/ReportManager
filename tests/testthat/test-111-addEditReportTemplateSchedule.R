# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(addEditReportTemplateSchedule(oid = "1", 
                                               parent_report_template = 1, 
                                               parent_schedule = 1, 
                                               start_date = Sys.time(),
                                               event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSchedule(oid = 1:2, 
                                               parent_report_template = 1, 
                                               parent_schedule = 1,  
                                               start_date = Sys.time(),
                                               event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(1)", 
  {
    expect_error(addEditReportTemplateSchedule(parent_report_template = "1", 
                                                 parent_schedule = 1,  
                                                 start_date = Sys.time(),
                                                 event_user = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1:2, 
                                                 parent_schedule = 1, 
                                                 start_date = Sys.time(), 
                                                 event_user = 1), 
                 "'parent_report_template': Must have length 1")
  }
)

test_that(
  "Return an error when parent_schedule is not integerish(1)", 
  {
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                                 parent_schedule = "1", 
                                                 start_date = Sys.time(), 
                                                 event_user = 1), 
                 "'parent_schedule': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                                 parent_schedule = 1:2, 
                                                 start_date = Sys.time(), 
                                                 event_user = 1), 
                 "'parent_schedule': Must have length 1")
  }
)

test_that(
  "Return an error when start_date is not POSIXct(1)", 
  {
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                                 parent_schedule = 1, 
                                                 start_date = "1", 
                                                 event_user = 1), 
                 "'start_date': Must be of type 'POSIXct'")
    
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                                 parent_schedule = 1, 
                                                 start_date = rep(Sys.time(), 2), 
                                                 event_user = 1), 
                 "'start_date': Must have length 1")
  }
)

test_that(
  "Return an error when is_active is not logical(1)", 
  {
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                                 parent_schedule = 1, 
                                                 start_date = Sys.time(), 
                                                 is_active = "TRUE", 
                                                 event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                                 parent_schedule = 1,  
                                                 start_date = Sys.time(),
                                                 is_active = c(TRUE, FALSE), 
                                                 event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error when event_user is not integerish(1)", 
  {
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                                 parent_schedule = 1, 
                                                 start_date = Sys.time(), 
                                                 event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                                 parent_schedule = 1, 
                                                 start_date = Sys.time(), 
                                                 event_user = 1:2), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

if (SQL_SERVER_READY){
  configureReportManager(flavor = "sql_server")
  purgeReportManagerDatabase()
  initializeUiTestingDatabase(system.file("Sql/SqlServer.sql", 
                                          package = "ReportManager"), 
                              include = c("User", "Role", "UserRole", 
                                          "ReportTemplate"))
}


test_that(
  "Record can be added", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    CurrentObjects <- queryReportTemplateSchedule()
    
    next_oid <- nrow(CurrentObjects) + 1
    
    addEditReportTemplateSchedule(parent_report_template = 1, 
                                  parent_schedule = 1,  
                                  start_date = Sys.time(),
                                  event_user = 1)
    
    New <- queryReportTemplateSchedule(oid = next_oid)
    
    expect_data_frame(New, 
                      nrows = 1)
    expect_equal(New$ParentReportTemplate, 
                 1)
    expect_equal(New$ParentSchedule, 
                 1)
  }
)

test_that(
  "Return an error if attempting to add a record for an existing template-disclaimer", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                               parent_schedule = 1,  
                                               start_date = Sys.time(),
                                               event_user = 1), 
                 "A ReportTemplateSchedule record for ReportTemplate.OID")
  }
)

test_that(
  "Edit an existing record", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    addEditReportTemplateSchedule(oid = 1, 
                                  parent_report_template = 1, 
                                  parent_schedule = 2, 
                                  start_date = Sys.time(),
                                  is_active = FALSE,
                                  event_user = 1)
    
    New <- queryReportTemplateSchedule(oid = 1)
    
    expect_data_frame(New, 
                      nrows = 1)
    expect_equal(New$ParentReportTemplate, 
                 1)
    expect_equal(New$ParentSchedule, 
                 2)
    expect_false(New$IsActive)
  }
)

test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_oid <- max(queryReportTemplateSchedule()$OID)
    next_oid <- last_oid + 1
    
    addEditReportTemplateSchedule(parent_report_template = 2,
                                  parent_schedule = 3,
                                  start_date = Sys.time(),
                                  is_active = TRUE,
                                  event_user = 1)
    
    TemplateEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  "SELECT * FROM ReportTemplateScheduleEvent WHERE ParentReportTemplateSchedule = ?", 
                                  next_oid))
    
    expect_equal(TemplateEvent$EventType,
                 c("Add", "EditStartDate", "Activate"))
    expect_true(all(table(TemplateEvent$EventType) == 1))
    
    addEditReportTemplateSchedule(oid = next_oid, 
                                  parent_report_template = 2, 
                                  parent_schedule = 3,
                                  start_date = Sys.time() + 365, 
                                  is_active = FALSE,
                                  event_user = 1)
    
    
    TemplateEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   "SELECT * FROM ReportTemplateScheduleEvent WHERE ParentReportTemplateSchedule = ?", 
                                   next_oid))

    expect_true(
      all(table(TemplateEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1,
              "EditStartDate" = 2))
    )
    
    dbDisconnect(conn)
  }
)


# Functionality - SQLite --------------------------------------------

if (SQLITE_READY){
  configureReportManager(flavor = "sqlite")
  purgeReportManagerDatabase()
  initializeUiTestingDatabase(system.file("Sql/Sqlite.sql", 
                                          package = "ReportManager"), 
                              include = c("User", "Role", "UserRole", 
                                          "ReportTemplate"))
}


test_that(
  "Record can be added", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    CurrentObjects <- queryReportTemplateSchedule()
    
    next_oid <- nrow(CurrentObjects) + 1
    
    addEditReportTemplateSchedule(parent_report_template = 1, 
                                    parent_schedule = 1,  
                                    start_date = Sys.time(),
                                    event_user = 1)
    
    New <- queryReportTemplateSchedule(oid = next_oid)
    
    expect_data_frame(New, 
                      nrows = 1)
    expect_equal(New$ParentReportTemplate, 
                 1)
    expect_equal(New$ParentSchedule, 
                 1)
  }
)

test_that(
  "Return an error if attempting to add a record for an existing template-disclaimer", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    expect_error(addEditReportTemplateSchedule(parent_report_template = 1, 
                                               parent_schedule = 1,  
                                               start_date = Sys.time(),
                                               event_user = 1), 
                 "A ReportTemplateSchedule record for ReportTemplate.OID")
  }
)

test_that(
  "Edit an existing record", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    addEditReportTemplateSchedule(oid = 1, 
                                  parent_report_template = 1, 
                                  parent_schedule = 2, 
                                  start_date = Sys.time(),
                                  is_active = FALSE,
                                  event_user = 1)
    
    New <- queryReportTemplateSchedule(oid = 1)
    
    expect_data_frame(New, 
                      nrows = 1)
    expect_equal(New$ParentReportTemplate, 
                 1)
    expect_equal(New$ParentSchedule, 
                 2)
    expect_false(New$IsActive)
  }
)

test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_oid <- max(queryReportTemplateSchedule()$OID)
    next_oid <- last_oid + 1
    
    addEditReportTemplateSchedule(parent_report_template = 3,
                                    parent_schedule = 3,
                                    start_date = Sys.time(),
                                    is_active = TRUE,
                                    event_user = 1)
    
    TemplateEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  "SELECT * FROM ReportTemplateScheduleEvent WHERE ParentReportTemplateSchedule = ?", 
                                  next_oid))
    
    expect_equal(TemplateEvent$EventType,
                 c("Add", "EditStartDate", "Activate"))
    expect_true(all(table(TemplateEvent$EventType) == 1))
    
    addEditReportTemplateSchedule(oid = next_oid, 
                                    parent_report_template = 3, 
                                    parent_schedule = 3,
                                    start_date = Sys.time() + 365, 
                                    is_active = FALSE,
                                    event_user = 1)
    
    
    TemplateEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   "SELECT * FROM ReportTemplateScheduleEvent WHERE ParentReportTemplateSchedule = ?", 
                                   next_oid))

    expect_true(
      all(table(TemplateEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1,
              "EditStartDate" = 2))
    )
    
    dbDisconnect(conn)
  }
)
