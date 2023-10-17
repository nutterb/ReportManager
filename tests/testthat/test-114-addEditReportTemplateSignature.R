# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(addEditReportTemplateSignature(oid = "1", 
                                             parent_report_template = 1, 
                                             parent_role = 1, 
                                             order = 1,
                                             event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSignature(oid = 1:2, 
                                             parent_report_template = 1, 
                                             parent_role = 1,  
                                             order = 1,
                                             event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(1)", 
  {
    expect_error(addEditReportTemplateSignature(parent_report_template = "1", 
                                             parent_role = 1,  
                                             order = 1,
                                             event_user = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSignature(parent_report_template = 1:2, 
                                             parent_role = 1, 
                                             order = 1, 
                                             event_user = 1), 
                 "'parent_report_template': Must have length 1")
  }
)

test_that(
  "Return an error when parent_role is not integerish(1)", 
  {
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                             parent_role = "1", 
                                             order = 1, 
                                             event_user = 1), 
                 "'parent_role': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                             parent_role = 1:2, 
                                             order = 1, 
                                             event_user = 1), 
                 "'parent_role': Must have length 1")
  }
)

test_that(
  "Return an error when order is not integerish(1)", 
  {
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                             parent_role = 1, 
                                             order = "1", 
                                             event_user = 1), 
                 "'order': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                             parent_role = 1, 
                                             order = 1:2, 
                                             event_user = 1), 
                 "'order': Must have length 1")
  }
)

test_that(
  "Return an error when is_active is not logical(1)", 
  {
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                             parent_role = 1, 
                                             order = 1, 
                                             is_active = "TRUE", 
                                             event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                             parent_role = 1,  
                                             order = 1,
                                             is_active = c(TRUE, FALSE), 
                                             event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error when event_user is not integerish(1)", 
  {
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                             parent_role = 1, 
                                             order = 1, 
                                             event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                             parent_role = 1, 
                                             order = 1, 
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
    
    CurrentObjects <- queryReportTemplateSignature()
    
    next_oid <- nrow(CurrentObjects) + 1
    
    addEditReportTemplateSignature(parent_report_template = 1, 
                                   parent_role = 1,  
                                   order = 1,
                                   event_user = 1)
    
    New <- queryReportTemplateSignature(oid = next_oid)
    
    expect_data_frame(New, 
                      nrows = 1)
    expect_equal(New$ParentReportTemplate, 
                 1)
    expect_equal(New$ParentRole, 
                 1)
  }
)

test_that(
  "Return an error if attempting to add a record for an existing template-signature", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                                parent_role = 1,  
                                                order = 1,
                                                event_user = 1), 
                 "A ReportTemplateSignature record for ReportTemplate.OID")
  }
)

test_that(
  "Edit an existing record", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    addEditReportTemplateSignature(oid = 1, 
                                   parent_report_template = 1, 
                                   parent_role = 2, 
                                   order = 1,
                                   is_active = FALSE,
                                   event_user = 1)
    
    New <- queryReportTemplateSignature(oid = 1)
    
    expect_data_frame(New, 
                      nrows = 1)
    expect_equal(New$ParentReportTemplate, 
                 1)
    expect_equal(New$ParentRole, 
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
    
    last_oid <- max(queryReportTemplateSignature()$OID)
    next_oid <- last_oid + 1
    
    addEditReportTemplateSignature(parent_report_template = 2,
                                   parent_role = 3,
                                   order = 1,
                                   is_active = TRUE,
                                   event_user = 1)
    
    TemplateEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  "SELECT * FROM ReportTemplateSignatureEvent WHERE ParentReportTemplateSignature = ?", 
                                  next_oid))
    
    expect_equal(TemplateEvent$EventType,
                 c("Add", "Reorder", "Activate"))
    expect_true(all(table(TemplateEvent$EventType) == 1))
    
    addEditReportTemplateSignature(oid = next_oid, 
                                   parent_report_template = 2, 
                                   parent_role = 3,
                                   order = 2, 
                                   is_active = FALSE,
                                   event_user = 1)
    
    
    TemplateEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   "SELECT * FROM ReportTemplateSignatureEvent WHERE ParentReportTemplateSignature = ?", 
                                   next_oid))
    
    expect_true(
      all(table(TemplateEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "Reorder" = 2))
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
    
    CurrentObjects <- queryReportTemplateSignature()
    
    next_oid <- nrow(CurrentObjects) + 1
    
    addEditReportTemplateSignature(parent_report_template = 1, 
                                    parent_role = 1,  
                                    order = 1,
                                    event_user = 1)
    
    New <- queryReportTemplateSignature(oid = next_oid)
    
    expect_data_frame(New, 
                      nrows = 1)
    expect_equal(New$ParentReportTemplate, 
                 1)
    expect_equal(New$ParentRole, 
                 1)
  }
)

test_that(
  "Return an error if attempting to add a record for an existing template-signature", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    expect_error(addEditReportTemplateSignature(parent_report_template = 1, 
                                                 parent_role = 1,  
                                                 order = 1,
                                                 event_user = 1), 
                 "A ReportTemplateSignature record for ReportTemplate.OID")
  }
)

test_that(
  "Edit an existing record", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    addEditReportTemplateSignature(oid = 1, 
                                    parent_report_template = 1, 
                                    parent_role = 2, 
                                    order = 1,
                                    is_active = FALSE,
                                    event_user = 1)
    
    New <- queryReportTemplateSignature(oid = 1)
    
    expect_data_frame(New, 
                      nrows = 1)
    expect_equal(New$ParentReportTemplate, 
                 1)
    expect_equal(New$ParentRole, 
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
    
    last_oid <- max(queryReportTemplateSignature()$OID)
    next_oid <- last_oid + 1
    
    addEditReportTemplateSignature(parent_report_template = 3,
                                    parent_role = 3,
                                    order = 1,
                                    is_active = TRUE,
                                    event_user = 1)
    
    TemplateEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  "SELECT * FROM ReportTemplateSignatureEvent WHERE ParentReportTemplateSignature = ?", 
                                  next_oid))
    
    expect_equal(TemplateEvent$EventType,
                 c("Add", "Reorder", "Activate"))
    expect_true(all(table(TemplateEvent$EventType) == 1))
    
    addEditReportTemplateSignature(oid = next_oid, 
                                    parent_report_template = 3, 
                                    parent_role = 3,
                                    order = 2, 
                                    is_active = FALSE,
                                    event_user = 1)
    
    
    TemplateEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   "SELECT * FROM ReportTemplateSignatureEvent WHERE ParentReportTemplateSignature = ?", 
                                   next_oid))
    
    expect_true(
      all(table(TemplateEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "Reorder" = 2))
    )
    
    dbDisconnect(conn)
  }
)
