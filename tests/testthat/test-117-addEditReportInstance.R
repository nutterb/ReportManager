# Argument Validation -----------------------------------------------

test_that(
  "Return an error when report_instance_oid is not integerish(0/1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = "1", 
                                       parent_report_template = numeric(0), 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'report_instance_oid': Must be of type 'integerish'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1:2, 
                                       parent_report_template = numeric(0), 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'report_instance_oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = "1", 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1:2, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'parent_report_template': Must have length [<][=] 1")
  }
)

test_that(
  "Return an error when start_time is not POSIXct(1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = "now", 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'start_time': Must be of type 'POSIXct'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = rep(Sys.time(), 2), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'start_time': Must have length 1")
  }
)

test_that(
  "Return an error when end_time is not POSIXct(1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = "now", 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'end_time': Must be of type 'POSIXct'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = rep(Sys.time(), 2), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'end_time': Must have length 1")
  }
)


test_that(
  "Return an error when is_signature_required is not logical(1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = "FALSE", 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'is_signature_required': Must be of type 'logical'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = c(TRUE, FALSE), 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'is_signature_required': Must have length 1")
  }
)

test_that(
  "Return an error when is_scheduled is not logical(1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = "FALSE", 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'is_scheduled': Must be of type 'logical'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = c(TRUE, FALSE), 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'is_scheduled': Must have length 1")
  }
)

test_that(
  "Return an error when instance_title is not character(1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = 123, 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'instance_title': Must be of type 'character'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = c("Title", "More Title"), 
                                       is_submitted = FALSE, 
                                       event_user = 1), 
                 "'instance_title': Must have length 1")
  }
)

test_that(
  "Return an error when is_submitted is not logical(1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = "FALSE", 
                                       event_user = 1), 
                 "'is_submitted': Must be of type 'logical'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = c(TRUE, FALSE), 
                                       event_user = 1), 
                 "'is_submitted': Must have length 1")
  }
)

test_that(
  "Return an error when event_user is not integerish(1)", 
  {
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditReportInstance(report_instance_oid = 1, 
                                       parent_report_template = 1, 
                                       start_time = Sys.time(), 
                                       end_time = Sys.time(), 
                                       is_signature_required = FALSE, 
                                       is_scheduled = FALSE, 
                                       instance_title = "Title", 
                                       is_submitted = FALSE, 
                                       event_user = 1:2), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
    purgeReportManagerDatabase()
    initializeUiTestingDatabase(SQL_FILE[flavor], 
                                include = c("User", "Role", "UserRole", 
                                            "ReportTemplate"))
  }

  test_that(
    "Add a Report Instance", 
    {
      skip_if_not(.ready, 
                  .message)
      
      CurrentObjects <- queryReportInstance()
      
      next_oid <- nrow(CurrentObjects) + 1
      
      addEditReportInstance(report_instance_oid = numeric(0), 
                            parent_report_template = 1, 
                            start_time = Sys.time(), 
                            end_time = Sys.time(), 
                            is_signature_required = FALSE, 
                            is_scheduled = FALSE, 
                            instance_title = "Title", 
                            is_submitted = FALSE, 
                            event_user = 1)
      
      New <- queryReportInstance(report_instance_oid = next_oid)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_equal(New$OID, 
                   1)
      expect_equal(New$ParentReportTemplate, 
                   1)
    }
  )
    
  test_that(
    "Edit an existing reportInstance", 
    {
      skip_if_not(.ready, 
                  .message)
      
      addEditReportInstance(report_instance_oid = 1, 
                            parent_report_template = 1, 
                            start_time = Sys.time(), 
                            end_time = Sys.time(), 
                            is_signature_required = FALSE, 
                            is_scheduled = FALSE, 
                            instance_title = "Title", 
                            is_submitted = FALSE, 
                            event_user = 1)
      
      New <- queryReportInstance(report_instance_oid = 1)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_equal(New$ParentReportTemplate, 
                   1)
      expect_false(New$IsSubmitted)
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      last_oid <- max(queryReportInstance()$OID)
      next_oid <- last_oid + 1
      
      addEditReportInstance(report_instance_oid = numeric(0), 
                            parent_report_template = 1, 
                            start_time = Sys.time(), 
                            end_time = Sys.time(), 
                            is_signature_required = TRUE, 
                            is_scheduled = TRUE, 
                            instance_title = "New Title", 
                            is_submitted = TRUE, 
                            event_user = 1)
      
      TemplateEvent <- dbGetQuery(conn, 
                                  sqlInterpolate(
                                    conn,
                                    switch(flavor, 
                                           "sql_server" = "SELECT * FROM dbo.ReportInstanceEvent WHERE ParentReportInstance = ?",
                                           "SELECT * FROM ReportInstanceEvent WHERE ParentReportInstance = ?"),
                                    next_oid))
      
      expect_equal(TemplateEvent$EventType,
                   c("Add", 
                     "EditStartTime", "EditEndTime", 
                     "EditIsScheduled", "EditIsSignatureRequired", 
                     "EditIsSubmitted", "EditInstanceTitle"))
      expect_true(all(table(TemplateEvent$EventType) == 1))
      
      addEditReportInstance(report_instance_oid = numeric(0), 
                            parent_report_template = 1, 
                            start_time = Sys.time(), 
                            end_time = Sys.time(), 
                            is_signature_required = FALSE, 
                            is_scheduled = FALSE, 
                            instance_title = "Different Title", 
                            is_submitted = FALSE, 
                            event_user = 1)
      
      
      TemplateEvent2 <- dbGetQuery(conn, 
                                   sqlInterpolate(
                                     conn,
                                     switch(flavor, 
                                            "sql_server" = "SELECT * FROM dbo.ReportInstanceEvent WHERE ParentReportInstance = ?",
                                            "SELECT * FROM ReportInstanceEvent WHERE ParentReportInstance = ?"),
                                     next_oid))
      
      expect_true(
        all(table(TemplateEvent2$EventType) ==
              c("Add" = 1, 
                "EditEndTime" = 1,
                "EditInstanceTitle" = 1, 
                "EditIsScheduled" = 1, 
                "EditIsSignatureRequired" = 1, 
                "EditIsSubmitted" = 1, 
                "EditStartTime" = 1))
      )
      
      dbDisconnect(conn)
    }
  )
}
