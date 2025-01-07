# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(
      addEditAutoDistribution(oid = "1", 
                            parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'oid': Must be of type 'integerish'")
    
    expect_error(
      addEditAutoDistribution(oid = 1:2, 
                            parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if parent_report_template is not integerish(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = "1", 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1:2, 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'parent_report_template': Must have length 1")
  }
)

test_that(
  "Return an error if parent_schedule is not integerish(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = "1", 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'parent_schedule': Must be of type 'integerish'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = 1:2, 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'parent_schedule': Must have length 1")
  }
)

test_that(
  "Return an error if start_date_time is not POSIXct(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = format(Sys.time()), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'start_date_time': Must be of type 'POSIXct'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = rep(Sys.time(), 2), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'start_date_time': Must have length 1")
  }
)

test_that(
  "Return an error if is_active is not logical(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = "TRUE", 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'is_active': Must be of type 'logical'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = c(FALSE, TRUE), 
                            delay_after_instance_end = 1, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if delay_after_instance_end is not integerish(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = "1", 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'delay_after_instance_end': Must be of type 'integerish'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1:2, 
                            delay_units = "Second", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'delay_after_instance_end': Must have length 1")
  }
)

test_that(
  "Return an error if delay_units is not an appropriate option", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                            parent_schedule = 1, 
                            start_date_time = Sys.time(), 
                            is_active = TRUE, 
                            delay_after_instance_end = 1, 
                            delay_units = "None", 
                            current_or_last_instance = "Current", 
                            is_add_to_archive = FALSE, 
                            report_format = "html", 
                            is_distribute_internal_only = TRUE,  
                            is_embed_html = TRUE, 
                            event_user = 1), 
      "'delay_units': Must be element of set")
  }
)

test_that(
  "Return an error if current_or_last_instance is not an appropriate option", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Future", 
                              is_add_to_archive = FALSE, 
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1), 
      "'current_or_last_instance': Must be element of set")
  }
)

test_that(
  "Return an error if is_add_to_archive is not logical(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = "FALSE", 
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1), 
      "'is_add_to_archive': Must be of type 'logical'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = FALSE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = c(FALSE, TRUE),
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1), 
      "'is_add_to_archive': Must have length 1")
  }
)

test_that(
  "Return an error if report_format is not an appropriate option", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE, 
                              report_format = "bad file", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1), 
      "'report_format': Must be element of set")
  }
)

test_that(
  "Return an error if is_distribute_internal_only is not logical(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE, 
                              report_format = "html", 
                              is_distribute_internal_only = "TRUE",  
                              is_embed_html = TRUE, 
                              event_user = 1), 
      "'is_distribute_internal_only': Must be of type 'logical'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = FALSE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE,
                              report_format = "html", 
                              is_distribute_internal_only = c(FALSE, TRUE),  
                              is_embed_html = TRUE, 
                              event_user = 1), 
      "'is_distribute_internal_only': Must have length 1")
  }
)

test_that(
  "Return an error if is_embed_html is not logical(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE, 
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = "TRUE", 
                              event_user = 1), 
      "'is_embed_html': Must be of type 'logical'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = FALSE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE,
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = c(TRUE, FALSE), 
                              event_user = 1), 
      "'is_embed_html': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE, 
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = "1"), 
      "'event_user': Must be of type 'integerish'")
    
    expect_error(
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE, 
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1:2), 
      "'event_user': Must have length 1")
  }
)

# Functionality -----------------------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "Add a new AutoDistribution record", 
    {
      skip_if_not(.ready, 
                  .message)
      
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE, 
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1)
      
      CurrentObjects <- queryAutoDistribution()
      
      next_oid <- nrow(CurrentObjects) + 1
      
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 2, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE, 
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1)
      
      New <- queryAutoDistribution(oid = next_oid)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_equal(New$ParentReportTemplate, 
                   1)
      expect_equal(New$ParentSchedule, 
                   2)
    }
  )
  
 
  test_that(
    "Edit an existing record", 
    {
      skip_if_not(.ready, 
                  .message)
      
      addEditAutoDistribution(oid = 1, 
                              parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = FALSE, 
                              delay_after_instance_end = 2, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = FALSE, 
                              report_format = "html", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1)
      
      New <- queryAutoDistribution(oid = 1)
      
      expect_data_frame(New, 
                        nrows = 1)
      expect_false(New$IsActive)
      expect_equal(New$DelayAfterInstanceEnd, 
                   2)
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      last_oid <- max(queryAutoDistribution()$OID)
      next_oid <- last_oid + 1
      
      addEditAutoDistribution(parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = TRUE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "Current", 
                              is_add_to_archive = TRUE, 
                              report_format = "pdf", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1)

      InstanceEvent <- dbGetQuery(conn,
                                  sqlInterpolate(
                                    conn,
                                    switch(flavor,
                                           "sql_server" = "SELECT * FROM dbo.AutoDistributionEvent WHERE ParentAutoDistribution = ?",
                                           "SELECT * FROM AutoDistributionEvent WHERE ParentAutoDistribution = ?"),
                                    next_oid))

      expect_equal(InstanceEvent$EventType,
                   c("Add", 
                     "Activate", 
                     "EditSchedule", 
                     "EditStartDateTime", 
                     "EditDelayAfterInstanceEnd", 
                     "EditDelayUnits", 
                     "EditCurrentOrLastInstance", 
                     "EditIsAddToArchive", 
                     "EditReportFormat", 
                     "EditIsDistributeInternalOnly", 
                     "EditIsEmbedHtml"))
      expect_true(all(table(InstanceEvent$EventType) == 1))

      addEditAutoDistribution(oid = next_oid, 
                              parent_report_template = 1, 
                              parent_schedule = 1, 
                              start_date_time = Sys.time(), 
                              is_active = FALSE, 
                              delay_after_instance_end = 1, 
                              delay_units = "Second", 
                              current_or_last_instance = "LastCompleted", 
                              is_add_to_archive = TRUE, 
                              report_format = "pdf", 
                              is_distribute_internal_only = TRUE,  
                              is_embed_html = TRUE, 
                              event_user = 1)


      InstanceEvent2 <- dbGetQuery(conn,
                                   sqlInterpolate(
                                     conn,
                                     switch(flavor,
                                            "sql_server" = "SELECT * FROM dbo.AutoDistributionEvent WHERE ParentAutoDistribution = ?",
                                            "SELECT * FROM AutoDistributionEvent WHERE ParentAutoDistribution = ?"),
                                     next_oid))

      expect_true(
        all(table(InstanceEvent2$EventType) ==
              c("Activate" = 1, 
                "Add" = 1, 
                "Deactivate" = 1, 
                "EditCurrentOrLastInstance" = 2, 
                "EditDelayAfterInstanceEnd" = 1, 
                "EditDelayUnits" = 1, 
                "EditIsAddToArchive" = 1, 
                "EditIsDistributeInternalOnly" = 1, 
                "EditIsEmbedHtml" = 1, 
                "EditReportFormat" = 1, 
                "EditSchedule" = 1, 
                "EditStartDateTime" = 2))
      )
      
      dbDisconnect(conn)
    }
  )
}