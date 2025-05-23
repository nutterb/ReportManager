# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(addEditFooter(oid = "1", 
                               footer = "Text", 
                               event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditFooter(oid = 1:2,
                               footer = "Text",  
                               event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if footer is not character(1)", 
  {
    expect_error(addEditFooter(footer = 123,
                               event_user = 1), 
                 "'footer': Must be of type 'string'")
    
    expect_error(addEditFooter(footer = c("Disclaim", "Text"), 
                               event_user = 1), 
                 "'footer': Must have length 1")
    
    expect_error(addEditFooter(footer = randomVarchar(201), 
                               event_user = 1), 
                 "'footer': All elements must have at most 200")
  }
)

test_that(
  "Return an error if is_active is not logical(1)", 
  {
    expect_error(addEditFooter(footer = "Text", 
                               is_active = "TRUE", 
                               event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditFooter(footer = "Text", 
                               is_active = c(TRUE, FALSE), 
                               event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(addEditFooter(footer = "Text",
                               event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditFooter(footer = "Text", 
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
    initializeReportManagerDatabase(SQL_FILE[flavor], 
                                    last_name = "Doe", 
                                    first_name = "Jane", 
                                    login_id = "jdoe", 
                                    email = "jdoe@domain.com")
  }
  
  test_that(
    "addEditFooter functionality for SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      StartFooter <- queryFooter()
      
      # Add a new Date Reporting Format
      
      addEditFooter(footer = "Testing Footer",
                    event_user = 1)
      
      NewFooter <- queryFooter(oid = nrow(StartFooter) + 1)
      
      expect_data_frame(NewFooter, 
                        nrows = 1)
      
      
      # Edit an existing report format
      
      addEditFooter(oid = NewFooter$OID, 
                    footer = "Modified Footer", 
                    event_user = 1)
      
      NewFooter <- queryFooter(oid = NewFooter$OID)
      
      expect_data_frame(NewFooter, 
                        nrows = 1)
      
      expect_equal(NewFooter$Footer, 
                   "Modified Footer")
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      last_footer_oid <- max(queryFooter()$OID)
      next_footer_oid <- last_footer_oid + 1
      
      addEditFooter(footer = "Footer for Event Testing",
                    is_active = TRUE, 
                    event_user = 1)
      
      FooterEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  switch(flavor, 
                                         "sql_server" = "SELECT * FROM dbo.FooterEvent WHERE ParentFooter = ?",
                                         "SELECT * FROM FooterEvent WHERE ParentFooter = ?"), 
                                  next_footer_oid))
      
      expect_equal(FooterEvent$EventType,
                   c("Add", "EditFooter", "Activate"))
      expect_true(all(table(FooterEvent$EventType) == 1))
      
      addEditFooter(oid = next_footer_oid,
                    footer = "Edited Footer",
                    is_active = FALSE, 
                    event_user = 1)
      
      FooterEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   switch(flavor, 
                                          "sql_server" = "SELECT * FROM dbo.FooterEvent WHERE ParentFooter = ?",
                                          "SELECT * FROM FooterEvent WHERE ParentFooter = ?"),
                                   next_footer_oid))
      
      expect_true(
        all(table(FooterEvent2$EventType) ==
              c("Activate" = 1, 
                "Add" = 1, 
                "Deactivate" = 1, 
                "EditFooter" = 2))
      )
      
      dbDisconnect(conn)
    }
  )
}
