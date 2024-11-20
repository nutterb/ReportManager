# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(addEditDisclaimer(oid = "1", 
                                   disclaimer = "Text", 
                                   event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditDisclaimer(oid = 1:2, 
                                   disclaimer = "Text",  
                                   event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if disclaimer is not character(1)", 
  {
    expect_error(addEditDisclaimer(disclaimer = 123,
                                   event_user = 1), 
                 "'disclaimer': Must be of type 'string'")
    
    expect_error(addEditDisclaimer(disclaimer = c("Disclaim", "Text"), 
                                   event_user = 1), 
                 "'disclaimer': Must have length 1")
    
    expect_error(addEditDisclaimer(disclaimer = randomVarchar(2001), 
                                   event_user = 1), 
                 "'disclaimer': All elements must have at most 2000")
  }
)

test_that(
  "Return an error if is_active is not logical(1)", 
  {
    expect_error(addEditDisclaimer(disclaimer = "Text", 
                                   is_active = "TRUE", 
                                   event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditDisclaimer(disclaimer = "Text", 
                                   is_active = c(TRUE, FALSE), 
                                   event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(addEditDisclaimer(disclaimer = "Text",
                                   event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditDisclaimer(disclaimer = "Text", 
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
    "addEditDisclaimer functionality for SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      StartDisclaim <- queryDisclaimer()
      
      # Add a new Date Reporting Format
      
      addEditDisclaimer(disclaimer = "Testing Disclaimer",
                        event_user = 1)
      
      NewDisclaim <- queryDisclaimer(oid = nrow(StartDisclaim) + 1)
      
      expect_data_frame(NewDisclaim, 
                        nrows = 1)
      
      
      # Edit an existing report format
      
      addEditDisclaimer(oid = NewDisclaim$OID, 
                        disclaimer = "Modified Disclaimer", 
                        event_user = 1)
      
      NewDisclaim <- queryDisclaimer(oid = NewDisclaim$OID)
      
      expect_data_frame(NewDisclaim, 
                        nrows = 1)
      
      expect_equal(NewDisclaim$Disclaimer, 
                   "Modified Disclaimer")
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      last_disclaimer_oid <- max(queryDisclaimer()$OID)
      next_disclaimer_oid <- last_disclaimer_oid + 1
      
      addEditDisclaimer(disclaimer = "Disclaimer for Event Testing",
                        is_active = TRUE, 
                        event_user = 1)
      
      DisclaimerEvent <- dbGetQuery(conn, 
                                    sqlInterpolate(
                                      conn,
                                      switch(flavor, 
                                             "sql_server" = "SELECT * FROM dbo.DisclaimerEvent WHERE ParentDisclaimer = ?",
                                             "SELECT * FROM DisclaimerEvent WHERE ParentDisclaimer = ?"),
                                      next_disclaimer_oid))
      
      expect_equal(DisclaimerEvent$EventType,
                   c("Add", "EditDisclaimer", "Activate"))
      expect_true(all(table(DisclaimerEvent$EventType) == 1))
      
      addEditDisclaimer(oid = next_disclaimer_oid,
                        disclaimer = "Edited Disclaimer",
                        is_active = FALSE, 
                        event_user = 1)
      
      DisclaimEvent2 <- dbGetQuery(conn, 
                                   sqlInterpolate(
                                     conn,
                                     switch(flavor, 
                                            "sql_server" = "SELECT * FROM dbo.DisclaimerEvent WHERE ParentDisclaimer = ?",
                                            "SELECT * FROM DisclaimerEvent WHERE ParentDisclaimer = ?"), 
                                     next_disclaimer_oid))
      
      expect_true(
        all(table(DisclaimEvent2$EventType) ==
              c("Activate" = 1, 
                "Add" = 1, 
                "Deactivate" = 1, 
                "EditDisclaimer" = 2))
      )
      
      dbDisconnect(conn)
    }
  )
}
