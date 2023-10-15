# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(addEditFooter(oid = "1", 
                               title = "Title",
                               footer = "Text", 
                               event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditFooter(oid = 1:2,
                               title = "Title",
                               footer = "Text",  
                               event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if title is not character(1)", 
  {
    expect_error(addEditFooter(title = 123,
                               footer = "Text",
                               event_user = 1), 
                 "'title': Must be of type 'string'")
    
    expect_error(addEditFooter(title = c("Title", "Text"), 
                               footer = "Text", 
                               event_user = 1), 
                 "'title': Must have length 1")
    
    expect_error(addEditFooter(title = randomVarchar(101),
                               footer = "Text", 
                               event_user = 1), 
                 "'title': All elements must have at most 100")
  }
)

test_that(
  "Return an error if footer is not character(1)", 
  {
    expect_error(addEditFooter(title = "Title",
                               footer = 123,
                               event_user = 1), 
                 "'footer': Must be of type 'string'")
    
    expect_error(addEditFooter(title = "Title", 
                               footer = c("Disclaim", "Text"), 
                               event_user = 1), 
                 "'footer': Must have length 1")
    
    expect_error(addEditFooter(title = "Title",
                               footer = randomVarchar(201), 
                               event_user = 1), 
                 "'footer': All elements must have at most 200")
  }
)

test_that(
  "Return an error if is_active is not logical(1)", 
  {
    expect_error(addEditFooter(title = "Title", 
                               footer = "Text", 
                               is_active = "TRUE", 
                               event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditFooter(title = "Title", 
                               footer = "Text", 
                               is_active = c(TRUE, FALSE), 
                               event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(addEditFooter(title = "Title", 
                               footer = "Text",
                               event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditFooter(title = "Title", 
                               footer = "Text", 
                               event_user = 1:2), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "addEditFooter functionality for SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    StartFooter <- queryFooter()
    
    # Add a new Date Reporting Format
    
    addEditFooter(title = "Title", 
                  footer = "Testing Footer",
                  event_user = 1)
    
    NewFooter <- queryFooter(oid = nrow(StartFooter) + 1)
    
    expect_data_frame(NewFooter, 
                      nrows = 1)
    
    
    # Edit an existing report format
    
    addEditFooter(oid = NewFooter$OID, 
                  title = "Modified Title",
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
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_footer_oid <- max(queryFooter()$OID)
    next_footer_oid <- last_footer_oid + 1
    
    addEditFooter(title = "Title", 
                  footer = "Footer for Event Testing",
                  is_active = TRUE, 
                  event_user = 1)
    
    FooterEvent <- dbGetQuery(conn, 
                              sqlInterpolate(
                                conn,
                                "SELECT * FROM dbo.FooterEvent WHERE ParentFooter = ?", 
                                next_footer_oid))
    
    expect_equal(FooterEvent$EventType,
                 c("Add", "EditTitle", "EditFooter", "Activate"))
    expect_true(all(table(FooterEvent$EventType) == 1))
    
    addEditFooter(oid = next_footer_oid,
                  title = "Edited Title",
                  footer = "Edited Footer",
                  is_active = FALSE, 
                  event_user = 1)
    
    FooterEvent2 <- dbGetQuery(conn, 
                               sqlInterpolate(
                                 conn,
                                 "SELECT * FROM dbo.FooterEvent WHERE ParentFooter = ?", 
                                 next_footer_oid))
    
    expect_true(
      all(table(FooterEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "EditFooter" = 2, 
              "EditTitle" = 2))
    )
    
    dbDisconnect(conn)
  }
)

# Functionality - SQLite --------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "addEditFooter functionality for SQL Server", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    StartFooter <- queryFooter()
    
    # Add a new Date Reporting Format
    
    addEditFooter(title = "Title", 
                  footer = "Testing Footer",
                  event_user = 1)
    
    NewFooter <- queryFooter(oid = nrow(StartFooter) + 1)
    
    expect_data_frame(NewFooter, 
                      nrows = 1)
    
    
    # Edit an existing report format
    
    addEditFooter(oid = NewFooter$OID, 
                  title = "Modified Title",
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
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_footer_oid <- max(queryFooter()$OID)
    next_footer_oid <- last_footer_oid + 1
    
    addEditFooter(title = "Title", 
                  footer = "Footer for Event Testing",
                  is_active = TRUE, 
                  event_user = 1)
    
    FooterEvent <- dbGetQuery(conn, 
                              sqlInterpolate(
                                conn,
                                "SELECT * FROM FooterEvent WHERE ParentFooter = ?", 
                                next_footer_oid))
    
    expect_equal(FooterEvent$EventType,
                 c("Add", "EditTitle", "EditFooter", "Activate"))
    expect_true(all(table(FooterEvent$EventType) == 1))
    
    addEditFooter(oid = next_footer_oid,
                  title = "Edited Title",
                  footer = "Edited Footer",
                  is_active = FALSE, 
                  event_user = 1)
    
    FooterEvent2 <- dbGetQuery(conn, 
                               sqlInterpolate(
                                 conn,
                                 "SELECT * FROM FooterEvent WHERE ParentFooter = ?", 
                                 next_footer_oid))
    
    expect_true(
      all(table(FooterEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "EditFooter" = 2, 
              "EditTitle" = 2))
    )
    
    dbDisconnect(conn)
  }
)
