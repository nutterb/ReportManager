# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(addEditDateReportingFormat(oid = "1", 
                                            format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditDateReportingFormat(oid = 1:2, 
                                            format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if format_name is not character(1)", 
  {
    expect_error(addEditDateReportingFormat(format_name = 123, 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            event_user = 1), 
                 "'format_name': Must be of type 'string'")
    
    expect_error(addEditDateReportingFormat(format_name = c("Date", "Format"), 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            event_user = 1), 
                 "'format_name': Must have length 1")
    
    expect_error(addEditDateReportingFormat(format_name = randomVarchar(26), 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            event_user = 1), 
                 "'format_name': All elements must have at most 25")
  }
)

test_that(
  "Return an error if description is not character(1)", 
  {
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = 15, 
                                            format_code = "%d %b %Y", 
                                            event_user = 1), 
                 "'description': Must be of type 'string'")
    
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = c("15 Jan ", "2000"), 
                                            format_code = "%d %b %Y", 
                                            event_user = 1), 
                 "'description': Must have length 1")
    
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = randomVarchar(51), 
                                            format_code = "%d %b %Y", 
                                            event_user = 1), 
                 "'description': All elements must have at most 50")
  }
)

test_that(
  "Return an error if format_code is not character(1)", 
  {
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = 123, 
                                            event_user = 1), 
                 "'format_code': Must be of type 'string'")
    
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = c("%d %b", " %Y"), 
                                            event_user = 1), 
                 "'format_code': Must have length 1")
    
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = randomVarchar(26), 
                                            event_user = 1), 
                 "'format_code': All elements must have at most 25")
  }
)

test_that(
  "Return an error if increment_start is not integerish(1)", 
  {
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y",
                                            increment_start = "1",
                                            event_user = 1), 
                 "'increment_start': Must be of type 'integerish'")
    
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            increment_start = 1:2,
                                            event_user = 1), 
                 "'increment_start': Must have length 1")
  }
)

test_that(
  "Return an error if increment_start_unit is not an accepted value", 
  {
    expect_error(addEditDateReportingFormat(format_name = "date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            increment_start = 1, 
                                            increment_start_unit = "milligram", 
                                            event_user = 1), 
                 "'increment_start_unit': Must be element of")
  }
)

test_that(
  "Return an error if increment_end is not integerish(1)", 
  {
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y",
                                            increment_end = "1",
                                            event_user = 1), 
                 "'increment_end': Must be of type 'integerish'")
    
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            increment_end = 1:2,
                                            event_user = 1), 
                 "'increment_end': Must have length 1")
  }
)

test_that(
  "Return an error if increment_end_unit is not an accepted value", 
  {
    expect_error(addEditDateReportingFormat(format_name = "date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            increment_end = 1, 
                                            increment_end_unit = "milligram", 
                                            event_user = 1), 
                 "'increment_end_unit': Must be element of")
  }
)

test_that(
  "Return an error if is_active is not logical(1)", 
  {
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            is_active = "TRUE", 
                                            event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            is_active = c(TRUE, FALSE), 
                                            event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y",
                                            event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditDateReportingFormat(format_name = "Date", 
                                            description = "15 Jan 2000", 
                                            format_code = "%d %b %Y", 
                                            event_user = 1:2), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "addEditDateReportingFormat functionality for SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    StartFormat <- queryDateReportingFormat()
   
    # Add a new Date Reporting Format
    
    addEditDateReportingFormat(format_name = "Date (with full month)", 
                               description = "15 January 2000", 
                               format_code = "%d %B %Y",
                               increment_end = -1, 
                               increment_end_unit = "Second",
                               event_user = 1)
    
    NewFormat <- queryDateReportingFormat(oid = nrow(StartFormat) + 1)
    
    expect_data_frame(NewFormat, 
                      nrows = 1)
    
    
    # Edit an existing report format
    
    addEditDateReportingFormat(oid = NewFormat$OID, 
                               format_name = "Date with no month", 
                               description = "15 2000", 
                               format_code = "%d %Y", 
                               increment_end = -1, 
                               increment_end_unit = "Second", 
                               event_user = 1)
    
    NewFormat <- queryDateReportingFormat(oid = NewFormat$OID)
    
    expect_data_frame(NewFormat, 
                      nrows = 1)
    
    expect_equal(NewFormat$FormatName, 
                 "Date with no month")
    
    expect_equal(NewFormat$Description, 
                 "15 2000")
    
    expect_equal(NewFormat$FormatCode, 
                 "%d %Y")
  }
)



test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_format_oid <- max(queryDateReportingFormat()$OID)
    next_format_oid <- last_format_oid + 1
    
    addEditDateReportingFormat(format_name = "Date format event test",
                               description = "testing events when editing date formats", 
                               format_code = "%Y-%m-%d",
                               increment_start = 0, 
                               increment_start_unit = "Second", 
                               increment_end = -1, 
                               increment_end_unit = "Second",
                               is_active = FALSE, 
                               event_user = 1)
    
    FormatEvent <- dbGetQuery(conn, 
                              sqlInterpolate(
                                conn,
                                "SELECT * FROM dbo.DateReportingFormatEvent WHERE ParentDateReportingFormat = ?", 
                                next_format_oid))
    
    expect_equal(FormatEvent$EventType,
                 c("Add", "Deactivate", "EditFormatName", "EditFormatDescription", 
                   "EditFormatCode", "EditIncrementStart", "EditIncrementEnd"))
    expect_true(all(table(FormatEvent$EventType) == 1))
    
    addEditDateReportingFormat(oid = next_format_oid, 
                               format_name = "change Date format",
                               description = "change testing events when editing date formats", 
                               format_code = "%Y-%m-%d %H:%M:%S",
                               increment_start = 1, 
                               increment_start_unit = "Second", 
                               increment_end = -1, 
                               increment_end_unit = "Day",
                               is_active = TRUE, 
                               event_user = 1)
    
    
    FormatEvent2 <- dbGetQuery(conn, 
                               sqlInterpolate(
                                 conn,
                                 "SELECT * FROM dbo.DateReportingFormatEvent WHERE ParentDateReportingFormat = ?", 
                                 next_format_oid))
    
    expect_true(
      all(table(FormatEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "EditFormatCode" = 2, 
              "EditFormatDescription" = 2, 
              "EditFormatName" = 2, 
              "EditIncrementEnd" = 2, 
              "EditIncrementStart" = 2))
    )
    
    dbDisconnect(conn)
  }
)

# Functionality - SQLite --------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "addEditDateReportingFormat functionality for SQL Server", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    StartFormat <- queryDateReportingFormat()
    
    # Add a new Date Reporting Format
    
    addEditDateReportingFormat(format_name = "Date (with full month)", 
                               description = "15 January 2000", 
                               format_code = "%d %B %Y",
                               increment_end = -1, 
                               increment_end_unit = "Second",
                               event_user = 1)
    
    NewFormat <- queryDateReportingFormat(oid = nrow(StartFormat) + 1)
    
    expect_data_frame(NewFormat, 
                      nrows = 1)
    
    
    # Edit an existing report format
    
    addEditDateReportingFormat(oid = NewFormat$OID, 
                               format_name = "Date with no month", 
                               description = "15 2000", 
                               format_code = "%d %Y", 
                               increment_end = -1, 
                               increment_end_unit = "Second", 
                               event_user = 1)
    
    NewFormat <- queryDateReportingFormat(oid = NewFormat$OID)
    
    expect_data_frame(NewFormat, 
                      nrows = 1)
    
    expect_equal(NewFormat$FormatName, 
                 "Date with no month")
    
    expect_equal(NewFormat$Description, 
                 "15 2000")
    
    expect_equal(NewFormat$FormatCode, 
                 "%d %Y")
  }
)

test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_format_oid <- max(queryDateReportingFormat()$OID)
    next_format_oid <- last_format_oid + 1
    
    addEditDateReportingFormat(format_name = "Date format event test",
                               description = "testing events when editing date formats", 
                               format_code = "%Y-%m-%d",
                               increment_start = 0, 
                               increment_start_unit = "Second", 
                               increment_end = -1, 
                               increment_end_unit = "Second",
                               is_active = FALSE, 
                               event_user = 1)
    
    FormatEvent <- dbGetQuery(conn, 
                              sqlInterpolate(
                                  conn,
                                  "SELECT * FROM DateReportingFormatEvent WHERE ParentDateReportingFormat = ?", 
                                  next_format_oid))
    
    expect_equal(FormatEvent$EventType,
                 c("Add", "Deactivate", "EditFormatName", "EditFormatDescription", 
                   "EditFormatCode", "EditIncrementStart", "EditIncrementEnd"))
    expect_true(all(table(FormatEvent$EventType) == 1))
    
    addEditDateReportingFormat(oid = next_format_oid, 
                               format_name = "change Date format",
                               description = "change testing events when editing date formats", 
                               format_code = "%Y-%m-%d %H:%M:%S",
                               increment_start = 1, 
                               increment_start_unit = "Second", 
                               increment_end = -1, 
                               increment_end_unit = "Day",
                               is_active = TRUE, 
                               event_user = 1)
    
    
    FormatEvent2 <- dbGetQuery(conn, 
                               sqlInterpolate(
                                 conn,
                                 "SELECT * FROM DateReportingFormatEvent WHERE ParentDateReportingFormat = ?", 
                                 next_format_oid))
    
    expect_true(
      all(table(FormatEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "EditFormatCode" = 2, 
              "EditFormatDescription" = 2, 
              "EditFormatName" = 2, 
              "EditIncrementEnd" = 2, 
              "EditIncrementStart" = 2))
    )
    
    dbDisconnect(conn)
  }
)
