temp_file <- tempfile(fileext = ".txt")
writeLines("Testing addEditFileArchive", 
           temp_file)

# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(addEditReportTemplate(oid = "1", 
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplate(oid = 1:2, 
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when template_directory is not character(1)",
  {
    expect_error(addEditReportTemplate(template_directory = 123, 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'template_directory': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_directory = letters, 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'template_directory': Must have length 1")
    
    expect_error(addEditReportTemplate(template_directory = randomVarchar(51), 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'template_directory': All elements must have at most 50")
  }
)

test_that(
  "Return an error when template_file is not character(1)",
  {
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = 123, 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'template_file': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = letters, 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'template_file': Must have length 1")
    
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = randomVarchar(51), 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'template_file': All elements must have at most 50")
  }
)

test_that(
  "Return an error when title is not character(1)",
  {
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = 123, 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'title': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = letters, 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'title': Must have length 1")
    
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = randomVarchar(201), 
                                       title_size = "LARGE", 
                                       event_user = 1), 
                 "'title': All elements must have at most 200")
  }
)

test_that(
  "Return an error if title_size is not an acceptable value", 
  {
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "something or other", 
                                       event_user = 1), 
                 "'title_size': Must be element of set")
  }
)

test_that(
  "Return an error if is_signature_required is not logical(1)", 
  {
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       is_signature_required = "TRUE", 
                                       event_user = 1), 
                 "'is_signature_required': Must be of type 'logical'")
    
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       is_signature_required = c(TRUE, FALSE), 
                                       event_user = 1), 
                 "'is_signature_required': Must have length 1")
  }
)

test_that(
  "Return an error if is_active is not logical(1)", 
  {
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       is_active = "TRUE", 
                                       event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       is_active = c(TRUE, FALSE), 
                                       event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if logo_oid is not integerish(0/1)", 
  {
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1, 
                                       logo_oid = "1"), 
                 "'logo_oid': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1, 
                                       logo_oid = 1:2), 
                 "'logo_oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplate(template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       event_user = 1:2), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

if (SQL_SERVER_READY){
  configureReportManager(flavor = "sql_server")
  purgeReportManagerDatabase()
  initializeReportManagerDatabase(system.file("Sql/SqlServer.sql", 
                                              package = "ReportManager"), 
                                  last_name = "Doe", 
                                  first_name = "Jane", 
                                  login_id = "jdoe", 
                                  email = "jdoe@domain.com")
  addLogo(file_path = temp_file, 
          description = "This is a logo")
  addLogo(file_path = temp_file, 
          description = "This is another logo")
}

test_that(
  "addEditReportTemplate functionality for SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    StartTemplate <- queryReportTemplate()
    
    # Add a new Date Reporting Format
    
    addEditReportTemplate(template_directory = "TestDirectory", 
                          template_file = "Filename",
                          title = "Report Title", 
                          title_size = "Large", 
                          is_signature_required = FALSE, 
                          is_active = TRUE,
                          logo_oid = 1,
                          event_user = 1)
    
    NewTemplate <- queryReportTemplate(oid = nrow(StartTemplate) + 1)
    
    expect_data_frame(NewTemplate, 
                      nrows = 1)
    
    
    # Edit an existing report format
    
    addEditReportTemplate(oid = NewTemplate$OID, 
                          template_directory = "TestDirectory2", 
                          template_file = "Filename2",
                          title = "Report Title2", 
                          title_size = "LARGE", 
                          is_signature_required = TRUE, 
                          is_active = FALSE,
                          logo_oid = 2,
                          event_user = 1)
    
    NewTemplate <- queryReportTemplate(oid = NewTemplate$OID)
    
    expect_data_frame(NewTemplate, 
                      nrows = 1)
    
    expect_equal(NewTemplate$TemplateDirectory, 
                 "TestDirectory2")
    expect_equal(NewTemplate$TemplateFile, 
                 "Filename2")
    expect_equal(NewTemplate$TitleSize, 
                 "LARGE")
  }
)

test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_template_oid <- max(queryReportTemplate()$OID)
    next_template_oid <- last_template_oid + 1
    
    addEditReportTemplate(template_directory = "TestDirectory2", 
                          template_file = "Filename2",
                          title = "Report Title2", 
                          title_size = "LARGE", 
                          is_signature_required = TRUE, 
                          is_active = FALSE,
                          logo_oid = 2,
                          event_user = 1)
    
    TemplateEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  "SELECT * FROM dbo.ReportTemplateEvent WHERE ParentReportTemplate = ?", 
                                  next_template_oid))
    
    expect_equal(TemplateEvent$EventType,
                 c("Add", "SetSignatureRequiredTrue", "Deactivate", 
                   "EditTemplateFolder", "EditTemplateFile", 
                   "EditTitle", "EditTitleSize", "EditLogoFile"))
    expect_true(all(table(TemplateEvent$EventType) == 1))
    
    addEditReportTemplate(oid = next_template_oid, 
                          template_directory = "TestDirectoryEdit", 
                          template_file = "FilenameEdit",
                          title = "Report Title Edit", 
                          title_size = "large", 
                          is_signature_required = FALSE, 
                          is_active = TRUE,
                          logo_oid = 1,
                          event_user = 1)
    
    
    TemplateEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   "SELECT * FROM dbo.ReportTemplateEvent WHERE ParentReportTemplate = ?", 
                                   next_template_oid))
    
    expect_true(
      all(table(TemplateEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "EditLogoFile" = 2, 
              "EditTemplateFile" = 2, 
              "EditTemplateFolder" = 2, 
              "EditTitle" = 2, 
              "EditTitleSize" = 2, 
              "SetSignatureRequiredFalse" = 1, 
              "SetSignatureRequiredTrue" = 1))
    )
    
    dbDisconnect(conn)
  }
)


# Functionality - SQLite --------------------------------------------

if (SQLITE_READY){
  configureReportManager(flavor = "sqlite")
  purgeReportManagerDatabase()
  initializeReportManagerDatabase(system.file("Sql/Sqlite.sql", 
                                              package = "ReportManager"), 
                                  last_name = "Doe", 
                                  first_name = "Jane", 
                                  login_id = "jdoe", 
                                  email = "jdoe@domain.com")
  addLogo(file_path = temp_file, 
          description = "This is a logo")
  addLogo(file_path = temp_file, 
          description = "This is another logo")
}

test_that(
  "addEditDisclaimer functionality for SQLite", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    StartTemplate <- queryReportTemplate()
    
    # Add a new Date Reporting Format
    
    addEditReportTemplate(template_directory = "TestDirectory", 
                          template_file = "Filename",
                          title = "Report Title", 
                          title_size = "Large", 
                          is_signature_required = FALSE, 
                          is_active = TRUE,
                          logo_oid = 1,
                          event_user = 1)
    
    NewTemplate <- queryReportTemplate(oid = nrow(StartTemplate) + 1)
    
    expect_data_frame(NewTemplate, 
                      nrows = 1)
    
    
    # Edit an existing report format
    
    addEditReportTemplate(oid = NewTemplate$OID, 
                          template_directory = "TestDirectory2", 
                          template_file = "Filename2",
                          title = "Report Title2", 
                          title_size = "LARGE", 
                          is_signature_required = TRUE, 
                          is_active = FALSE,
                          logo_oid = 2,
                          event_user = 1)
    
    NewTemplate <- queryReportTemplate(oid = NewTemplate$OID)
    
    expect_data_frame(NewTemplate, 
                      nrows = 1)
    
    expect_equal(NewTemplate$TemplateDirectory, 
                 "TestDirectory2")
    expect_equal(NewTemplate$TemplateFile, 
                 "Filename2")
    expect_equal(NewTemplate$TitleSize, 
                 "LARGE")
  }
)

test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_template_oid <- max(queryReportTemplate()$OID)
    next_template_oid <- last_template_oid + 1
    
    addEditReportTemplate(template_directory = "TestDirectory2", 
                          template_file = "Filename2",
                          title = "Report Title2", 
                          title_size = "LARGE", 
                          is_signature_required = TRUE, 
                          is_active = FALSE,
                          logo_oid = 2,
                          event_user = 1)
    
    TemplateEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  "SELECT * FROM ReportTemplateEvent WHERE ParentReportTemplate = ?", 
                                  next_template_oid))
    
    expect_equal(TemplateEvent$EventType,
                 c("Add", "SetSignatureRequiredTrue", "Deactivate", 
                   "EditTemplateFolder", "EditTemplateFile", 
                   "EditTitle", "EditTitleSize", "EditLogoFile"))
    expect_true(all(table(TemplateEvent$EventType) == 1))
    
    addEditReportTemplate(oid = next_template_oid, 
                          template_directory = "TestDirectoryEdit", 
                          template_file = "FilenameEdit",
                          title = "Report Title Edit", 
                          title_size = "large", 
                          is_signature_required = FALSE, 
                          is_active = TRUE,
                          logo_oid = 1,
                          event_user = 1)
    
    
    TemplateEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   "SELECT * FROM ReportTemplateEvent WHERE ParentReportTemplate = ?", 
                                   next_template_oid))
    
    expect_true(
      all(table(TemplateEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "EditLogoFile" = 2, 
              "EditTemplateFile" = 2, 
              "EditTemplateFolder" = 2, 
              "EditTitle" = 2, 
              "EditTitleSize" = 2, 
              "SetSignatureRequiredFalse" = 1, 
              "SetSignatureRequiredTrue" = 1))
    )
    
    dbDisconnect(conn)
  }
)


unlink(temp_file)