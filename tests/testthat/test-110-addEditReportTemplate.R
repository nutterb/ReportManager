temp_file <- tempfile(fileext = ".txt")
writeLines("Testing addEditFileArchive", 
           temp_file)

# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(addEditReportTemplate(oid = "1", 
                                       template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE",
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplate(oid = 1:2, 
                                       template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when template_name is not character(1)",
  {
    expect_error(addEditReportTemplate(template_name = 123,
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_name': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_name = letters, 
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_name': Must have length 1")
    
    expect_error(addEditReportTemplate(template_name = randomVarchar(51),
                                       template_directory = "dir",
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_name': All elements must have at most 50")
  }
)

test_that(
  "Return an error when template_directory is not character(1)",
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = 123, 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_directory': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = letters, 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_directory': Must have length 1")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = randomVarchar(51), 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_directory': All elements must have at most 50")
  }
)

test_that(
  "Return an error when template_file is not character(1)",
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = 123, 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_file': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = letters, 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_file': Must have length 1")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = randomVarchar(51), 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'template_file': All elements must have at most 50")
  }
)

test_that(
  "Return an error when title is not character(1)",
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = 123, 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'title': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = letters, 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'title': Must have length 1")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = randomVarchar(201), 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'title': All elements must have at most 200")
  }
)

test_that(
  "Return an error if title_size is not an acceptable value", 
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "something or other", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'title_size': Must be element of set")
  }
)

test_that(
  "Return an error if include_toc is not logical(1)", 
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = "FALSE", 
                                       default_email = "",
                                       event_user = 1), 
                 "'include_toc': Must be of type 'logical'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = c(TRUE, FALSE), 
                                       default_email = "",
                                       event_user = 1), 
                 "'include_toc': Must have length 1")
  }
)

test_that(
  "Return an error when default_email is not character(1)",
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "Title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = 123,
                                       event_user = 1), 
                 "'default_email': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "Title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = letters,
                                       event_user = 1), 
                 "'default_email': Must have length 1")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "Title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = randomVarchar(1001),
                                       event_user = 1), 
                 "'default_email': All elements must have at most 1000")
  }
)

test_that(
  "Return an error if is_signature_required is not logical(1)", 
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       is_signature_required = "TRUE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1), 
                 "'is_signature_required': Must be of type 'logical'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       is_signature_required = c(TRUE, FALSE), 
                                       event_user = 1), 
                 "'is_signature_required': Must have length 1")
  }
)

test_that(
  "Return an error if is_active is not logical(1)", 
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       is_active = "TRUE", 
                                       event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       is_active = c(TRUE, FALSE), 
                                       event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if logo_oid is not integerish(0/1)", 
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1, 
                                       logo_oid = "1"), 
                 "'logo_oid': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = 1, 
                                       logo_oid = 1:2), 
                 "'logo_oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if date_reporting_format is not integerish(1)", 
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       date_reporting_format = "1",
                                       event_user = 1), 
                 "'date_reporting_format': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       date_reporting_format = 1:2,
                                       event_user = 1), 
                 "'date_reporting_format': Must have length 1")
  }
)

test_that(
  "Return an error when supporting_data_file is not character(1)",
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "Title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "email",
                                       supporting_data_file = 123,
                                       event_user = 1), 
                 "'supporting_data_file': Must be of type 'string'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "Title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "email",
                                       supporting_data_file = letters,
                                       event_user = 1), 
                 "'supporting_data_file': Must have length 1")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "Title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "email", 
                                       supporting_data_file = randomVarchar(151),
                                       event_user = 1), 
                 "'supporting_data_file': All elements must have at most 150")
  }
)

test_that(
  "Return an error if is_include_data is not logical(1)", 
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       is_active = TRUE, 
                                       is_include_data = "TRUE",
                                       event_user = 1), 
                 "'is_include_data': Must be of type 'logical'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       is_active = TRUE, 
                                       is_include_data = c(FALSE, TRUE),
                                       event_user = 1), 
                 "'is_include_data': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
                                       event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditReportTemplate(template_name = "name",
                                       template_directory = "dir", 
                                       template_file = "file", 
                                       title = "title", 
                                       title_size = "LARGE", 
                                       include_toc = FALSE, 
                                       default_email = "",
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
    addLogo(file_path = temp_file, 
            description = "This is a logo")
    addLogo(file_path = temp_file, 
            description = "This is another logo")
  }
  
  test_that(
    "addEditReportTemplate functionality for SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      StartTemplate <- queryReportTemplate()
      
      # Add a new Date Reporting Template
      
      addEditReportTemplate(template_name = "Test Template", 
                            template_directory = "TestDirectory", 
                            template_file = "Filename",
                            title = "Report Title", 
                            title_size = "Large", 
                            include_toc = FALSE, 
                            default_email = "some text",
                            is_signature_required = FALSE, 
                            is_active = TRUE,
                            logo_oid = 1,
                            event_user = 1)
      
      NewTemplate <- queryReportTemplate(oid = nrow(StartTemplate) + 1)
      
      expect_data_frame(NewTemplate, 
                        nrows = 1)
      
      
      # Edit an existing Report Template
      
      addEditReportTemplate(oid = NewTemplate$OID, 
                            template_name = "Test Template",
                            template_directory = "TestDirectory2", 
                            template_file = "Filename2",
                            title = "Report Title2", 
                            title_size = "LARGE", 
                            include_toc = TRUE, 
                            default_email = "A description of the report",
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
      expect_equal(NewTemplate$DefaultEmailText, 
                   "A description of the report")
      expect_equal(NewTemplate$IncludeTableOfContents, 
                   TRUE)
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      last_template_oid <- max(queryReportTemplate()$OID)
      next_template_oid <- last_template_oid + 1
      
      addEditReportTemplate(template_name = "Test Template 2", 
                            template_directory = "TestDirectory2", 
                            template_file = "Filename2",
                            title = "Report Title2", 
                            title_size = "LARGE", 
                            include_toc = TRUE, 
                            default_email = "email text start",
                            is_signature_required = TRUE, 
                            is_active = FALSE,
                            logo_oid = 2,
                            event_user = 1)
      
      TemplateEvent <- dbGetQuery(conn, 
                                  sqlInterpolate(
                                    conn,
                                    switch(flavor, 
                                           "sql_server" = "SELECT * FROM dbo.ReportTemplateEvent WHERE ParentReportTemplate = ?", 
                                           "SELECT * FROM ReportTemplateEvent WHERE ParentReportTemplate = ?"), 
                                    next_template_oid))
      
      expect_equal(TemplateEvent$EventType,
                   c("Add", "SetIncludeTocTrue", 
                     "SetSignatureRequiredTrue", "Deactivate", 
                     "EditTemplateFolder", "EditTemplateFile",
                     "EditTitle", "EditTitleSize", 
                     "EditDefaultEmailText", "EditLogoFile", 
                     "EditDateReportingFormat", 
                     "EditTemplateName", 
                     "EditSupportingDataFile", 
                     "EditIsIncludeData"))
      expect_true(all(table(TemplateEvent$EventType) == 1))
      
      addEditReportTemplate(oid = next_template_oid, 
                            template_name = "Test Template 2",
                            template_directory = "TestDirectoryEdit", 
                            template_file = "FilenameEdit",
                            title = "Report Title Edit", 
                            title_size = "large", 
                            include_toc = FALSE, 
                            default_email = "email text end",
                            is_signature_required = FALSE, 
                            is_active = TRUE,
                            logo_oid = 1,
                            event_user = 1)
      
      
      TemplateEvent2 <- dbGetQuery(conn, 
                                   sqlInterpolate(
                                     conn,
                                     switch(flavor, 
                                            "sql_server" = "SELECT * FROM dbo.ReportTemplateEvent WHERE ParentReportTemplate = ?", 
                                            "SELECT * FROM ReportTemplateEvent WHERE ParentReportTemplate = ?"),
                                     next_template_oid))
      
      expect_true(
        all(table(TemplateEvent2$EventType) ==
              c("Activate" = 1, 
                "Add" = 1, 
                "Deactivate" = 1, 
                "EditDateReportingFormat" = 1,
                "EditDefaultEmailText" = 2,
                "EditIsIncludeData" = 1,
                "EditLogoFile" = 2, 
                "EditSupportingDataFile" = 1,
                "EditTemplateFile" = 2, 
                "EditTemplateFolder" = 2, 
                "EditTemplateName" = 1,
                "EditTitle" = 2, 
                "EditTitleSize" = 2, 
                "SetIncludeTocFalse" = 1, 
                "SetIncludeTocTrue" = 1,
                "SetSignatureRequiredFalse" = 1, 
                "SetSignatureRequiredTrue" = 1))
      )
      
      dbDisconnect(conn)
     }
  )
} 

unlink(temp_file)
