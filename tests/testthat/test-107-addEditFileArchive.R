temp_file <- tempfile(fileext = ".txt")
writeLines("Testing addEditFileArchive", 
           temp_file)

# Argument Validation - addFileLogo ---------------------------------

test_that(
  "Return an error if parent_report_template is not numeric(1)", 
  {
    expect_error(addFileArchive(parent_report_template = "1", 
                                file_path = temp_file), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(addFileArchive(parent_report_instance = 1:2, 
                                file_path = temp_file), 
                 "'parent_report_instance': Must have length 1")
  }
)

test_that(
  "Return an error if parent_report_instance is not numeric(1)", 
  {
    expect_error(addFileArchive(parent_report_instance = "1", 
                                file_path = temp_file), 
                 "'parent_report_instance': Must be of type 'integerish'")
    
    expect_error(addFileArchive(parent_report_instance = 1:2, 
                                file_path = temp_file), 
                 "'parent_report_instance': Must have length 1")
  }
)

test_that(
  "Return an error if description is not character(1)", 
  {
    expect_error(addFileArchive(description = 1, 
                                file_path = temp_file), 
                 "'description': Must be of type 'string'")
    
    expect_error(addFileArchive(description = letters, 
                                file_path = temp_file), 
                 "'description': Must have length 1")
    
    expect_error(addFileArchive(description = randomVarchar(251), 
                                file_path = temp_file), 
                 "'description': All elements must have at most 250")
  }
)

test_that(
  "Return an error if is_logo is not logical(1)", 
  {
    expect_error(addFileArchive(is_logo = "TRUE", 
                                file_path = temp_file), 
                 "'is_logo': Must be of type 'logical'")
    
    expect_error(addFileArchive(is_logo = c(TRUE, FALSE), 
                                file_path = temp_file), 
                 "'is_logo': Must have length 1")
  }
)

test_that(
  "Return an error if file_path is not character(1)", 
  {
    expect_error(addFileArchive(file_path = 123, 
                                file_name = "123"), 
                 "'file_path': Must be of type 'character'")
    
    expect_error(addFileArchive(file_path = rep(temp_file, 2)), 
                 "'file_path': Must have length 1")
    
    expect_error(addFileArchive(file_path = "not a file.txt"), 
                 "'file_path': File does not exist")
  }
)

test_that(
  "Return an error if file_name is not character(1)", 
  {
    expect_error(addFileArchive(file_name = 1, 
                                file_path = temp_file), 
                 "'file_name': Must be of type 'string'")
    
    expect_error(addFileArchive(file_name = letters, 
                                file_path = temp_file), 
                 "'file_name': Must have length 1")
    
    expect_error(addFileArchive(file_name = randomVarchar(251), 
                                file_path = temp_file), 
                 "'file_name': All elements must have at most 250")
  }
)

test_that(
  "Return an error if file_extension is not character(1)", 
  {
    expect_error(addFileArchive(file_extension = 1, 
                                file_path = temp_file), 
                 "'file_extension': Must be of type 'string'")
    
    expect_error(addFileArchive(file_extension = letters, 
                                file_path = temp_file), 
                 "'file_extension': Must have length 1")
    
    expect_error(addFileArchive(file_extension = randomVarchar(16), 
                                file_path = temp_file), 
                 "'file_extension': All elements must have at most 15")
  }
)


# Argument Validation - editFileArchive -----------------------------

test_that(
  "Return an error if parent_report_template is not numeric(0/1)", 
  {
    expect_error(editFileArchive(oid = 1, 
                                 parent_report_template = "1"), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(editFileArchive(oid = 1, 
                                 parent_report_instance = 1:2), 
                 "'parent_report_instance': Must have length <= 1")
  }
)

test_that(
  "Return an error if parent_report_instance is not numeric(1)", 
  {
    expect_error(editFileArchive(oid = 1, 
                                 parent_report_instance = "1"), 
                 "'parent_report_instance': Must be of type 'integerish'")
    
    expect_error(editFileArchive(oid = 1, 
                                 parent_report_instance = 1:2), 
                 "'parent_report_instance': Must have length <= 1")
  }
)

test_that(
  "Return an error if description is not character(1)", 
  {
    expect_error(editFileArchive(oid = 1, 
                                 description = 1), 
                 "'description': Must be of type 'character'")
    
    expect_error(editFileArchive(oid = 1, 
                                 description = letters), 
                 "'description': Must have length <= 1")
    
    expect_error(editFileArchive(oid = 1, 
                                 description = randomVarchar(251)), 
                 "'description': All elements must have at most 250")
  }
)

test_that(
  "Return an error if is_logo is not logical(1)", 
  {
    expect_error(editFileArchive(oid = 1, 
                                 is_logo = "TRUE"), 
                 "'is_logo': Must be of type 'logical'")
    
    expect_error(editFileArchive(oid = 1, 
                                 is_logo = c(TRUE, FALSE)), 
                 "'is_logo': Must have length <= 1")
  }
)

test_that(
  "Return an error if file_name is not character(0/1)", 
  {
    expect_error(editFileArchive(oid = 1, 
                                 file_name = 1), 
                 "'file_name': Must be of type 'character'")
    
    expect_error(editFileArchive(oid = 1, 
                                 file_name = letters), 
                 "'file_name': Must have length <= 1")
    
    expect_error(editFileArchive(oid = 1, 
                                 file_name = randomVarchar(251)), 
                 "'file_name': All elements must have at most 250")
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
    "Unassociated File can be added to the FileArchive", 
    {
      skip_if_not(.ready, 
                  .message)
      
      FileArchive <- queryFileArchive()
      
      next_oid <- if (nrow(FileArchive) == 0) 1 else max(FileArchive$OID) + 1
      
      addFileArchive(file_path = temp_file, 
                     description = "This is a file")
      
      NewArchive <- queryFileArchive(oid = next_oid)
      
      expect_data_frame(NewArchive, 
                        nrows = 1)
      
      expect_equal("This is a file", 
                   NewArchive$Description)
      
      expect_false(NewArchive$IsLogo)
    }
  )
  
  test_that(
    "Associated file can be added to the File Archive", 
    {
      skip_if_not(.ready, 
                  .message)
      
      FileArchive <- queryFileArchive()
      
      next_oid <- if (nrow(FileArchive) == 0) 1 else max(FileArchive$OID) + 1
      
      addFileArchive(parent_report_template = 1, 
                     parent_report_instance = 1,
                     file_path = temp_file, 
                     is_logo = TRUE,
                     description = "This is a file")
      
      NewArchive <- queryFileArchive(oid = next_oid)
      
      expect_data_frame(NewArchive, 
                        nrows = 1)
      
      expect_equal("This is a file", 
                   NewArchive$Description)
      
      expect_true(NewArchive$IsLogo)
      
      expect_equal(NewArchive$ParentReportTemplate, 1)
      expect_equal(NewArchive$ParentReportInstance, 1)
    }
  )
  
  test_that(
    "Edit an existing FileArchive", 
    {
      skip_if_not(.ready, 
                  .message)
      
      editFileArchive(oid = 1, 
                      file_name = "New file name")
      
      expect_equal(queryFileArchive(oid = 1)$FileName, 
                   "New file name")
    }
  )
}
