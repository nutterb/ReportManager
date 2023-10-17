temp_file <- tempfile(fileext = ".txt")
writeLines("Testing addEditFileArchive", 
           temp_file)

# Argument Validation - addLogo -------------------------------------

test_that(
  "Return an error if description is not character(1)", 
  {
    expect_error(addLogo(description = 1, 
                         file_path = temp_file), 
                 "'description': Must be of type 'string'")
    
    expect_error(addLogo(description = letters, 
                                file_path = temp_file), 
                 "'description': Must have length 1")
    
    expect_error(addLogo(description = randomVarchar(251), 
                                file_path = temp_file), 
                 "'description': All elements must have at most 250")
  }
)

test_that(
  "Return an error if file_path is not character(1)", 
  {
    expect_error(addLogo(file_path = 123, 
                                file_name = "123"), 
                 "'file_path': Must be of type 'character'")
    
    expect_error(addLogo(file_path = rep(temp_file, 2)), 
                 "'file_path': Must have length 1")
    
    expect_error(addLogo(file_path = "not a file.txt"), 
                 "'file_path': File does not exist")
  }
)

test_that(
  "Return an error if file_name is not character(1)", 
  {
    expect_error(addLogo(file_name = 1, 
                                file_path = temp_file), 
                 "'file_name': Must be of type 'string'")
    
    expect_error(addLogo(file_name = letters, 
                                file_path = temp_file), 
                 "'file_name': Must have length 1")
    
    expect_error(addLogo(file_name = randomVarchar(251), 
                                file_path = temp_file), 
                 "'file_name': All elements must have at most 250")
  }
)

test_that(
  "Return an error if file_extension is not character(1)", 
  {
    expect_error(addLogo(file_extension = 1, 
                                file_path = temp_file), 
                 "'file_extension': Must be of type 'string'")
    
    expect_error(addLogo(file_extension = letters, 
                                file_path = temp_file), 
                 "'file_extension': Must have length 1")
    
    expect_error(addLogo(file_extension = randomVarchar(16), 
                                file_path = temp_file), 
                 "'file_extension': All elements must have at most 15")
  }
)


# Argument Validation - editLogo ------------------------------------

test_that(
  "Return and error if oid is not integerish(1)",
  {
    expect_error(editLogo(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(editLogo(oid = 1:2), 
                 "'oid': Must have length 1")
  }
)

test_that(
  "Return an error if description is not character(0/1)", 
  {
    expect_error(editLogo(oid = 1, 
                          description = 1), 
                 "'description': Must be of type 'string'")
    
    expect_error(editLogo(oid = 1, 
                          description = letters), 
                 "'description': Must have length 1")
    
    expect_error(editLogo(oid = 1, 
                          description = randomVarchar(251)), 
                 "'description': All elements must have at most 250")
  }
)

test_that(
  "Return an error if file_name is not character(0/1)", 
  {
    expect_error(editLogo(oid = 1, 
                          file_name = 1), 
                 "'file_name': Must be of type 'string'")
    
    expect_error(editLogo(oid = 1, 
                          file_name = letters), 
                 "'file_name': Must have length 1")
    
    expect_error(editLogo(oid = 1, 
                          file_name = randomVarchar(251)), 
                 "'file_name': All elements must have at most 250")
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
}

test_that(
  "Add a Logo to the FileArchive", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    FileArchive <- queryFileArchive()
    
    next_oid <- if (nrow(FileArchive) == 0) 1 else max(FileArchive$OID) + 1
    
    addLogo(file_path = temp_file, 
            description = "This is a logo")
    
    NewLogo <- queryFileArchive(oid = next_oid)
    
    expect_data_frame(NewLogo, 
                      nrows = 1)
    
    expect_equal("This is a logo", 
                 NewLogo$Description)
    
    expect_true(NewLogo$IsLogo)
  }
)

test_that(
  "Edit an existing Logo", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    FileArchive <- queryFileArchive()
    LogoArchive <- FileArchive[FileArchive$IsLogo, ]
    
    editLogo(oid = LogoArchive$OID[1], 
             description = "Edit the logo description", 
             file_name = "newFileName")
    
    expect_equal(queryFileArchive(oid = LogoArchive$OID[1])$FileName, 
                 "newFileName")
    
    expect_equal(queryFileArchive(oid = LogoArchive$OID[1])$Description, 
                 "Edit the logo description")
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
}

test_that(
  "Add a Logo to the FileArchive", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    FileArchive <- queryFileArchive()
    
    next_oid <- if (nrow(FileArchive) == 0) 1 else max(FileArchive$OID) + 1
    
    addLogo(file_path = temp_file, 
            description = "This is a logo")
    
    NewLogo <- queryFileArchive(oid = next_oid)
    
    expect_data_frame(NewLogo, 
                      nrows = 1)
    
    expect_equal("This is a logo", 
                 NewLogo$Description)
    
    expect_true(NewLogo$IsLogo)
  }
)

test_that(
  "Edit an existing Logo", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    FileArchive <- queryFileArchive()
    LogoArchive <- FileArchive[FileArchive$IsLogo, ]
    
    editLogo(oid = LogoArchive$OID[1], 
             description = "Edit the logo description", 
             file_name = "newFileName")
    
    expect_equal(queryFileArchive(oid = LogoArchive$OID[1])$FileName, 
                 "newFileName")
    
    expect_equal(queryFileArchive(oid = LogoArchive$OID[1])$Description, 
                 "Edit the logo description")
  }
)
