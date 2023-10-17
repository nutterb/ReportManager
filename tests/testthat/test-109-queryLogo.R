# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "queryLogo returns a data frame of only Logos", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    FileArchive <- queryFileArchive()
    LogoArchive <- queryLogo()
    
    expect_data_frame(LogoArchive)
    
    expect_true(all(LogoArchive$IsLogo))
    
    expect_equal(nrow(LogoArchive), 
                 nrow(FileArchive[FileArchive$IsLogo,]))
  }
)

# Functionality - SQLite --------------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "queryLogo returns a data frame of only Logos", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    FileArchive <- queryFileArchive()
    LogoArchive <- queryLogo()
    
    expect_data_frame(LogoArchive)
    
    expect_true(all(LogoArchive$IsLogo))
    
    expect_equal(nrow(LogoArchive), 
                 nrow(FileArchive[FileArchive$IsLogo,]))
  }
)
