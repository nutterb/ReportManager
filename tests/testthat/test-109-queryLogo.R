# Functionality - SQL Server ----------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "queryLogo returns a data frame of only Logos", 
    {
      skip_if_not(.ready, 
                  .message)
      
      FileArchive <- queryFileArchive()
      LogoArchive <- queryLogo()
      
      expect_data_frame(LogoArchive)
      
      expect_true(all(LogoArchive$IsLogo))
      
      expect_equal(nrow(LogoArchive), 
                   nrow(FileArchive[FileArchive$IsLogo,]))
    }
  )
}
