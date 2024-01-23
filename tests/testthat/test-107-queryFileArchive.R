# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(queryFileArchive(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(queryFileArchive(oid = 1:2), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_template is not integerish(0/1)", 
  {
    expect_error(queryFileArchive(parent_report_template = "1"), 
                 "'parent_report_template': Must be of type 'integerish'")
    
    expect_error(queryFileArchive(parent_report_template = 1:2), 
                 "'parent_report_template': Must have length <= 1")
  }
)

test_that(
  "Return an error when parent_report_instance is not integerish(0/1)", 
  {
    expect_error(queryFileArchive(parent_report_instance = "1"), 
                 "'parent_report_instance': Must be of type 'integerish'")
    
    expect_error(queryFileArchive(parent_report_instance = 1:2), 
                 "'parent_report_instance': Must have length <= 1")
  }
)

# Functionality - SQL Server ----------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "query FileArchive under SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      FileArchive <- queryFileArchive()
      expect_data_frame(FileArchive)
      expect_true(nrow(FileArchive) > 0)
      
      FileArchive <- queryFileArchive(oid = 1)
      expect_data_frame(FileArchive, 
                        nrows = 1)
      
      FileArchive <- queryFileArchive(parent_report_template = 1)
      expect_data_frame(FileArchive)
      expect_true(nrow(FileArchive) > 0)
      
      FileArchive <- queryFileArchive(parent_report_instance = 1)
      expect_data_frame(FileArchive)
      expect_true(nrow(FileArchive) > 0)
      
      FileArchive <- queryFileArchive(parent_report_template = 1, 
                                      parent_report_instance = 1)
      expect_data_frame(FileArchive)
      expect_true(nrow(FileArchive) > 0)
    }
  )
}
