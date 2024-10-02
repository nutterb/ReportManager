for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "Return the appropriate data frames", 
    {
      skip_if_not(.ready, 
                  .message)
      
      expect_data_frame(queryReportSelection())
    }
  )
}
