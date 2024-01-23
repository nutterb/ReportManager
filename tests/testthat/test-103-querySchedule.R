# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(1)", 
  {
    expect_error(querySchedule(oid = "1"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(querySchedule(oid = 1:2), 
                 "'oid': Must have length <= 1")
  }
)

# Functionality - SQLite --------------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "querySchedule functionality for SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      # Retrieve all schedules
      
      expect_data_frame(querySchedule())
      
      expect_equal(names(querySchedule()), 
                   c("OID", "ScheduleName", 
                     "Frequency", "FrequencyUnit", 
                     "OffsetOverlap", "OffsetOverlapUnit", 
                     "IsActive", "IsPeriodToDate"))    
      
      # Retrive a single Schedule
      
      expect_data_frame(querySchedule(oid = 3), 
                        nrows = 1)
    }
  )
}
