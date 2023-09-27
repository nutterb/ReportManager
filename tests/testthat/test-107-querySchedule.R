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

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "querySchedule functionality for SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    # Retrieve all schedules
    
    expect_data_frame(querySchedule())

    expect_equal(names(querySchedule()), 
                 c("OID", "ScheduleName", 
                   "Frequency", "FrequencyUnit", 
                   "OffsetOverlap", "OffsetOverlapUnit", 
                   "IsActive"))    
    
    # Retrive a single Schedule
    
    expect_data_frame(querySchedule(oid = 3), 
                      nrows = 1)
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "querySchedule functionality for SQL Server", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    # Retrieve all schedules
    
    expect_data_frame(querySchedule())
    
    expect_equal(names(querySchedule()), 
                 c("OID", "ScheduleName", 
                   "Frequency", "FrequencyUnit", 
                   "OffsetOverlap", "OffsetOverlapUnit", 
                   "IsActive"))    
    
    # Retrive a single Schedule
    
    expect_data_frame(querySchedule(oid = 3), 
                      nrows = 1)
  }
)