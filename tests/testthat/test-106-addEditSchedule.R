# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not character(0/1)", 
  {
    expect_error(addEditSchedule(oid = "1", 
                                 schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditSchedule(oid = c(1, 2), 
                                 schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)


test_that(
  "Return an error when schedule_name is not character(1)", 
  {
    expect_error(addEditSchedule(schedule_name = 123, 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'schedule_name': Must be of type 'character'")
    
    expect_error(addEditSchedule(schedule_name = c("Schedule", "Name"), 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'schedule_name': Must have length 1")
  }
)


test_that(
  "Return an error when frequency is not integerish(1)", 
  {
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = "1", 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'frequency': Must be of type 'integerish'")
    
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1:2, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'frequency': Must have length 1")
  }
)


test_that(
  "Return an error when frequency_unit is not an acceptable value", 
  {
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Not a time unit", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'frequency_unit': Must be element of set")
  }
)


test_that(
  "Return an error when offset_overlap is not integerish(1)", 
  {
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = "0", 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'offset_overlap': Must be of type 'integerish'")
    
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0:1, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'offset_overlap': Must have length 1")
  }
)


test_that(
  "Return an error when offset_overlap_unit is not an acceptable value", 
  {
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Not a time unit", 
                                 is_active = TRUE, 
                                 event_user = 1), 
                 "'offset_overlap_unit': Must be element of set")
  }
)


test_that(
  "Return an error when is_active is not logical(1)", 
  {
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = "TRUE", 
                                 event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = c(TRUE, FALSE), 
                                 event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error when event_user is not integerish(1)", 
  {
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditSchedule(schedule_name = "Schedule", 
                                 frequency = 1, 
                                 frequency_unit = "Day", 
                                 offset_overlap = 0, 
                                 offset_overlap_unit = "Day", 
                                 is_active = TRUE, 
                                 event_user = 1:2), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "addEditSchedule functionality for SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    CurrentSchedule <- querySchedule()
    
    addEditSchedule(schedule_name = "Another Schedule", 
                    frequency = 7, 
                    frequency_unit = "Day", 
                    offset_overlap = 14, 
                    offset_overlap_unit = "Day", 
                    event_user = 1)
    
    NewSchedule <- querySchedule(oid = nrow(CurrentSchedule) + 1)
    
    expect_data_frame(NewSchedule, 
                      nrows = 1)
    
    # Edit the schedule
    
    addEditSchedule(oid = NewSchedule$OID, 
                    schedule_name = "Rename the schedule", 
                    frequency = 7, 
                    frequency_unit = "Day", 
                    offset_overlap = 14, 
                    offset_overlap_unit = "Day", 
                    event_user = 1)
    
    NewSchedule <- querySchedule(oid = NewSchedule$OID)
    
    expect_equal(NewSchedule$ScheduleName, 
                 "Rename the schedule")
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sqlite")

test_that(
  "addEditSchedule functionality for SQLite", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    CurrentSchedule <- querySchedule()
    
    addEditSchedule(schedule_name = "Another Schedule", 
                    frequency = 7, 
                    frequency_unit = "Day", 
                    offset_overlap = 14, 
                    offset_overlap_unit = "Day", 
                    event_user = 1)
    
    NewSchedule <- querySchedule(oid = nrow(CurrentSchedule) + 1)
    
    expect_data_frame(NewSchedule, 
                      nrows = 1)
    
    # Edit the schedule
    
    addEditSchedule(oid = NewSchedule$OID, 
                    schedule_name = "Rename the schedule", 
                    frequency = 7, 
                    frequency_unit = "Day", 
                    offset_overlap = 14, 
                    offset_overlap_unit = "Day", 
                    event_user = 1)
    
    NewSchedule <- querySchedule(oid = NewSchedule$OID)
    
    expect_equal(NewSchedule$ScheduleName, 
                 "Rename the schedule")
  }
)
