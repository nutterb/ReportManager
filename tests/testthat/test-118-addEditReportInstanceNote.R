# Argument Validation -----------------------------------------------

test_that(
  "cast an error when report_instance_oid is not integerish(1)",
  {
    expect_error(addReportInstanceNote(report_instance_oid = "1", 
                                       parent_user = 1, 
                                       note = "text"), 
                 "'report_instance_oid': Must be of type 'integerish'")
    
    expect_error(addReportInstanceNote(report_instance_oid = 1:2, 
                                       parent_user = 1, 
                                       note = "text"), 
                 "'report_instance_oid': Must have length 1")
  }
)

test_that(
  "cast an error when parent_user is not integerish(1)",
  {
    expect_error(addReportInstanceNote(report_instance_oid = 1, 
                                       parent_user = "1", 
                                       note = "text"), 
                 "'parent_user': Must be of type 'integerish'")
    
    expect_error(addReportInstanceNote(report_instance_oid = 1, 
                                       parent_user = 1:2, 
                                       note = "text"), 
                 "'parent_user': Must have length 1")
  }
)

test_that(
  "cast an error when note is not character(1)",
  {
    expect_error(addReportInstanceNote(report_instance_oid = 1, 
                                       parent_user = 1, 
                                       note = 1), 
                 "'note': Must be of type 'character'")
    
    expect_error(addReportInstanceNote(report_instance_oid = 1, 
                                       parent_user = 1, 
                                       note = letters), 
                 "'note': Must have length 1")
  }
)

test_that(
  "cast an error when note_date_time is not POSIXct(1)",
  {
    expect_error(addReportInstanceNote(report_instance_oid = 1, 
                                       parent_user = 1, 
                                       note = "text", 
                                       note_date_time = "today"), 
                 "'note_date_time': Must be of type 'POSIXct'")
    
    expect_error(addReportInstanceNote(report_instance_oid = 1, 
                                       parent_user = 1, 
                                       note = "text", 
                                       note_date_time = rep(Sys.time(), 2)), 
                 "'note_date_time': Must have length 1")
  }
)

# Functionality -----------------------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  CurrentNotes <- queryReportInstanceNote(report_instance_oid = 1)
  
  addReportInstanceNote(report_instance_oid = 1, 
                        parent_user = 1, 
                        note = "Testing note addition")
  
  NewNotes <- queryReportInstanceNote(report_instance_oid = 1)
  
  expect_data_frame(NewNotes, 
                    nrows = nrow(CurrentNotes) + 1)
  
  expect_equal(NewNotes$Note, 
               "Testing note addition")
}
