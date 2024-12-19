# Argument Validation -----------------------------------------------

test_that(
  "Return an error if report_instance_oid is not integerish(1)", 
  {
    expect_error(
      addReportInstanceRevision(report_instance_oid = "1", 
                                parent_user = 1, 
                                revision_date_time = Sys.time(), 
                                reason = "reason"), 
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceRevision(report_instance_oid = 1:2, 
                                parent_user = 1, 
                                revision_date_time = Sys.time(), 
                                reason = "reason"), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error if parent_user is not integerish(1)", 
  {
    expect_error(
      addReportInstanceRevision(report_instance_oid = 1, 
                                parent_user = "1", 
                                revision_date_time = Sys.time(), 
                                reason = "reason"), 
      "'parent_user': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceRevision(report_instance_oid = 1, 
                                parent_user = 1:2, 
                                revision_date_time = Sys.time(), 
                                reason = "reason"), 
      "'parent_user': Must have length 1"
    )
  }
)

test_that(
  "Return an error if revision_date_time is not POSIXct(1)", 
  {
    expect_error(
      addReportInstanceRevision(report_instance_oid = 1, 
                                parent_user = 1, 
                                revision_date_time = format(Sys.time()), 
                                reason = "reason"), 
      "'revision_date_time': Must be of type 'POSIXct'"
    )
    
    expect_error(
      addReportInstanceRevision(report_instance_oid = 1, 
                                parent_user = 1, 
                                revision_date_time = rep(Sys.time(), 2), 
                                reason = "reason"), 
      "'revision_date_time': Must have length 1"
    )
  }
)

test_that(
  "Return an error if reason is not character(1)", 
  {
    expect_error(
      addReportInstanceRevision(report_instance_oid = 1, 
                                parent_user = 1, 
                                revision_date_time = Sys.time(), 
                                reason = TRUE), 
      "'reason': Must be of type 'character'"
    )
    
    expect_error(
      addReportInstanceRevision(report_instance_oid = 1, 
                                parent_user = 1, 
                                revision_date_time = Sys.time(), 
                                reason = letters), 
      "'reason': Must have length 1"
    )
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
  
  test_that(
    "Record a ReportInstanceRevision", 
    {
      conn <- connectToReportManager()
      on.exit({ DBI::dbDisconnect(conn) })
      
      statement <- switch(flavor, 
                          "sql_server" = "SELECT * FROM dbo.ReportInstanceRevision",
                          "SELECT * FROM ReportInstanceRevision")
      
      Current <- DBI::dbGetQuery(conn, 
                                 statement)
      
      addReportInstanceRevision(report_instance_oid = 1, 
                                parent_user = 1, 
                                revision_date_time = Sys.time(), 
                                reason = "example reason")
      
      New <- DBI::dbGetQuery(conn, 
                             statement)
      
      expect_equal(nrow(Current) + 1, 
                   nrow(New))
    }
  )
}
