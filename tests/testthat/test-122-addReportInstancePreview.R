# Argument valdiation -----------------------------------------------

test_that(
  "Return an error when report_instance_oid is not integerish(1)", 
  {
    expect_error(
      addReportInstancePreview(report_instance_oid = "1", 
                               include_data = TRUE, 
                               preview_user = 1, 
                               preview_type = "preview", 
                               preview_datetime = Sys.time()), 
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstancePreview(report_instance_oid = 1:2, 
                               include_data = TRUE, 
                               preview_user = 1, 
                               preview_type = "preview", 
                               preview_datetime = Sys.time()), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error when include_data is not logical(1)", 
  {
    expect_error(
      addReportInstancePreview(report_instance_oid = 1, 
                               include_data = "TRUE", 
                               preview_user = 1, 
                               preview_type = "preview", 
                               preview_datetime = Sys.time()), 
      "'include_data': Must be of type 'logical'"
    )
    
    expect_error(
      addReportInstancePreview(report_instance_oid = 1, 
                               include_data = c(FALSE, TRUE), 
                               preview_user = 1, 
                               preview_type = "preview", 
                               preview_datetime = Sys.time()), 
      "'include_data': Must have length 1"
    )
  }
)

test_that(
  "Return an error when preview_user is not integerish(1)", 
  {
    expect_error(
      addReportInstancePreview(report_instance_oid = 1, 
                               include_data = TRUE, 
                               preview_user = "1", 
                               preview_type = "preview", 
                               preview_datetime = Sys.time()), 
      "'preview_user': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstancePreview(report_instance_oid = 1, 
                               include_data = TRUE, 
                               preview_user = 1:2, 
                               preview_type = "preview", 
                               preview_datetime = Sys.time()), 
      "'preview_user': Must have length 1"
    )
  }
)

test_that(
  "Return an error when preview_type is not one of....", 
  {
    expect_error(
      addReportInstancePreview(report_instance_oid = 1, 
                               include_data = TRUE, 
                               preview_user = 1, 
                               preview_type = "nope", 
                               preview_datetime = Sys.time()), 
      "'preview_type': Must be element of set"
    )
  }
)

test_that(
  "Return an error if preview_datetime is not POSIXct(1)", 
  {
    expect_error(
      addReportInstancePreview(report_instance_oid = 1, 
                               include_data = TRUE, 
                               preview_user = 1, 
                               preview_type = "preview",
                               preview_datetime = "2024-01-01 00:00:00"), 
      "'preview_datetime': Must be of type 'POSIXct'"
    )
    
    expect_error(
      addReportInstancePreview(report_instance_oid = 1, 
                               include_data = TRUE, 
                               preview_user = 1, 
                               preview_type = "preview",
                               preview_datetime = rep(Sys.time(), 2)), 
      "'preview_datetime': Must have length 1"
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
  
  conn <- connectToReportManager()
  
  CurrentPreview <- DBI::dbGetQuery(conn, "SELECT * FROM ReportInstancePreview")
  
  expect_no_error(
    addReportInstancePreview(report_instance_oid = 1, 
                             include_data = FALSE, 
                             preview_user = 1)
  )
  
  NewPreview <- DBI::dbGetQuery(conn, "SELECT * FROM ReportInstancePreview")
  
  expect_equal(nrow(CurrentPreview) + 1,
               nrow(NewPreview))
  
  
  dbDisconnect(conn)
}
