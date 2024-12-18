# Argument Validation -----------------------------------------------

test_that(
  "Return an error if report_instance_generation_oid is not integerish(1)",
  {
    opt <- options()$width 
    options(width = 2000)
    expect_error(
      addReportInstanceGenerationRecipient(
        report_instance_generation_oid = "1", 
        user_oid = 1:3
      ), 
      "'report_instance_generation_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceGenerationRecipient(
        report_instance_generation_oid = 1:3, 
        user_oid = 1:3
      ), 
      "'report_instance_generation_oid': Must have length 1"
    )
    
    options(width = opt)
  }
)

test_that(
  "Return an error if parent_user is not integerish(1)",
  {
    expect_error(
      addReportInstanceGenerationRecipient(
        report_instance_generation_oid = 1, 
        user_oid = letters
      ), 
      "'user_oid': Must be of type 'integerish'"
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
    "Record a ReportInstnaceGenerationRecipient", 
    {
      conn <- connectToReportManager()
      on.exit({ DBI::dbDisconnect(conn) })
      
      Generation <- addReportInstanceGeneration(report_instance_oid = 1, 
                                                report_template_oid = 1, 
                                                start_date_time = Sys.time(), 
                                                end_date_time = Sys.time(), 
                                                report_format = "html", 
                                                include_data = FALSE, 
                                                is_preview = FALSE, 
                                                is_distributed = TRUE,
                                                is_archived = TRUE,
                                                is_submission = FALSE, 
                                                user_oid = 1)
      
      statement <- switch(flavor, 
                          "sql_server" = "SELECT * FROM dbo.ReportInstanceGenerationRecipient",
                          "SELECT * FROM ReportInstanceGenerationRecipient")
      
      Current <- DBI::dbGetQuery(conn, 
                                 statement)

      addReportInstanceGenerationRecipient(report_instance_generation_oid = Generation$OID, 
                                           user_oid = 1:2)
      
      New <- DBI::dbGetQuery(conn, 
                             statement)
      
      expect_equal(nrow(Current) + 2, 
                   nrow(New))
    }
  )
}
