# Argument Validation -----------------------------------------------

test_that(
  "Return an error when report_instance_oid is not integerish(1)", 
  {
    expect_error(
      makeReportFileName(report_instance_oid = "1", 
                         is_preview = TRUE, 
                         is_submission = FALSE, 
                         file_extension = "zip"), 
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      makeReportFileName(report_instance_oid = 1:2, 
                         is_preview = TRUE, 
                         is_submission = FALSE, 
                         file_extension = "zip"), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error when is_preview is not logical(1)", 
  {
    expect_error(
      makeReportFileName(report_instance_oid = 1, 
                         is_preview = "TRUE", 
                         is_submission = FALSE, 
                         file_extension = "zip"), 
      "'is_preview': Must be of type 'logical'"
    )
    
    expect_error(
      makeReportFileName(report_instance_oid = 1, 
                         is_preview = c(FALSE, TRUE), 
                         is_submission = FALSE, 
                         file_extension = "zip"), 
      "'is_preview': Must have length 1"
    )
  }
)

test_that(
  "Return an error when is_submission is not logical(1)", 
  {
    expect_error(
      makeReportFileName(report_instance_oid = 1, 
                         is_preview = TRUE, 
                         is_submission = "FALSE", 
                         file_extension = "zip"), 
      "'is_submission': Must be of type 'logical'"
    )
    
    expect_error(
      makeReportFileName(report_instance_oid = 1, 
                         is_preview = TRUE, 
                         is_submission = c(FALSE, TRUE),
                         file_extension = "zip"), 
      "'is_submission': Must have length 1"
    )
  }
)

test_that(
  "Return an error if file_extension is not an accepted value",
  {
    expect_error(
      makeReportFileName(report_instance_oid = 1, 
                         is_preview = TRUE, 
                         is_submission = FALSE, 
                         file_extension = "not this"), 
      "'file_extension': Must be element of set"
    )
  }
)

# Functionality -----------------------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  test_that(
    "Test options for previews", 
    {
      skip_if_not(.ready, 
                  .message)
      
      filename <- makeReportFileName(report_instance_oid = 1, 
                                     is_preview = TRUE, 
                                     is_submission = FALSE, 
                                     file_extension = "zip")
      
      expect_true(grepl("\\d{4}-\\d{2}-\\d{2} - \\d{4}-\\d{2}-\\d{2}-Preview-ReportTemplate2.zip", 
                        filename))
      
      
      filename <- makeReportFileName(report_instance_oid = 1, 
                                     is_preview = FALSE, 
                                     is_submission = FALSE, 
                                     file_extension = "zip")
      
      expect_true(grepl("\\d{4}-\\d{2}-\\d{2} - \\d{4}-\\d{2}-\\d{2}-ReportTemplate2.zip", 
                        filename))
      
      
      filename <- makeReportFileName(report_instance_oid = 1, 
                                     is_preview = TRUE, 
                                     is_submission = FALSE, 
                                     file_extension = "html")
      
      expect_true(grepl("\\d{4}-\\d{2}-\\d{2} - \\d{4}-\\d{2}-\\d{2}-Preview-ReportTemplate2.html", 
                        filename))
      
      
      filename <- makeReportFileName(report_instance_oid = 1, 
                                     is_preview = TRUE, 
                                     is_submission = FALSE, 
                                     file_extension = "pdf")
      
      expect_true(grepl("\\d{4}-\\d{2}-\\d{2} - \\d{4}-\\d{2}-\\d{2}-Preview-ReportTemplate2.pdf", 
                        filename))
      
    }
  )
  
  test_that(
    "Test options for submissions", 
    {
      skip("Tests should be developed after incorporating revision numbers")
      
      filename <- makeReportFileName(report_instance_oid = 1, 
                                     is_preview = FALSE, 
                                     is_submission = TRUE, 
                                     file_extension = "pdf")
      
      expect_true(grepl("\\d{4}-\\d{2}-\\d{2} - \\d{4}-\\d{2}-\\d{2}-ReportTemplate2[RevNumber].pdf", 
                        filename))
    }
  )
}