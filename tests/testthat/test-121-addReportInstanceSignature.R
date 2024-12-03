# Argument Validation -----------------------------------------------

test_that(
  "Return an error when report_instance_oid is not integerish(1)",
  {
    expect_error(
      addReportInstanceSignature(report_instance_oid = "1", 
                                 report_template_signature = 1,
                                 signed = TRUE, 
                                 event_user = 1, 
                                 signed_datetime = Sys.time()),
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1:2, 
                                 report_template_signature = 1,
                                 signed = TRUE, 
                                 event_user = 1, 
                                 signed_datetime = Sys.time()),
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error when report_template_signature is not integerish(1)",
  {
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1, 
                                 report_template_signature = "1",
                                 signed = TRUE, 
                                 event_user = 1, 
                                 signed_datetime = Sys.time()),
      "'report_template_signature': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1, 
                                 report_template_signature = 1:2,
                                 signed = TRUE, 
                                 event_user = 1, 
                                 signed_datetime = Sys.time()),
      "'report_template_signature': Must have length 1"
    )
  }
)

test_that(
  "Return an error when signed is not logical(1)",
  {
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1, 
                                 report_template_signature = 1,
                                 signed = "TRUE", 
                                 event_user = 1, 
                                 signed_datetime = Sys.time()),
      "'signed': Must be of type 'logical'"
    )
    
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1, 
                                 report_template_signature = 1,
                                 signed = c(FALSE, TRUE), 
                                 event_user = 1, 
                                 signed_datetime = Sys.time()),
      "'signed': Must have length 1"
    )
  }
)

test_that(
  "Return an error when event_user is not integerish(1)",
  {
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1, 
                                 report_template_signature = 1,
                                 signed = TRUE, 
                                 event_user = "1", 
                                 signed_datetime = Sys.time()),
      "'event_user': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1, 
                                 report_template_signature = 1,
                                 signed = TRUE, 
                                 event_user = 1:2, 
                                 signed_datetime = Sys.time()),
      "'event_user': Must have length 1"
    )
  }
)

test_that(
  "Return an error when signed_datetime is not POSIXct(1)",
  {
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1, 
                                 report_template_signature = 1,
                                 signed = TRUE, 
                                 event_user = 1, 
                                 signed_datetime = format(Sys.time())),
      "'signed_datetime': Must be of type 'POSIXct'"
    )
    
    expect_error(
      addReportInstanceSignature(report_instance_oid = 1, 
                                 report_template_signature = 1,
                                 signed = TRUE, 
                                 event_user = 1, 
                                 signed_datetime = rep(Sys.time(), 2)),
      "'signed_datetime': Must have length 1"
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
  
  Current <- queryReportInstanceSignature(report_instance_oid = 4)
  
  expect_true(sum(Current$IsSigned) == 0)
  
  addEditReportTemplateSignature(parent_report_template = 2, 
                                 parent_role = 1, 
                                 order = 1,
                                 event_user = 1)
  
  addReportInstanceSignature(report_instance_oid = 4, 
                             report_template_signature = 1,
                             signed = TRUE, 
                             event_user = 1)
  
  Signed <- queryReportInstanceSignature(report_instance_oid = 4)
  
  expect_data_frame(Signed, 
                    nrows = nrow(Current))
  
  expect_true(sum(Signed$IsSigned) == 1)
  
  addReportInstanceSignature(report_instance_oid = 4, 
                             report_template_signature = 1,
                             signed = FALSE, 
                             event_user = 1)
  
  Unsigned <- queryReportInstanceSignature(report_instance_oid = 4)
  
  expect_data_frame(Unsigned, 
                    nrows = nrow(Signed) + 1)
  
  expect_true(sum(Signed$IsSigned) == 1)
}
