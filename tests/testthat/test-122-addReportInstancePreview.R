# Argument valdiation -----------------------------------------------

test_that(
  "Return an error when report_instance_oid is not integerish(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = "1",
                               report_template_oid = 1,
                               start_date_time = Sys.time(),
                               end_date_time = Sys.time(),
                               report_format = "preview", 
                               include_data = TRUE, 
                               is_preview = TRUE, 
                               is_distributed = FALSE, 
                               is_archived = FALSE, 
                               is_submission = FALSE,
                               user_oid = 1,
                               preview_date_time = Sys.time()),
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1:2,
                               report_template_oid = 1,
                               start_date_time = Sys.time(),
                               end_date_time = Sys.time(),
                               report_format = "preview", 
                               include_data = TRUE, 
                               is_preview = TRUE, 
                               is_distributed = FALSE, 
                               is_archived = FALSE, 
                               is_submission = FALSE,
                               user_oid = 1,
                               preview_date_time = Sys.time()), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error when report_template_oid is not integerish(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                               report_template_oid = "1",
                               start_date_time = Sys.time(),
                               end_date_time = Sys.time(),
                               report_format = "preview", 
                               include_data = TRUE, 
                               is_preview = TRUE, 
                               is_distributed = FALSE, 
                               is_archived = FALSE, 
                               is_submission = FALSE,
                               user_oid = 1,
                               preview_date_time = Sys.time()),
      "'report_template_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                               report_template_oid = 1:2,
                               start_date_time = Sys.time(),
                               end_date_time = Sys.time(),
                               report_format = "preview", 
                               include_data = TRUE, 
                               is_preview = TRUE, 
                               is_distributed = FALSE, 
                               is_archived = FALSE, 
                               is_submission = FALSE,
                               user_oid = 1,
                               preview_date_time = Sys.time()), 
      "'report_template_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error when start_date_time is not POSIXct(1)",
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = as.character(Sys.time()),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'start_date_time': Must be of type 'POSIXct'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = rep(Sys.time(), 2),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'start_date_time': Must have length 1"
    )
  }
)

test_that(
  "Return an error when end_date_time is not POSIXct(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = as.character(Sys.time()),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'end_date_time': Must be of type 'POSIXct'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = rep(Sys.time(), 2),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'end_date_time': Must have length 1"
    )
  }
)

test_that(
  "Return an error when report_format is not one of....", 
  {
    expect_error(
      addReportInstanceGeneration(addReportInstanceGeneration(report_instance_oid = 1,
                                                           report_template_oid = 1,
                                                           start_date_time = Sys.time(),
                                                           end_date_time = Sys.time(),
                                                           report_format = "nope", 
                                                           include_data = TRUE, 
                                                           is_preview = TRUE, 
                                                           is_distributed = FALSE, 
                                                           is_archived = FALSE, 
                                                           is_submission = FALSE,
                                                           user_oid = 1,
                                                           preview_date_time = Sys.time())), 
      "'report_format': Must be element of set"
    )
  }
)

test_that(
  "Return an error when include_data is not logical(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = "TRUE", 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'include_data': Must be of type 'logical'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = c(FALSE, TRUE), 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'include_data': Must have length 1"
    )
  }
)

test_that(
  "Return an error if is_preview is not logical(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = "TRUE", 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'is_preview': Must be of type 'logical'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = c(FALSE, TRUE), 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'is_preview': Must have length 1"
    )
  }
)

test_that(
  "Return an error if is_distributed is not logical(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = "FALSE", 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'is_distributed': Must be of type 'logical'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = c(FALSE, TRUE), 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'is_distributed': Must have length 1"
    )
  }
)

test_that(
  "Return an error if is_archived is not logical(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = "FALSE", 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'is_archived': Must be of type 'logical'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = c(FALSE, TRUE), 
                                  is_submission = FALSE,
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'is_archived': Must have length 1"
    )
  }
)

test_that(
  "Return an error if is_submission is not logical(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = "FALSE",
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'is_submission': Must be of type 'logical'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = c(FALSE, TRUE),
                                  user_oid = 1,
                                  preview_date_time = Sys.time()), 
      "'is_submission': Must have length 1"
    )
  }
)

test_that(
  "Return an error if user_oid is not integerish(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = "1",
                                  preview_date_time = Sys.time()), 
      "'user_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                                  report_template_oid = 1,
                                  start_date_time = Sys.time(),
                                  end_date_time = Sys.time(),
                                  report_format = "preview", 
                                  include_data = TRUE, 
                                  is_preview = TRUE, 
                                  is_distributed = FALSE, 
                                  is_archived = FALSE, 
                                  is_submission = FALSE,
                                  user_oid = 1:2,
                                  preview_date_time = Sys.time()), 
      "'user_oid': Must have length 1"
    )
  }
)

test_that(
  "Return an error if preview_date_time is not POSIXct(1)", 
  {
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                               report_template_oid = 1,
                               start_date_time = Sys.time(),
                               end_date_time = Sys.time(),
                               report_format = "preview", 
                               include_data = TRUE, 
                               is_preview = TRUE, 
                               is_distributed = FALSE, 
                               is_archived = FALSE, 
                               is_submission = FALSE,
                               user_oid = 1,
                               preview_date_time = as.character(Sys.time())), 
      "'preview_date_time': Must be of type 'POSIXct'"
    )
    
    expect_error(
      addReportInstanceGeneration(report_instance_oid = 1,
                               report_template_oid = 1,
                               start_date_time = Sys.time(),
                               end_date_time = Sys.time(),
                               report_format = "preview", 
                               include_data = TRUE, 
                               is_preview = TRUE, 
                               is_distributed = FALSE, 
                               is_archived = FALSE, 
                               is_submission = FALSE,
                               user_oid = 1,
                               preview_date_time = rep(Sys.time(), 2)), 
      "'preview_date_time': Must have length 1"
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
  
  CurrentPreview <- DBI::dbGetQuery(conn, "SELECT * FROM ReportInstanceGeneration")
  
  expect_no_error(
    addReportInstanceGeneration(report_instance_oid = 1,
                                report_template_oid = 1,
                                start_date_time = Sys.time(),
                                end_date_time = Sys.time(),
                                report_format = "preview", 
                                include_data = TRUE, 
                                is_preview = TRUE, 
                                is_distributed = FALSE, 
                                is_archived = FALSE, 
                                is_submission = FALSE,
                                user_oid = 1,
                                preview_date_time = Sys.time())
  )
  
  NewPreview <- DBI::dbGetQuery(conn, "SELECT * FROM ReportInstanceGeneration")
  
  expect_equal(nrow(CurrentPreview) + 1,
               nrow(NewPreview))
  
  
  dbDisconnect(conn)
}
