# Argument Validation -----------------------------------------------

test_that(
  "report_instance_oid is integerish(1)", 
  {
    expect_error(
      updateInstanceIsSubmitted(
        report_instance_oid = "1", 
        is_submitted = FALSE, 
        current_user_oid = 1, 
        event_date_time = Sys.time()
      ), 
      "'report_instance_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      updateInstanceIsSubmitted(
        report_instance_oid = 1:2, 
        is_submitted = FALSE, 
        current_user_oid = 1, 
        event_date_time = Sys.time()
      ), 
      "'report_instance_oid': Must have length 1"
    )
  }
)

test_that(
  "is_submitted must be logical(1)", 
  {
    expect_error(
      updateInstanceIsSubmitted(
        report_instance_oid = 1, 
        is_submitted = "FALSE", 
        current_user_oid = 1, 
        event_date_time = Sys.time()
      ), 
      "'is_submitted': Must be of type 'logical'"
    )
    
    expect_error(
      updateInstanceIsSubmitted(
        report_instance_oid = 1, 
        is_submitted = c(FALSE, TRUE),
        current_user_oid = 1, 
        event_date_time = Sys.time()
      ), 
      "'is_submitted': Must have length 1"
    )
  }
)

test_that(
  "current_user_oid must be integerish(1)", 
  {
    expect_error(
      updateInstanceIsSubmitted(
        report_instance_oid = 1, 
        is_submitted = FALSE, 
        current_user_oid = "1", 
        event_date_time = Sys.time()
      ), 
      "'current_user_oid': Must be of type 'integerish'"
    )
    
    expect_error(
      updateInstanceIsSubmitted(
        report_instance_oid = 1, 
        is_submitted = FALSE, 
        current_user_oid = 1:2, 
        event_date_time = Sys.time()
      ), 
      "'current_user_oid': Must have length 1"
    )
  }
)

test_that(
  "event_date_time must be POSIXct(1)", 
  {
    expect_error(
      updateInstanceIsSubmitted(
        report_instance_oid = 1, 
        is_submitted = FALSE, 
        current_user_oid = 1, 
        event_date_time = "datetime"
      ), 
      "'event_date_time': Must be of type 'POSIXct'"
    )
    
    expect_error(
      updateInstanceIsSubmitted(
        report_instance_oid = 1, 
        is_submitted = FALSE, 
        current_user_oid = 1, 
        event_date_time = rep(Sys.time(), 2)
      ), 
      "'event_date_time': Must have length 1"
    )
  }
)


# TODO: Functionality tests
