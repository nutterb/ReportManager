context("startReportManager.R")

# Argument Validation -----------------------------------------------

test_that(
  "Return an error if email is not character(1)", 
  {
    expect_error(startReportManager(email = c("off", "live")), 
                 "'email': Must have length 1")
    
    expect_error(startReportManager(email = 123), 
                 "'email': Must be of type 'character'")
  }
)