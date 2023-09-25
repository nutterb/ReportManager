# Functionality -----------------------------------------------------

test_that(
  "Default object has status TRUE and no messages to report", 
  {
    ivc <- inputValidationCollection()
    
    expect_true(ivc$is_ok())
    
    expect_equal(ivc$report(), "")
  }
)

test_that(
  "Invalidating a collection makes the status FALSE", 
  {
    ivc <- inputValidationCollection()
    
    ivc$invalidate("This is the message")
    
    expect_false(ivc$is_ok())
    
    expect_equal(ivc$report(), "* This is the message")
  }
)


test_that(
  "Invalidating multiple times binds messages when reporting", 
  {
    ivc <- inputValidationCollection()
    
    ivc$invalidate("This message")
    
    ivc$invalidate("That message")
    
    expect_false(ivc$is_ok())
    
    expect_equal(ivc$report(), 
                 "* This message\n* That message")
  }
)
