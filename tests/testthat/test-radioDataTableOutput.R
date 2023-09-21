# Argument Validation -----------------------------------------------

test_that(
  "Return an error if outputId is not character(1)", 
  {
    expect_error(radioDataTableOutput(outputId = 123, 
                                      radioId = "rdo_name"), 
                 "'outputId': Must be of type 'character'")
    
    expect_error(radioDataTableOutput(outputId = c("x", "y"), 
                                      radioId = "rdo_name"), 
                 "'outputId': Must have length 1")
  }
)

test_that(
  "Return an error if radioId is not character(1)",
  {
    expect_error(radioDataTableOutput(outputId = "dt_radio", 
                                      radioId = 123), 
                 "'radioId': Must be of type 'character'")
    
    expect_error(radioDataTableOutput(outputId = "dt_radio", 
                                      radioId = c("radio", "name")), 
                 "'radioId': Must have length 1")
  }
)

test_that(
  "Return an error if height is not character(1)",
  {
    expect_error(radioDataTableOutput(outputId = "dt_radio", 
                                      radioId = "rdo_name",
                                      height = 100), 
                 "'height': Must be of type 'character'")
    
    expect_error(radioDataTableOutput(outputId = "dt_radio", 
                                      radioId = "rdo_name", 
                                      height = c("100", "200")), 
                 "'height': Must have length 1")
  }
)

test_that(
  "Return an error if width is not character(1)",
  {
    expect_error(radioDataTableOutput(outputId = "dt_radio", 
                                      radioId = "rdo_name",
                                      width = 100), 
                 "'width': Must be of type 'character'")
    
    expect_error(radioDataTableOutput(outputId = "dt_radio", 
                                      radioId = "rdo_name", 
                                      width = c("100", "200")), 
                 "'width': Must have length 1")
  }
)

test_that(
  "Return an error if horizontal_scroll is not character(1)",
  {
    expect_error(radioDataTableOutput(outputId = "dt_radio", 
                                      radioId = "rdo_name",
                                      horizontal_scroll = "TRUE"), 
                 "'horizontal_scroll': Must be of type 'logical'")
    
    expect_error(radioDataTableOutput(outputId = "dt_radio", 
                                      radioId = "rdo_name", 
                                      horizontal_scroll = c(TRUE, FALSE)), 
                 "'horizontal_scroll': Must have length 1")
  }
)


# Functionality -----------------------------------------------------

test_that(
  "Validate the contents of the output object", 
  {
    output <- radioDataTableOutput(outputId = "dt_radio", 
                                   radioId = "rdo_name")
    
    expect_class(output, "shiny.tag")
    
    output <- as.character(output)
    
    expect_true(grepl("id=\"rdo_name\"", output))
    expect_true(grepl("overflow-y:scroll", output))
    expect_true(grepl("max-height:1000px", output))
    expect_true(grepl("width:100%", output))
    
    
    
    # Validate the result with non-default values
    output <- radioDataTableOutput(outputId = "dt_radio", 
                                   radioId = "rdo_name", 
                                   height = "100pt", 
                                   width = "500pt", 
                                   horizontal_scroll = FALSE)
    
    expect_class(output, "shiny.tag")
    
    output <- as.character(output)
    
    expect_true(grepl("id=\"rdo_name\"", output))
    expect_false(grepl("overflow-y", output))
    expect_true(grepl("max-height:100pt", output))
    expect_true(grepl("width:500pt", output))
  }
)
