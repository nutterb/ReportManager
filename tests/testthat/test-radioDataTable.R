TestingData <- data.frame(id = 1:10, 
                          letter = letters[1:10])

# Argument Validation -----------------------------------------------

test_that(
  "Return an error if data is not a data frame", 
  {
    expect_error(radioDataTable(data = "not data", 
                                id_variable = "id", 
                                element_name = "rdo_name"), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if id_variable is not character(1)",
  {
    expect_error(radioDataTable(data = TestingData, 
                                id_variable = letters, 
                                element_name = "rdo_name"), 
                 "'id_variable': Must have length 1")
    
    expect_error(radioDataTable(data = TestingData, 
                                id_variable = 1234, 
                                element_name = "rdo_name"), 
                 "'id_variable': Must be of type 'character'")
  }
)

test_that(
  "Return an error if id_variable is not a variable in data", 
  {
    expect_error(radioDataTable(data = TestingData, 
                                id_variable = "not the id", 
                                element_name = "rdo_name"), 
                 "'id_variable': Must be a subset of")
  }
)

test_that(
  "Return an error if element_name is not character(1)", 
  {
    expect_error(radioDataTable(data = TestingData, 
                                id_variable = "id", 
                                element_name = letters), 
                 "'element_name': Must have length 1")
    
    expect_error(radioDataTable(data = TestingData, 
                                id_variable = "id", 
                                element_name = 1234), 
                 "'element_name': Must be of type 'character'")
  }
)

test_that(
  "Return an error if checked is not character(0/1) or integerish(0/1)", 
  {
    expect_error(radioDataTable(data = TestingData, 
                                id_variable = "id", 
                                element_name = "rdo_name", 
                                checked = TRUE), 
                 "One of the following must apply")
    
    expect_error(radioDataTable(data = TestingData, 
                                id_variable = "id", 
                                element_name = "rdo_name", 
                                checked = 1:2), 
                 "One of the following must apply")
  }
)
    
test_that(
  "Return an error if checked is not an element of the id_field", 
  {
    expect_error(radioDataTable(data = TestingData, 
                                id_variable = "id", 
                                element_name = "rdo_name", 
                                checked = "alpha"), 
                 "'checked': Must be a subset of")
  }
)

# Functionality -----------------------------------------------------

test_that(
  "Radio data table is returned", 
  {
    # No default checked value
    Radio <- radioDataTable(TestingData, 
                            id_variable = "id", 
                            element_name = "rdo_name")
    
    expect_equal(Radio$id[1], 
                 "<input type = 'radio' name = 'rdo_name' value = '1' />")
    
    
    # Use an integerish value for checked.
    Radio <- radioDataTable(TestingData, 
                            id_variable = "id", 
                            element_name = "rdo_name", 
                            checked = 3)
    
    expect_equal(Radio$id[3], 
                 "<input type = 'radio' name = 'rdo_name' value = '3' checked = 'checked'/>")
    
    
    # Use a character value for checked.
    Radio <- radioDataTable(TestingData, 
                            id_variable = "id", 
                            element_name = "rdo_name", 
                            checked = "3")
    
    expect_equal(Radio$id[3], 
                 "<input type = 'radio' name = 'rdo_name' value = '3' checked = 'checked'/>")
  }
)
