# Argument Validation - multiSelect ---------------------------------

test_that(
  "Return an error if inputId is not character(1)", 
  {
    expect_error(multiSelect(inputId = 123, 
                             label = "label", 
                             choices = letters, 
                             selected = c("a", "z"), 
                             width = "100%", 
                             up_down = FALSE), 
                 "'inputId': Must be of type 'character'")
    
    expect_error(multiSelect(inputId = c("input", "name"), 
                             label = "label", 
                             choices = letters, 
                             selected = c("a", "z"), 
                             width = "100%", 
                             up_down = FALSE), 
                 "'inputId': Must have length 1")
  }
)


test_that(
  "Return an error if label is not character(1)", 
  {
    expect_error(multiSelect(inputId = "input", 
                             label = 123, 
                             choices = letters, 
                             selected = c("a", "z"), 
                             width = "100%", 
                             up_down = FALSE), 
                 "'label': Must be of type 'character'")
    
    expect_error(multiSelect(inputId = "input", 
                             label = c("label", "name"), 
                             choices = letters, 
                             selected = c("a", "z"), 
                             width = "100%", 
                             up_down = FALSE), 
                 "'label': Must have length 1")
  }
)


test_that(
  "Return an error if choices/selected are not character", 
  {
    expect_error(multiSelect(inputId = "input", 
                             label = "label", 
                             choices = 1:10, 
                             selected = "1", 
                             width = "100%", 
                             up_down = FALSE), 
                 "'choices': Must be of type 'character'")
    
    expect_error(multiSelect(inputId = "input", 
                             label = "label", 
                             choices = as.character(1:10), 
                             selected = 1, 
                             width = "100%", 
                             up_down = FALSE), 
                 "'selected': Must be of type 'character'")
  }
)

test_that(
  "Return an error if selected is not a subset of choices", 
  {
    expect_error(multiSelect(inputId = "input", 
                             label = "label", 
                             choices = letters, 
                             selected = "one", 
                             width = "100%", 
                             up_down = FALSE), 
                 "'selected': Must be a subset of")
  }
)

test_that(
  "Return an error if width is not character(1)", 
  {
    expect_error(multiSelect(inputId = "input", 
                             label = "label", 
                             choices = letters, 
                             selected = c("a", "z"), 
                             width = 100, 
                             up_down = FALSE), 
                 "'width': Must be of type 'character'")
    
    expect_error(multiSelect(inputId = "input", 
                             label = "label", 
                             choices = letters, 
                             selected = c("a", "z"), 
                             width = c("10%", "100%"), 
                             up_down = FALSE), 
                 "'width': Must have length 1")
  }
)

test_that(
  "Return an error if up_down is not logical(1)", 
  {
    expect_error(multiSelect(inputId = "input", 
                             label = "label", 
                             choices = letters, 
                             selected = c("a", "z"), 
                             width = "100%", 
                             up_down = "FALSE"), 
                 "'up_down': Must be of type 'logical'")
    
    expect_error(multiSelect(inputId = "input", 
                             label = "label", 
                             choices = letters, 
                             selected = c("a", "z"), 
                             width = "100%", 
                             up_down = c(FALSE, TRUE)), 
                 "'up_down': Must have length 1")
  }
)

# Functionality - multiSelect ---------------------------------------

test_that(
  "multiSelect return shiny tags", 
  {
    expect_class(multiSelect(inputId = "input", 
                             label = "label", 
                             choices = letters, 
                             selected = c("a", "z")), 
                 "shiny.tag.list")
  }
)

# Argument Validation - updateMultiSelect ---------------------------

test_that(
  "Return an error if inputId is not character(1)", 
  {
    expect_error(updateMultiSelect(inputId = 123), 
                 "'inputId': Must be of type 'character'")
    
    expect_error(updateMultiSelect(inputId = c("input", "name")), 
                 "'inputId': Must have length 1")
  }
)

test_that(
  "Return an error if action is not an acceptable value", 
  {
    expect_error(updateMultiSelect(inputId = "input", 
                                   action = "something different"), 
                 "'action': Must be element of set")
  }
)

# Argument Validation - replaceMultiSelect --------------------------

test_that(
  "Return an error if inputId is not character(1)", 
  {
    expect_error(replaceMultiSelect(inputId = 123, 
                                    choices = letters, 
                                    selected = c("a", "z")), 
                 "'inputId': Must be of type 'character'")
    
    expect_error(replaceMultiSelect(inputId = c("input", "name"), 
                                    choices = letters, 
                                    selected = c("a", "z")), 
                 "'inputId': Must have length 1")
  }
)

test_that(
  "Return an error if choices/selected are not character", 
  {
    expect_error(replaceMultiSelect(inputId = "input", 
                                    choices = 1:10, 
                                    selected = "1"), 
                 "'choices': Must be of type 'character'")
    
    expect_error(replaceMultiSelect(inputId = "input", 
                                    choices = as.character(1:10), 
                                    selected = 1), 
                 "'selected': Must be of type 'character'")
  }
)

test_that(
  "Return an error if selected is not a subset of choices", 
  {
    expect_error(replaceMultiSelect(inputId = "input", 
                                    choices = letters, 
                                    selected = "one"), 
                 "'selected': Must be a subset of")
  }
)
