# Argument Validation -----------------------------------------------

test_that(
  "title must be character(1)", 
  {
    expect_error(collapsibleDiv(title = 1, 
                                id = "id", 
                                style = "", 
                                checked = FALSE), 
                 "'title': Must be of type 'character'")
    
    expect_error(collapsibleDiv(title = letters, 
                                id = "id", 
                                style = "", 
                                checked = FALSE), 
                 "'title': Must have length 1")
  }
)

test_that(
  "id must be character(1)", 
  {
    expect_error(collapsibleDiv(title = "title", 
                                id = 1, 
                                style = "", 
                                checked = FALSE), 
                 "'id': Must be of type 'character'")
    
    expect_error(collapsibleDiv(title = "title", 
                                id = letters, 
                                style = "", 
                                checked = FALSE), 
                 "'id': Must have length 1")
  }
)

test_that(
  "style must be character(1)", 
  {
    expect_error(collapsibleDiv(title = "title", 
                                id = "id", 
                                style = 123, 
                                checked = FALSE), 
                 "'style': Must be of type 'character'")
    
    expect_error(collapsibleDiv(title = "title", 
                                id = "id", 
                                style = letters, 
                                checked = FALSE), 
                 "'style': Must have length 1")
  }
)

test_that(
  "checked must be logical(1)", 
  {
    expect_error(collapsibleDiv(title = "title", 
                                id = "id", 
                                style = "", 
                                checked = "FALSE"), 
                 "'checked': Must be of type 'logical'")
    
    expect_error(collapsibleDiv(title = "title", 
                                id = "id", 
                                style = "", 
                                checked = c(TRUE, FALSE)), 
                 "'checked': Must have length 1")
  }
)

test_that(
  "div_id must be character(1)", 
  {
    expect_error(collapsibleDiv(title = "title", 
                                id = "id", 
                                style = "", 
                                checked = FALSE, 
                                div_id = 123), 
                 "'div_id': Must be of type 'character'")
    
    expect_error(collapsibleDiv(title = "title", 
                                id = "id", 
                                style = "", 
                                checked = FALSE, 
                                div_id = letters), 
                 "'div_id': Must have length 1")
  }
)

# Functionality -----------------------------------------------------

test_that(
  "collapsible div returns the desired output", 
  {
    title <- "title"
    id <- "id"
    style <- "style"
    checked <- FALSE
    div_id <- "colldiv-id"
    
    cd <- collapsibleDiv(title, id, style = style, checked = checked) 
    
    compare <- 
      shiny::tagList(
        shiny::div(class = "wrap-collabsible",
                   id = div_id, 
                   shiny::HTML(sprintf("<input id = '%s' class = 'toggle' type = 'checkbox' style = 'display:none;' %s/>",
                                       id,
                                       if (checked) "checked" else "")),
                   shiny::tags$label(`for` = id,
                                     class = "lbl-toggle",
                                     style = style,
                                     title),
                   shiny::div(class = "collapsible-content",
                              shiny::div(class = "content-inner")))
      )
      
      expect_equal(cd, compare)
  }
)
