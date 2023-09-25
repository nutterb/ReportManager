# Argument validation - insertRecord --------------------------------

test_that(
  "Return an error if data is not a data frame", 
  {
    expect_error(insertRecord(data = "not data", 
                              table_name = "User", 
                              return_oid = TRUE), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error when table_name is not character(1)", 
  {
    expect_error(insertRecord(data = mtcars, 
                              table_name = 123, 
                              return_oid = TRUE), 
                 "'table_name': Must be of type 'character'")
    
    expect_error(insertRecord(data = mtcars, 
                              table_name = c("User", "Table"), 
                              return_oid = TRUE), 
                 "'table_name': Must have length 1")
  }
)

test_that(
  "Return an error when return_oid is not logical(1)", 
  {
    expect_error(insertRecord(data = mtcars,
                              table_name = "User",
                              return_oid = "TRUE"), 
                 "'return_oid': Must be of type 'logical'")
    
    expect_error(insertRecord(data = mtcars, 
                              table_name = "User",
                              return_oid = c(TRUE, FALSE)), 
                 "'return_oid': Must have length 1")
  }
)


test_that(
  "Return an error when flavor is not character(1)", 
  {
    expect_error(insertRecord(data = mtcars,
                              table_name = "User",
                              return_oid = TRUE, 
                              flavor = 123), 
                 "'flavor': Must be of type 'character'")
    
    expect_error(insertRecord(data = mtcars, 
                              table_name = "User",
                              return_oid = TRUE,
                              flavor = c("User", "Table")), 
                 "'flavor': Must have length 1")
  }
)

test_that(
  "Return an error when id_field is not character(1)", 
  {
    expect_error(insertRecord(data = mtcars,
                              table_name = "User",
                              return_oid = TRUE, 
                              id_field = 123), 
                 "'id_field': Must be of type 'character'")
    
    expect_error(insertRecord(data = mtcars, 
                              table_name = "User",
                              return_oid = TRUE,
                              id_field = c("User", "Table")), 
                 "'id_field': Must have length 1")
  }
)

test_that(
  "Return an error when schema is not character(1)", 
  {
    expect_error(insertRecord(data = mtcars,
                              table_name = "User",
                              return_oid = TRUE, 
                              schema = 123), 
                 "'schema': Must be of type 'character'")
    
    expect_error(insertRecord(data = mtcars, 
                              table_name = "User",
                              return_oid = TRUE,
                              schema = c("User", "Table")), 
                 "'schema': Must have length 1")
  }
)

# Argument validation - updateRecord --------------------------------

test_that(
  "Return an error if data is not a data frame", 
  {
    expect_error(updateRecord(data = "not data", 
                              where_data = mtcars,
                              table_name = "User"), 
                 "'data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error if where_data is not a data frame", 
  {
    expect_error(updateRecord(data = mtcars,
                              where_data = "not data",
                              table_name = "User"), 
                 "'where_data': Must be of type 'data.frame'")
  }
)

test_that(
  "Return an error when table_name is not character(1)", 
  {
    expect_error(updateRecord(data = mtcars, 
                              where_data = mtcars,
                              table_name = 123), 
                 "'table_name': Must be of type 'character'")
    
    expect_error(updateRecord(data = mtcars,
                              where_data = mtcars,
                              table_name = c("User", "Table")), 
                 "'table_name': Must have length 1")
  }
)

test_that(
  "Return an error when flavor is not character(1)", 
  {
    expect_error(updateRecord(data = mtcars,
                              where_data = mtcars,
                              table_name = "User",
                              flavor = 123), 
                 "'flavor': Must be of type 'character'")
    
    expect_error(updateRecord(data = mtcars, 
                              where_data = mtcars,
                              table_name = "User",
                              flavor = c("User", "Table")), 
                 "'flavor': Must have length 1")
  }
)

test_that(
  "Return an error when schema is not character(1)", 
  {
    expect_error(updateRecord(data = mtcars,
                              where_data = mtcars,
                              table_name = "User",
                              schema = 123), 
                 "'schema': Must be of type 'character'")
    
    expect_error(updateRecord(data = mtcars, 
                              where_data = mtcars,
                              table_name = "User",
                              schema = c("User", "Table")), 
                 "'schema': Must have length 1")
  }
)
