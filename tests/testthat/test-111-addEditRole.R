# Argument Validation -----------------------------------------------

test_that(
  "Return an error when oid is not integerish(0/1)", 
  {
    expect_error(addEditRole(oid = "1", 
                             role_name = "role", 
                             role_description = "role description", 
                             event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditRole(oid = c(1, 2), 
                             role_name = "role", 
                             role_description = "role description", 
                             event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Retun an error when role_name is not character(1)", 
  {
    expect_error(addEditRole(oid = 1, 
                             role_name = c("a", "b"), 
                             role_description = "role description", 
                             event_user = 1), 
                 "'role_name': Must have length 1")
    
    expect_error(addEditRole(oid = 1, 
                             role_name = 123, 
                             role_description = "role description", 
                             event_user = 1), 
                 "'role_name': Must be of type 'character'")
  }
)

test_that(
  "Retun an error when role_description is not character(1)", 
  {
    expect_error(addEditRole(oid = 1, 
                             role_name = "role name", 
                             role_description = c("a", "b"), 
                             event_user = 1), 
                 "'role_description': Must have length 1")
    
    expect_error(addEditRole(oid = 1, 
                             role_name = "role", 
                             role_description = 123, 
                             event_user = 1), 
                 "'role_description': Must be of type 'character'")
  }
)

test_that(
  "Return an error when is_active is not logical(1)", 
  {
    expect_error(addEditRole(oid = 1, 
                             role_name = "role name", 
                             role_description = "role description", 
                             is_active = "TRUE", 
                             event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditRole(oid = 1, 
                             role_name = "role name", 
                             role_description = "role description", 
                             is_active = c(TRUE, FALSE), 
                             event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error when event_user is not integerish(1)", 
  {
    expect_error(addEditRole(oid = 1, 
                             role_name = "role", 
                             role_description = "role description", 
                             event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditRole(oid = 1, 
                             role_name = "role", 
                             role_description = "role description", 
                             event_user = c(1, 2)), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQLite --------------------------------------------

test_that(
  "Add a Role", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    addEditRole(role_name = "New Role", 
                role_description = "Role description", 
                event_user = 1)
    
    NewRole <- queryRole(oid = 4)
    
    expect_data_frame(NewRole, 
                      nrows = 1, 
                      ncols = 4)
  }
)

test_that(
  "Edit a Role", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    addEditRole(oid = 4, 
                role_name = "Edited Role Name", 
                event_user = 1)
    
    NewRole <- queryRole(oid = 4)
    
    expect_data_frame(NewRole, 
                      nrows = 1, 
                      ncols = 4)
    
    expect_equal(NewRole$RoleName, "Edited Role Name")
  }
)

# Functionality - SQL Server ----------------------------------------

# TODO: Add SQL Server Tests