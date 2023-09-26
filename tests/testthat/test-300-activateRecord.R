# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(1)", 
  {
    expect_error(activateRecord(oid = "1", 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "User", 
                                event_table_name = "UserEvent", 
                                parent_field_name = "ParentUser"), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(activateRecord(oid = c(1, 2), 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "User", 
                                event_table_name = "UserEvent", 
                                parent_field_name = "ParentUser"), 
                 "'oid': Must have length 1")
  }
)

test_that(
  "Return an error if active is not logical(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = "TRUE", 
                                event_user = 1, 
                                table_name = "User", 
                                event_table_name = "UserEvent", 
                                parent_field_name = "ParentUser"), 
                 "'active': Must be of type 'logical'")
    
    expect_error(activateRecord(oid = 1, 
                                active = c(TRUE, FALSE), 
                                event_user = 1, 
                                table_name = "User", 
                                event_table_name = "UserEvent", 
                                parent_field_name = "ParentUser"), 
                 "'active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = "1", 
                                table_name = "User", 
                                event_table_name = "UserEvent", 
                                parent_field_name = "ParentUser"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = c(1, 2), 
                                table_name = "User", 
                                event_table_name = "UserEvent", 
                                parent_field_name = "ParentUser"), 
                 "'event_user': Must have length 1")
  }
)

test_that(
  "Return an error if table_name is not character(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = 123, 
                                event_table_name = "UserEvent", 
                                parent_field_name = "ParentUser"), 
                 "'table_name': Must be of type 'character'")
    
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = c("User", "ReportRole"), 
                                event_table_name = "UserEvent", 
                                parent_field_name = "ParentUser"), 
                 "'table_name': Must have length 1")
  }
)

test_that(
  "Return an error if event_table_name is not character(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "User", 
                                event_table_name = 123, 
                                parent_field_name = "ParentUser"), 
                 "'event_table_name': Must be of type 'character'")
    
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "User", 
                                event_table_name = c("UserEvent", "RoleEvent"), 
                                parent_field_name = "ParentUser"), 
                 "'event_table_name': Must have length 1")
  }
)

test_that(
  "Return an error if parent_field_name is not character(1)", 
  {
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "User", 
                                event_table_name = "UserEvent", 
                                parent_field_name = 123), 
                 "'parent_field_name': Must be of type 'character'")
    
    expect_error(activateRecord(oid = 1, 
                                active = TRUE, 
                                event_user = 1, 
                                table_name = "User", 
                                event_table_name = "UserEvent", 
                                parent_field_name = c("ParentUser", "ParentRole")), 
                 "'parent_field_name': Must have length 1")
  }
)

# User - SQL Server -------------------------------------------------

configureReportManager(flavor = "sql_server")

test_that(
  "Activate and Deactivate Users", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    UserNow <- queryUser(oid = 1)
    
    expect_true(UserNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "User", 
                   event_table_name = "UserEvent", 
                   parent_field_name = "ParentUser")
    
    expect_false(queryUser(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "User", 
                   event_table_name = "UserEvent", 
                   parent_field_name = "ParentUser")
    
    expect_true(queryUser(oid = 1)$IsActive)
  }
)

# User - SQLite -----------------------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "Activate and Deactivate Users", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    UserNow <- queryUser(oid = 1)
    
    expect_true(UserNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "User", 
                   event_table_name = "UserEvent", 
                   parent_field_name = "ParentUser")
    
    expect_false(queryUser(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "User", 
                   event_table_name = "UserEvent", 
                   parent_field_name = "ParentUser")
    
    expect_true(queryUser(oid = 1)$IsActive)
  }
)

# Role - SQL Server -------------------------------------------------

configureReportManager(flavor = "sql_server")

test_that(
  "Activate and Deactivate Roles", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    RoleNow <- queryRole(oid = 1)
    
    expect_true(RoleNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "Role", 
                   event_table_name = "RoleEvent", 
                   parent_field_name = "ParentRole")
    
    expect_false(queryRole(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "Role", 
                   event_table_name = "RoleEvent", 
                   parent_field_name = "ParentRole")
    
    expect_true(queryRole(oid = 1)$IsActive)
  }
)

# Role - SQLite -----------------------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "Activate and Deactivate Roles", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    RoleNow <- queryRole(oid = 1)
    
    expect_true(RoleNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "Role", 
                   event_table_name = "RoleEvent", 
                   parent_field_name = "ParentRole")
    
    expect_false(queryRole(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "Role", 
                   event_table_name = "RoleEvent", 
                   parent_field_name = "ParentRole")
    
    expect_true(queryRole(oid = 1)$IsActive)
  }
)

# User Role - SQL Server --------------------------------------------

configureReportManager(flavor = "sql_server")

test_that(
  "Activate and Deactivate User Roles", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    UserRoleNow <- queryUserRole(oid = 1)
    
    expect_true(UserRoleNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "UserRole", 
                   event_table_name = "UserRoleEvent", 
                   parent_field_name = "ParentUserRole")
    
    expect_false(queryUserRole(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "UserRole", 
                   event_table_name = "UserRoleEvent", 
                   parent_field_name = "ParentUserRole")
    
    expect_true(queryUserRole(oid = 1)$IsActive)
  }
)

# User Role - SQLite ------------------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "Activate and Deactivate Roles", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    UserRoleNow <- queryUserRole(oid = 1)
    
    expect_true(UserRoleNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "UserRole", 
                   event_table_name = "UserRoleEvent", 
                   parent_field_name = "ParentUserRole")
    
    expect_false(queryUserRole(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "UserRole", 
                   event_table_name = "UserRoleEvent", 
                   parent_field_name = "ParentUserRole")
    
    expect_true(queryUserRole(oid = 1)$IsActive)
  }
)
