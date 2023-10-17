# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(addEditUserRole(oid = "1", 
                                 parent_user = 1, 
                                 parent_role = 1, 
                                 event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditUserRole(oid = 1:2, 
                                 parent_user = 1, 
                                 parent_role = 1, 
                                 event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if parent_user is not integerish(1)", 
  {
    expect_error(addEditUserRole(parent_user = "1", 
                                 parent_role = 1, 
                                 event_user = 1), 
                 "'parent_user': Must be of type 'integerish'")
    
    expect_error(addEditUserRole(parent_user = 1:2, 
                                 parent_role = 1, 
                                 event_user = 1), 
                 "'parent_user': Must have length 1")
  }
)

test_that(
  "Return an error if parent_role is not integerish(1)", 
  {
    expect_error(addEditUserRole(parent_user = 1, 
                                 parent_role = "1", 
                                 event_user = 1), 
                 "'parent_role': Must be of type 'integerish'")
    
    expect_error(addEditUserRole(parent_user = 1, 
                                 parent_role = 1:2, 
                                 event_user = 1), 
                 "'parent_role': Must have length 1")
  }
)

test_that(
  "Return an error if is_active is not logical(1)", 
  {
    expect_error(addEditUserRole(parent_user = 1, 
                                 parent_role = 1, 
                                 is_active = "TRUE",
                                 event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditUserRole(parent_user = 1, 
                                 parent_role = 1, 
                                 is_active = c(TRUE, FALSE),
                                 event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(addEditUserRole(parent_user = 1, 
                                 parent_role = 1, 
                                 event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditUserRole(parent_user = 1, 
                                 parent_role = 1, 
                                 event_user = 1:2), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - SQL Server ----------------------------------------

if (SQL_SERVER_READY){
  configureReportManager(flavor = "sql_server")
  purgeReportManagerDatabase()
  initializeReportManagerDatabase(system.file("Sql/SqlServer.sql", 
                                              package = "ReportManager"), 
                                  last_name = "Doe", 
                                  first_name = "Jane", 
                                  login_id = "jdoe", 
                                  email = "jdoe@domain.com")
}

test_that(
  "addEditUserRole functionality", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    # Add a role for this testing
    addEditRole(role_name = "UserRole Testing", 
                event_user = 1)
    
    addEditUserRole(parent_user = 2, 
                    parent_role = 5, 
                    event_user = 1)
    
    
    UserRole <- queryUserRole(user_oid = 2, 
                              role_oid = 5)
    expect_data_frame(UserRole, 
                      nrows = 1)
    expect_true(UserRole$IsActive)
    
    
    # Edit the UserRole (deactivate or remove the role from the user)
    
    addEditUserRole(oid = 4, 
                    parent_user = 2,
                    parent_role = 5, 
                    event_user = 1, 
                    is_active = FALSE)
    UserRole <- queryUserRole(user_oid = 2, 
                              role_oid = 5)
    expect_data_frame(UserRole, 
                      nrows = 1)
    expect_false(UserRole$IsActive)
    
  }
)

test_that(
  "Generate an error if trying to add a UserRole for a User-Role that already exists", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    expect_error(addEditUserRole(parent_user = 2, 
                                 parent_role = 5, 
                                 event_user = 1), 
                 "A UserRole record for User.OID")
  }
)

test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a role for testing
    
    addEditRole(role_name = "UserRoleEventTest", 
                role_description = "Role for User Role Event Testing", 
                is_active = TRUE, 
                event_user = 1)
    
    role_oid <- max(queryRole()$OID)
    
    # Assign a user to the role
    
    last_userrole_oid <- max(queryUserRole()$OID)
    next_userrole_oid <- last_userrole_oid + 1
    
    addEditUserRole(parent_user = 1, 
                    parent_role = role_oid, 
                    is_active = FALSE, 
                    event_user = 1)
    
    UserRoleEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  "SELECT * FROM dbo.UserRoleEvent WHERE ParentUserRole = ?", 
                                  next_userrole_oid))
    
    expect_equal(UserRoleEvent$EventType,
                 c("Add", "Deactivate"))
    expect_true(all(table(UserRoleEvent$EventType) == 1))
    
    addEditUserRole(oid = next_userrole_oid, 
                    parent_user = 1, 
                    parent_role = role_oid, 
                    is_active = TRUE,  
                    event_user = 1)
    
    
    UserRoleEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   "SELECT * FROM dbo.UserRoleEvent WHERE ParentUserRole = ?", 
                                   next_userrole_oid))
    
    expect_true(
      all(table(UserRoleEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1))
    )
    
    dbDisconnect(conn)
  }
)

# Functionality - SQLite --------------------------------------------

if (SQLITE_READY){
  configureReportManager(flavor = "sqlite")
  purgeReportManagerDatabase()
  initializeReportManagerDatabase(system.file("Sql/Sqlite.sql", 
                                              package = "ReportManager"), 
                                  last_name = "Doe", 
                                  first_name = "Jane", 
                                  login_id = "jdoe", 
                                  email = "jdoe@domain.com")
}

test_that(
  "addEditUserRole functionality", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    # Add a role for this testing
    addEditRole(role_name = "UserRole Testing", 
                event_user = 1)
    
    addEditUserRole(parent_user = 2, 
                    parent_role = 5, 
                    event_user = 1)
    
    
    UserRole <- queryUserRole(user_oid = 2, 
                              role_oid = 5)
    expect_data_frame(UserRole, 
                      nrows = 1)
    expect_true(UserRole$IsActive)
    
    
    # Edit the UserRole (deactivate or remove the role from the user)
    
    addEditUserRole(oid = 4, 
                    parent_user = 2,
                    parent_role = 5, 
                    event_user = 1, 
                    is_active = FALSE)
    UserRole <- queryUserRole(user_oid = 2, 
                              role_oid = 5)
    expect_data_frame(UserRole, 
                      nrows = 1)
    expect_false(UserRole$IsActive)
    
    
  }
)

test_that(
  "Generate an error if trying to add a UserRole for a User-Role that already exists", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    expect_error(addEditUserRole(parent_user = 2, 
                                 parent_role = 5, 
                                 event_user = 1), 
                 "A UserRole record for User.OID")
  }
)

test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a role for testing
    
    addEditRole(role_name = "UserRoleEventTest", 
                role_description = "Role for User Role Event Testing", 
                is_active = TRUE, 
                event_user = 1)
    
    role_oid <- max(queryRole()$OID)
    
    # Assign a user to the role
    
    last_userrole_oid <- max(queryUserRole()$OID)
    next_userrole_oid <- last_userrole_oid + 1
    
    addEditUserRole(parent_user = 1, 
                    parent_role = role_oid, 
                    is_active = FALSE, 
                    event_user = 1)
    
    UserRoleEvent <- dbGetQuery(conn, 
                                sqlInterpolate(
                                  conn,
                                  "SELECT * FROM UserRoleEvent WHERE ParentUserRole = ?", 
                                  next_userrole_oid))
    
    expect_equal(UserRoleEvent$EventType,
                 c("Add", "Deactivate"))
    expect_true(all(table(UserRoleEvent$EventType) == 1))
    
    addEditUserRole(oid = next_userrole_oid, 
                    parent_user = 1, 
                    parent_role = role_oid, 
                    is_active = TRUE,  
                    event_user = 1)
    
    
    UserRoleEvent2 <- dbGetQuery(conn, 
                                 sqlInterpolate(
                                   conn,
                                   "SELECT * FROM UserRoleEvent WHERE ParentUserRole = ?", 
                                   next_userrole_oid))
    
    expect_true(
      all(table(UserRoleEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1))
    )
    
    dbDisconnect(conn)
  }
)
