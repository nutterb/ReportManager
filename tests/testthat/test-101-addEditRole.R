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
                 "'role_name': Must be of type 'string'")
    
    expect_error(addEditRole(oid = 1, 
                             role_name = randomVarchar(76), 
                             role_description = "role description", 
                             event_user = 1), 
                 "'role_name': All elements must have at most 75")
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
                 "'role_description': Must be of type 'string'")
    
    expect_error(addEditRole(oid = 1, 
                             role_name = "role", 
                             role_description = randomVarchar(251), 
                             event_user = 1), 
                 "'role_description': All elements must have at most 250")
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

options(RM_sql_flavor = "sqlite")

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


test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_role_oid <- max(queryRole()$OID)
    next_role_oid <- last_role_oid + 1
    
    addEditRole(role_name = "RoleEventTesting",
                role_description = "testing that we record events",
                is_active = FALSE, 
                event_user = 1)
    
    RoleEvent <- dbGetQuery(conn, 
                            sqlInterpolate(
                              conn,
                              "SELECT * FROM RoleEvent WHERE ParentRole = ?", 
                              next_role_oid))
    
    expect_equal(RoleEvent$EventType,
                 c("Add", "Deactivate", "EditRoleName", "EditRoleDescription"))
    expect_true(all(table(RoleEvent$EventType) == 1))
    
    addEditRole(oid = next_role_oid, 
                role_name = "RoleEventTestingChange",
                role_description = "changing that we record events",
                is_active = TRUE, 
                event_user = 1)
    
    
    RoleEvent2 <- dbGetQuery(conn, 
                             sqlInterpolate(
                               conn,
                               "SELECT * FROM RoleEvent WHERE ParentRole = ?", 
                               next_role_oid))
    
    expect_true(
      all(table(RoleEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "EditRoleDescription" = 2, 
              "EditRoleName" = 2))
    )
    
    dbDisconnect(conn)
  }
)

# Functionality - SQL Server ----------------------------------------

options(RM_sql_flavor = "sql_server")

test_that(
  "Add a Role", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
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
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
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

test_that(
  "Confirm events are recorded correctly", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    last_role_oid <- max(queryRole()$OID)
    next_role_oid <- last_role_oid + 1
    
    addEditRole(role_name = "RoleEventTesting",
                role_description = "testing that we record events",
                is_active = FALSE, 
                event_user = 1)
    
    RoleEvent <- dbGetQuery(conn, 
                            sqlInterpolate(
                              conn,
                              "SELECT * FROM dbo.RoleEvent WHERE ParentRole = ?", 
                              next_role_oid))
    
    expect_equal(RoleEvent$EventType,
                 c("Add", "Deactivate", "EditRoleName", "EditRoleDescription"))
    expect_true(all(table(RoleEvent$EventType) == 1))
    
    addEditRole(oid = next_role_oid, 
                role_name = "RoleEventTestingChange",
                role_description = "changing that we record events",
                is_active = TRUE, 
                event_user = 1)
    
    
    RoleEvent2 <- dbGetQuery(conn, 
                             sqlInterpolate(
                               conn,
                               "SELECT * FROM dbo.RoleEvent WHERE ParentRole = ?", 
                               next_role_oid))
    
    expect_true(
      all(table(RoleEvent2$EventType) ==
            c("Activate" = 1, 
              "Add" = 1, 
              "Deactivate" = 1, 
              "EditRoleDescription" = 2, 
              "EditRoleName" = 2))
    )
    
    dbDisconnect(conn)
  }
)
