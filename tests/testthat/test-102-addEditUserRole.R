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

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
    purgeReportManagerDatabase()
    initializeUiTestingDatabase(SQL_FILE[flavor], 
                                include = c("User", "Role"))
  }
  
  test_that(
    "addEditUserRole functionality", 
    {
      skip_if_not(.ready, 
                  .message)
      
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
      skip_if_not(.ready, 
                  .message)
      
      expect_error(addEditUserRole(parent_user = 2, 
                                   parent_role = 5, 
                                   event_user = 1), 
                   "A UserRole record for User.OID")
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
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
                                    switch(flavor, 
                                           "sql_server" = "SELECT * FROM dbo.UserRoleEvent WHERE ParentUserRole = ?",
                                           "SELECT * FROM UserRoleEvent WHERE ParentUserRole = ?"),
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
                                     switch(flavor, 
                                            "sql_server" = "SELECT * FROM dbo.UserRoleEvent WHERE ParentUserRole = ?",
                                            "SELECT * FROM UserRoleEvent WHERE ParentUserRole = ?"), 
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
}
