# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(addEditUser(oid = "1", 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'oid': Must be of type 'integerish'")
    
    expect_error(addEditUser(oid = c(1, 2), 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'oid': Must have length <= 1")
  }
)

test_that(
  "Return an error if last_name is not character(1)", 
  {
    expect_error(addEditUser(oid = 1, 
                             last_name = 123, 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'last_name': Must be of type 'string'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = c("Doe", "Duck"), 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'last_name': Must have length 1")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = randomVarchar(51), 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'last_name': All elements must have at most 50")
  }
)

test_that(
  "Return an error if first_name is not character(1)", 
  {
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = 123, 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'first_name': Must be of type 'string'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = c("Jane", "John"), 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'first_name': Must have length 1")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = randomVarchar(51), 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'first_name': All elements must have at most 50")
  }
)

test_that(
  "Return an error if login_id is not character(1)", 
  {
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = 123, 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'login_id': Must be of type 'string'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = c("Jane", "John"), 
                             login_id = c("jdoe", "janedoe"), 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'login_id': Must have length 1")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = randomVarchar(51), 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'login_id': All elements must have at most 50")
  }
)

test_that(
  "Return an error if email is not character(1)", 
  {
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = 123, 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'email': Must be of type 'string'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = c("jdoe@domain.com", "janedoe@domain.com"), 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'email': Must have length 1")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = randomVarchar(101), 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'email': All elements must have at most 100")
  }
)

test_that(
  "Return an error if is_internal is not logical(1)", 
  {
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = "TRUE", 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'is_internal': Must be of type 'logical'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = c(FALSE, TRUE), 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'is_internal': Must have length 1")
  }
)

test_that(
  "Return an error if is_internal is not logical(1)", 
  {
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = "TRUE", 
                             event_user = 1), 
                 "'is_active': Must be of type 'logical'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = c(TRUE, FALSE), 
                             event_user = 1), 
                 "'is_active': Must have length 1")
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = "1"), 
                 "'event_user': Must be of type 'integerish'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = c(1, 2)), 
                 "'event_user': Must have length 1")
  }
)

# Functionality - ---------------------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
    purgeReportManagerDatabase()
    initializeReportManagerDatabase(SQL_FILE[flavor], 
                                    last_name = "Doe", 
                                    first_name = "Jane", 
                                    login_id = "jdoe", 
                                    email = "jdoe@domain.com")
  }
  
  test_that(
    "Add a User in SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      addEditUser(last_name = "Doe", 
                  first_name = "Jane", 
                  login_id = "jdoe", 
                  email = "jdoe@domain.com", 
                  is_internal = TRUE, 
                  is_active = TRUE, 
                  event_user = 1)
      
      conn <- connectToReportManager()
      
      NewUser <- queryUser(oid = 2)
      expect_data_frame(NewUser, 
                        nrows = 1, 
                        ncols = 7)
      
      NewUserEvent <- dbGetQuery(conn, 
                                 switch(flavor, 
                                        "sql_server" = "SELECT * FROM dbo.UserEvent WHERE ParentUser = 2",
                                        "SELECT * FROM UserEvent WHERE ParentUser = 2"))
      expect_data_frame(NewUserEvent, 
                        nrows = 7, 
                        ncols = 6)
      
      dbDisconnect(conn)
    }
  )
  
  test_that(
    "Edit a User in SQL Server", 
    {
      skip_if_not(.ready, 
                  .message)
      
      addEditUser(oid = 2, 
                  last_name = "Duck", 
                  first_name = "John", 
                  login_id = "jdoe2",
                  email = "jdoe2@domain.com", 
                  is_internal = FALSE, 
                  is_active = FALSE, 
                  event_user = 1)
      
      conn <- connectToReportManager()
      
      NewUser <- queryUser(oid = 2)
      expect_data_frame(NewUser, 
                        nrows = 1, 
                        ncols = 7)
      
      NewUserEvent <- dbGetQuery(conn, 
                                 switch(flavor, 
                                        "sql_server" = "SELECT * FROM dbo.UserEvent WHERE ParentUser = 2", 
                                        "SELECT * FROM UserEvent WHERE ParentUser = 2"))
      expect_data_frame(NewUserEvent, 
                        nrows = 13, 
                        ncols = 6)
      
      dbDisconnect(conn)
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      last_user_oid <- max(queryUser()$OID)
      next_user_oid <- last_user_oid + 1
      
      addEditUser(last_name = "EventTest", 
                  first_name = "User", 
                  login_id = "eventtest", 
                  email = "eventtest@nowhere.com", 
                  is_internal = FALSE, 
                  is_active = FALSE, 
                  event_user = 1)
      
      UserEvent <- dbGetQuery(conn, 
                              sqlInterpolate(
                                conn,
                                switch(flavor, 
                                       "sql_server" = "SELECT * FROM dbo.UserEvent WHERE ParentUser = ?", 
                                       "SELECT * FROM UserEvent WHERE ParentUser = ?"),
                                next_user_oid))
      
      expect_equal(UserEvent$EventType,
                   c("Add", "EditLastName", "EditFirstName", "EditLoginId", "EditEmailAddress", 
                     "SetInternalFalse", "Deactivate"))
      expect_true(all(table(UserEvent$EventType) == 1))
      
      addEditUser(oid = next_user_oid, 
                  last_name = "EventTestEdit", 
                  first_name = "UserEdit", 
                  login_id = "LoginIdChange", 
                  email = "new@email.org", 
                  is_internal = TRUE, 
                  is_active = TRUE, 
                  event_user = 1)
      
      
      UserEvent2 <- dbGetQuery(conn, 
                               sqlInterpolate(
                                 conn,
                                 switch(flavor, 
                                        "sql_server" = "SELECT * FROM dbo.UserEvent WHERE ParentUser = ?", 
                                        "SELECT * FROM UserEvent WHERE ParentUser = ?"), 
                                 next_user_oid))
      
      expect_true(
        all(table(UserEvent2$EventType) ==
              c("Activate" = 1, 
                "Add" = 1, 
                "Deactivate" = 1, 
                "EditEmailAddress" = 2, 
                "EditFirstName" = 2, 
                "EditLastName" = 2, 
                "EditLoginId" = 2, 
                "SetInternalFalse" = 1, 
                "SetInternalTrue" = 1))
      )
      
      dbDisconnect(conn)
    }
  )
}
