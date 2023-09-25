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
                 "'last_name': Must be of type 'character'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = c("Doe", "Duck"), 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'last_name': Must have length 1")
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
                 "'first_name': Must be of type 'character'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = c("Jane", "John"), 
                             login_id = "jdoe", 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'first_name': Must have length 1")
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
                 "'login_id': Must be of type 'character'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = c("Jane", "John"), 
                             login_id = c("jdoe", "janedoe"), 
                             email = "jdoe@domain.com", 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'login_id': Must have length 1")
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
                 "'email': Must be of type 'character'")
    
    expect_error(addEditUser(oid = 1, 
                             last_name = "Doe", 
                             first_name = "Jane", 
                             login_id = "jdoe", 
                             email = c("jdoe@domain.com", "janedoe@domain.com"), 
                             is_internal = TRUE, 
                             is_active = TRUE, 
                             event_user = 1), 
                 "'email': Must have length 1")
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

# Functionality - SqlServer -----------------------------------------

configureReportManager(flavor = "sql_server")

test_that(
  "Add a User in SQL Server", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
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
    
    NewUserEvent <- dbGetQuery(conn, "SELECT * FROM dbo.UserEvent WHERE ParentUser = 2")
    expect_data_frame(NewUserEvent, 
                      nrows = 7, 
                      ncols = 6)
    
    dbDisconnect(conn)
  }
)

test_that(
  "Edit a User in SQLite", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
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
    
    NewUserEvent <- dbGetQuery(conn, "SELECT * FROM dbo.UserEvent WHERE ParentUser = 2")
    expect_data_frame(NewUserEvent, 
                      nrows = 13, 
                      ncols = 6)
    
    dbDisconnect(conn)
  }
)

# Functionality - SQLite --------------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "Add a User in SQLite", 
  {
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
    
    NewUserEvent <- dbGetQuery(conn, "SELECT * FROM UserEvent WHERE ParentUser = 2")
    expect_data_frame(NewUserEvent, 
                      nrows = 7, 
                      ncols = 6)
    
    dbDisconnect(conn)
  }
)

test_that(
  "Edit a User in SQLite", 
  {
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
    
    NewUserEvent <- dbGetQuery(conn, "SELECT * FROM UserEvent WHERE ParentUser = 2")
    expect_data_frame(NewUserEvent, 
                      nrows = 13, 
                      ncols = 6)
    
    dbDisconnect(conn)
  }
)
