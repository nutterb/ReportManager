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

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a new user for the test
    addEditUser(last_name = "Activation", 
                first_name = "Event Test", 
                login_id = "active_test", 
                email = "email@nowhere.net", 
                event_user = 1)
    
    oid <- max(queryUser()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "User", 
                   event_table_name = "UserEvent", 
                   parent_field_name = "ParentUser")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "User", 
                   event_table_name = "UserEvent", 
                   parent_field_name = "ParentUser")
    
    UserEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM dbo.UserEvent WHERE ParentUser = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(UserEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
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

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a new user for the test
    addEditUser(last_name = "Activation", 
                first_name = "Event Test", 
                login_id = "active_test", 
                email = "email@nowhere.net", 
                event_user = 1)
    
    oid <- max(queryUser()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "User", 
                   event_table_name = "UserEvent", 
                   parent_field_name = "ParentUser")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "User", 
                   event_table_name = "UserEvent", 
                   parent_field_name = "ParentUser")
    
    UserEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM UserEvent WHERE ParentUser = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(UserEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
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

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a new user for the test
    addEditRole(role_name = "activation event",
                role_description = "activation event testing",
                event_user = 1)
    
    oid <- max(queryRole()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "Role", 
                   event_table_name = "RoleEvent", 
                   parent_field_name = "ParentRole")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "Role", 
                   event_table_name = "RoleEvent", 
                   parent_field_name = "ParentRole")
    
    RoleEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM RoleEvent WHERE ParentRole = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(RoleEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
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

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a new user for the test
    addEditRole(role_name = "activation event",
                role_description = "activation event testing",
                event_user = 1)
    
    oid <- max(queryRole()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "Role", 
                   event_table_name = "RoleEvent", 
                   parent_field_name = "ParentRole")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "Role", 
                   event_table_name = "RoleEvent", 
                   parent_field_name = "ParentRole")
    
    RoleEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM RoleEvent WHERE ParentRole = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(RoleEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
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

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    role_oid <- max(queryRole()$OID)
    
    # Make a new user for the test
    addEditUserRole(parent_user = 1, 
                    parent_role = role_oid,
                    event_user = 1)
    
    oid <- max(queryUserRole()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "UserRole", 
                   event_table_name = "UserRoleEvent", 
                   parent_field_name = "ParentUserRole")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "UserRole", 
                   event_table_name = "UserRoleEvent", 
                   parent_field_name = "ParentUserRole")
    
    UserRoleEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM dbo.UserRoleEvent WHERE ParentUserRole = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(UserRoleEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
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

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    role_oid <- max(queryRole()$OID)
    
    # Make a new user for the test
    addEditUserRole(parent_user = 1, 
                    parent_role = role_oid,
                    event_user = 1)
    
    oid <- max(queryUserRole()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "UserRole", 
                   event_table_name = "UserRoleEvent", 
                   parent_field_name = "ParentUserRole")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "UserRole", 
                   event_table_name = "UserRoleEvent", 
                   parent_field_name = "ParentUserRole")
    
    UserRoleEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM UserRoleEvent WHERE ParentUserRole = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(UserRoleEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
  }
)

# Schedule - SQL Server ---------------------------------------------

configureReportManager(flavor = "sql_server")

test_that(
  "Activate and Deactivate User Roles", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    ScheduleNow <- querySchedule(oid = 1)
    
    expect_true(ScheduleNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "Schedule", 
                   event_table_name = "ScheduleEvent", 
                   parent_field_name = "ParentSchedule")
    
    expect_false(querySchedule(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "Schedule", 
                   event_table_name = "ScheduleEvent", 
                   parent_field_name = "ParentSchedule")
    
    expect_true(querySchedule(oid = 1)$IsActive)
  }
)

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a new user for the test
    addEditSchedule(schedule_name = "event test", 
                    frequency = 1, 
                    frequency_unit = "Day", 
                    offset_overlap = 0, 
                    offset_overlap_unit = "Day",
                    event_user = 1)
    
    oid <- max(querySchedule()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "Schedule", 
                   event_table_name = "ScheduleEvent", 
                   parent_field_name = "ParentSchedule")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "Schedule", 
                   event_table_name = "ScheduleEvent", 
                   parent_field_name = "ParentSchedule")
    
    ScheduleEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM dbo.ScheduleEvent WHERE ParentSchedule = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(ScheduleEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
  }
)

# Schedule - SQLite -------------------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "Activate and Deactivate Roles", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    ScheduleNow <- querySchedule(oid = 1)
    
    expect_true(ScheduleNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "Schedule", 
                   event_table_name = "ScheduleEvent", 
                   parent_field_name = "ParentSchedule")
    
    expect_false(querySchedule(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "Schedule", 
                   event_table_name = "ScheduleEvent", 
                   parent_field_name = "ParentSchedule")
    
    expect_true(querySchedule(oid = 1)$IsActive)
  }
)

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a new user for the test
    addEditSchedule(schedule_name = "event test", 
                    frequency = 1, 
                    frequency_unit = "Day", 
                    offset_overlap = 0, 
                    offset_overlap_unit = "Day",
                    event_user = 1)
    
    oid <- max(querySchedule()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "Schedule", 
                   event_table_name = "ScheduleEvent", 
                   parent_field_name = "ParentSchedule")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "Schedule", 
                   event_table_name = "ScheduleEvent", 
                   parent_field_name = "ParentSchedule")
    
    ScheduleEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM ScheduleEvent WHERE ParentSchedule = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(ScheduleEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
  }
)

# DateReportingFormat - SQL Server ----------------------------------

configureReportManager(flavor = "sql_server")

test_that(
  "Activate and Deactivate User Roles", 
  {
    skip_if_not(SQL_SERVER_READY, 
                SQL_SERVER_READY_MESSAGE)
    
    DateReportingFormatNow <- queryDateReportingFormat(oid = 1)
    
    expect_true(DateReportingFormatNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "DateReportingFormat", 
                   event_table_name = "DateReportingFormatEvent", 
                   parent_field_name = "ParentDateReportingFormat")
    
    expect_false(queryDateReportingFormat(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "DateReportingFormat", 
                   event_table_name = "DateReportingFormatEvent", 
                   parent_field_name = "ParentDateReportingFormat")
    
    expect_true(queryDateReportingFormat(oid = 1)$IsActive)
  }
)

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a new user for the test
    addEditDateReportingFormat(format_name = "event test", 
                               description = "testing that events are written",
                               format_code = "%y-%d-%m",
                               increment_start = 0, 
                               increment_end = 0,
                               event_user = 1)
    
    oid <- max(queryDateReportingFormat()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "DateReportingFormat", 
                   event_table_name = "DateReportingFormatEvent", 
                   parent_field_name = "ParentDateReportingFormat")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "DateReportingFormat", 
                   event_table_name = "DateReportingFormatEvent", 
                   parent_field_name = "ParentDateReportingFormat")
    
    DateReportingFormatEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM dbo.DateReportingFormatEvent WHERE ParentDateReportingFormat = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(DateReportingFormatEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
  }
)


# DateReportingFormat - SQLite --------------------------------------

configureReportManager(flavor = "sqlite")

test_that(
  "Activate and Deactivate Roles", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    DateReportingFormatNow <- queryDateReportingFormat(oid = 1)
    
    expect_true(DateReportingFormatNow$IsActive)
    
    activateRecord(oid = 1, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "DateReportingFormat", 
                   event_table_name = "DateReportingFormatEvent", 
                   parent_field_name = "ParentDateReportingFormat")
    
    expect_false(queryDateReportingFormat(oid = 1)$IsActive)
    
    activateRecord(oid = 1, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "DateReportingFormat", 
                   event_table_name = "DateReportingFormatEvent", 
                   parent_field_name = "ParentDateReportingFormat")
    
    expect_true(queryDateReportingFormat(oid = 1)$IsActive)
  }
)

test_that(
  "Verify events are written for activating and deactivating", 
  {
    skip_if_not(SQLITE_READY, 
                SQLITE_READY_MESSAGE)
    
    conn <- connectToReportManager()
    
    # Make a new user for the test
    addEditDateReportingFormat(format_name = "event test", 
                               description = "testing that events are written",
                               format_code = "%y-%d-%m",
                               increment_start = 0, 
                               increment_end = 0,
                               event_user = 1)
    
    oid <- max(queryDateReportingFormat()$OID)
    
    activateRecord(oid = oid, 
                   active = FALSE, 
                   event_user = 1, 
                   table_name = "DateReportingFormat", 
                   event_table_name = "DateReportingFormatEvent", 
                   parent_field_name = "ParentDateReportingFormat")
    
    activateRecord(oid = oid, 
                   active = TRUE, 
                   event_user = 1, 
                   table_name = "DateReportingFormat", 
                   event_table_name = "DateReportingFormatEvent", 
                   parent_field_name = "ParentDateReportingFormat")
    
    DateReportingFormatEvent <- 
      dbGetQuery(conn, 
                 sqlInterpolate(
                   conn, 
                   "SELECT * FROM DateReportingFormatEvent WHERE ParentDateReportingFormat = ? AND EventType IN (?, ?)", 
                   oid, 
                   "Activate", 
                   "Deactivate"
                 ))
    
    expect_equal(DateReportingFormatEvent$EventType, 
                 c("Activate", "Deactivate", "Activate"))
    
    dbDisconnect(conn)
  }
)
