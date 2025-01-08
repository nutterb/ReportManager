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


# Set Up Testing Database

if (SQL_SERVER_READY){
  configureReportManager(flavor = "sql_server")
  purgeReportManagerDatabase()
  initializeUiTestingDatabase(system.file("Sql/SqlServer.sql", 
                                          package = "ReportManager"), 
                              include = c("User", "Role", "UserRole", 
                                          "ReportTemplate"))
}

if (SQLITE_READY){
  configureReportManager(flavor = "sqlite")
  purgeReportManagerDatabase()
  initializeUiTestingDatabase(system.file("Sql/Sqlite.sql", 
                                          package = "ReportManager"), 
                              include = c("User", "Role", "UserRole", 
                                          "ReportTemplate"))
}

# Functionality -----------------------------------------------------

TABLE_ACTIVATE <- 
  "Table,EventTable,ParentFieldName,Function
User,UserEvent,ParentUser,queryUser
Role,RoleEvent,ParentRole,queryRole
UserRole,UserRoleEvent,ParentUserRole,queryUserRole
Schedule,ScheduleEvent,ParentSchedule,querySchedule
DateReportingFormat,DateReportingFormatEvent,ParentDateReportingFormat,queryDateReportingFormat
Disclaimer,DisclaimerEvent,ParentDisclaimer,queryDisclaimer
Footer,FooterEvent,ParentFooter,queryFooter
ReportTemplate,ReportTemplateEvent,ParentReportTemplate,queryReportTemplate
AutoDistribution,AutoDistributionEvent,ParentAutoDistribution,queryAutoDistribution"

TABLE_ACTIVATE <- read.csv(text = TABLE_ACTIVATE, 
                         stringsAsFactors = FALSE)

if (nrow(queryAutoDistribution()) == 0){
  addEditAutoDistribution(parent_report_template = 1, 
                          start_date_time = Sys.time(), 
                          is_active = TRUE, 
                          delay_after_instance_end = 1, 
                          delay_units = "Second", 
                          current_or_last_instance = "Current", 
                          is_add_to_archive = FALSE, 
                          report_format = "html", 
                          is_distribute_internal_only = TRUE,  
                          is_embed_html = TRUE, 
                          event_user = 1)
}

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  for (r in seq_len(nrow(TABLE_ACTIVATE))){
    test_that(
      "Activate and Deactivate Users", 
      {
        message(TABLE_ACTIVATE$Table[r])
        skip_if_not(.ready, 
                    .message)
        
        conn <- connectToReportManager()
        
        if (nrow(queryAutoDistribution()) == 0){
          addEditAutoDistribution(parent_report_template = 1, 
                                  start_date_time = Sys.time(), 
                                  is_active = TRUE, 
                                  delay_after_instance_end = 1, 
                                  delay_units = "Second", 
                                  current_or_last_instance = "Current", 
                                  is_add_to_archive = FALSE, 
                                  report_format = "html", 
                                  is_distribute_internal_only = TRUE,  
                                  is_embed_html = TRUE, 
                                  event_user = 1)
        }
        
        Now <- do.call(TABLE_ACTIVATE$Function[r], 
                       list(1))
        
        expect_true(Now$IsActive)
        
        activateRecord(oid = 1, 
                       active = FALSE, 
                       event_user = 1, 
                       table_name = TABLE_ACTIVATE$Table[r], 
                       event_table_name = TABLE_ACTIVATE$EventTable[r], 
                       parent_field_name = TABLE_ACTIVATE$ParentFieldName[r])
        
        New1 <- do.call(TABLE_ACTIVATE$Function[r], 
                        list(1))
        
        expect_false(New1$IsActive)
        
        activateRecord(oid = 1, 
                       active = TRUE, 
                       event_user = 1, 
                       table_name = TABLE_ACTIVATE$Table[r], 
                       event_table_name = TABLE_ACTIVATE$EventTable[r], 
                       parent_field_name = TABLE_ACTIVATE$ParentFieldName[r])
        
        New2 <- do.call(TABLE_ACTIVATE$Function[r], 
                        list(1))
        expect_true(New2$IsActive)
        
        sql_server_statement <- 
          sprintf("SELECT * FROM dbo.%s WHERE %s = ? AND EventType IN (?, ?) ORDER BY EventDateTime DESC, OID DESC", 
                  TABLE_ACTIVATE$EventTable[r], 
                  TABLE_ACTIVATE$ParentFieldName[r])
        sqlite_statement <- 
          sprintf("SELECT * FROM %s WHERE %s = ? AND EventType IN (?, ?) ORDER BY EventDateTime DESC, OID DESC", 
                  TABLE_ACTIVATE$EventTable[r], 
                  TABLE_ACTIVATE$ParentFieldName[r])
        
        Event <- 
          dbGetQuery(conn, 
                     sqlInterpolate(
                       conn,
                       switch(flavor, 
                              "sql_server" = sql_server_statement,
                              sqlite_statement),
                       1, 
                       "Activate", 
                       "Deactivate"
                     ))
        
        Event <- head(Event, 3)
        
        expect_equal(Event$EventType, 
                     c("Activate", "Deactivate", "Activate"))
        
        dbDisconnect(conn)
      }
    )
  }
}

# # User --------------------------------------------------------------
# 
# for (flavor in FLAVOR){
#   message(sprintf("Testing for SQL Flavor: %s\n", flavor))
#   .ready <- READY[flavor]
#   .message <- MESSAGE[flavor]
#   
#   if (.ready){
#     configureReportManager(flavor = flavor)
#   }
#   
#   test_that(
#     "Activate and Deactivate Users", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       UserNow <- queryUser(oid = 1)
#       
#       expect_true(UserNow$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "User", 
#                      event_table_name = "UserEvent", 
#                      parent_field_name = "ParentUser")
#       
#       expect_false(queryUser(oid = 1)$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "User", 
#                      event_table_name = "UserEvent", 
#                      parent_field_name = "ParentUser")
#       
#       expect_true(queryUser(oid = 1)$IsActive)
#     }
#   )
#   
#   test_that(
#     "Verify events are written for activating and deactivating", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       conn <- connectToReportManager()
#       
#       # Make a new user for the test
#       addEditUser(last_name = "Activation", 
#                   first_name = "Event Test", 
#                   login_id = "active_test", 
#                   email = "email@nowhere.net", 
#                   event_user = 1)
#       
#       oid <- max(queryUser()$OID)
#       
#       activateRecord(oid = oid, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "User", 
#                      event_table_name = "UserEvent", 
#                      parent_field_name = "ParentUser")
#       
#       activateRecord(oid = oid, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "User", 
#                      event_table_name = "UserEvent", 
#                      parent_field_name = "ParentUser")
#       
#       UserEvent <- 
#         dbGetQuery(conn, 
#                    sqlInterpolate(
#                      conn,
#                      switch(flavor, 
#                             "sql_server" = "SELECT * FROM dbo.UserEvent WHERE ParentUser = ? AND EventType IN (?, ?)",
#                             "SELECT * FROM UserEvent WHERE ParentUser = ? AND EventType IN (?, ?)"),
#                      oid, 
#                      "Activate", 
#                      "Deactivate"
#                    ))
#       
#       expect_equal(UserEvent$EventType, 
#                    c("Activate", "Deactivate", "Activate"))
#       
#       dbDisconnect(conn)
#     }
#   )
# }
# 
# 
# 
# # Role - SQL Server -------------------------------------------------
# 
# for (flavor in FLAVOR){
#   message(sprintf("Testing for SQL Flavor: %s\n", flavor))
#   .ready <- READY[flavor]
#   .message <- MESSAGE[flavor]
#   
#   if (.ready){
#     configureReportManager(flavor = flavor)
#   }
#   
#   test_that(
#     "Activate and Deactivate Roles", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       RoleNow <- queryRole(oid = 1)
#       
#       expect_true(RoleNow$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "Role", 
#                      event_table_name = "RoleEvent", 
#                      parent_field_name = "ParentRole")
#       
#       expect_false(queryRole(oid = 1)$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "Role", 
#                      event_table_name = "RoleEvent", 
#                      parent_field_name = "ParentRole")
#       
#       expect_true(queryRole(oid = 1)$IsActive)
#     }
#   )
#   
#   test_that(
#     "Verify events are written for activating and deactivating", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       conn <- connectToReportManager()
#       
#       # Make a new user for the test
#       addEditRole(role_name = "activation event",
#                   role_description = "activation event testing",
#                   event_user = 1)
#       
#       oid <- max(queryRole()$OID)
#       
#       activateRecord(oid = oid, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "Role", 
#                      event_table_name = "RoleEvent", 
#                      parent_field_name = "ParentRole")
#       
#       activateRecord(oid = oid, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "Role", 
#                      event_table_name = "RoleEvent", 
#                      parent_field_name = "ParentRole")
#       
#       RoleEvent <- 
#         dbGetQuery(conn, 
#                    sqlInterpolate(
#                      conn, 
#                      switch(flavor, 
#                             "sql_server" = "SELECT * FROM dbo.RoleEvent WHERE ParentRole = ? AND EventType IN (?, ?)",
#                             "SELECT * FROM RoleEvent WHERE ParentRole = ? AND EventType IN (?, ?)"), 
#                      oid, 
#                      "Activate", 
#                      "Deactivate"
#                    ))
#       
#       expect_equal(RoleEvent$EventType, 
#                    c("Activate", "Deactivate", "Activate"))
#       
#       dbDisconnect(conn)
#     }
#   )
#   
# }
# 
# 
# # User Role - SQL Server --------------------------------------------
# 
# for (flavor in FLAVOR){
#   message(sprintf("Testing for SQL Flavor: %s\n", flavor))
#   .ready <- READY[flavor]
#   .message <- MESSAGE[flavor]
#   
#   if (.ready){
#     configureReportManager(flavor = flavor)
#   }
#   
#   test_that(
#     "Activate and Deactivate User Roles", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       UserRoleNow <- queryUserRole(oid = 1)
#       
#       expect_true(UserRoleNow$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "UserRole", 
#                      event_table_name = "UserRoleEvent", 
#                      parent_field_name = "ParentUserRole")
#       
#       expect_false(queryUserRole(oid = 1)$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "UserRole", 
#                      event_table_name = "UserRoleEvent", 
#                      parent_field_name = "ParentUserRole")
#       
#       expect_true(queryUserRole(oid = 1)$IsActive)
#     }
#   )
#   
#   test_that(
#     "Verify events are written for activating and deactivating", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       conn <- connectToReportManager()
#       
#       role_oid <- max(queryRole()$OID)
#       
#       # Make a new user for the test
#       addEditUserRole(parent_user = 1, 
#                       parent_role = role_oid,
#                       event_user = 1)
#       
#       oid <- max(queryUserRole()$OID)
#       
#       activateRecord(oid = oid, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "UserRole", 
#                      event_table_name = "UserRoleEvent", 
#                      parent_field_name = "ParentUserRole")
#       
#       activateRecord(oid = oid, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "UserRole", 
#                      event_table_name = "UserRoleEvent", 
#                      parent_field_name = "ParentUserRole")
#       
#       UserRoleEvent <- 
#         dbGetQuery(conn, 
#                    sqlInterpolate(
#                      conn,
#                      switch(flavor, 
#                             "sql_server" = "SELECT * FROM dbo.UserRoleEvent WHERE ParentUserRole = ? AND EventType IN (?, ?)", 
#                             "SELECT * FROM UserRoleEvent WHERE ParentUserRole = ? AND EventType IN (?, ?)"),
#                      oid, 
#                      "Activate", 
#                      "Deactivate"
#                    ))
#       
#       expect_equal(UserRoleEvent$EventType, 
#                    c("Activate", "Deactivate", "Activate"))
#       
#       dbDisconnect(conn)
#     }
#   )
# }
# 
# 
# 
# # Schedule - SQL Server ---------------------------------------------
# 
# for (flavor in FLAVOR){
#   message(sprintf("Testing for SQL Flavor: %s\n", flavor))
#   .ready <- READY[flavor]
#   .message <- MESSAGE[flavor]
#   
#   if (.ready){
#     configureReportManager(flavor = flavor)
#   }
#   
#   test_that(
#     "Activate and Deactivate User Roles", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       ScheduleNow <- querySchedule(oid = 1)
#       
#       expect_true(ScheduleNow$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "Schedule", 
#                      event_table_name = "ScheduleEvent", 
#                      parent_field_name = "ParentSchedule")
#       
#       expect_false(querySchedule(oid = 1)$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "Schedule", 
#                      event_table_name = "ScheduleEvent", 
#                      parent_field_name = "ParentSchedule")
#       
#       expect_true(querySchedule(oid = 1)$IsActive)
#     }
#   )
#   
#   test_that(
#     "Verify events are written for activating and deactivating", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       conn <- connectToReportManager()
#       
#       # Make a new user for the test
#       addEditSchedule(schedule_name = "event test", 
#                       frequency = 1, 
#                       frequency_unit = "Day", 
#                       offset_overlap = 0, 
#                       offset_overlap_unit = "Day",
#                       event_user = 1)
#       
#       oid <- max(querySchedule()$OID)
#       
#       activateRecord(oid = oid, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "Schedule", 
#                      event_table_name = "ScheduleEvent", 
#                      parent_field_name = "ParentSchedule")
#       
#       activateRecord(oid = oid, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "Schedule", 
#                      event_table_name = "ScheduleEvent", 
#                      parent_field_name = "ParentSchedule")
#       
#       ScheduleEvent <- 
#         dbGetQuery(conn, 
#                    sqlInterpolate(
#                      conn,
#                      switch(flavor, 
#                             "sql_server" = "SELECT * FROM dbo.ScheduleEvent WHERE ParentSchedule = ? AND EventType IN (?, ?)", 
#                             "SELECT * FROM ScheduleEvent WHERE ParentSchedule = ? AND EventType IN (?, ?)"),
#                      oid, 
#                      "Activate", 
#                      "Deactivate"
#                    ))
#       
#       expect_equal(ScheduleEvent$EventType, 
#                    c("Activate", "Deactivate", "Activate"))
#       
#       dbDisconnect(conn)
#     }
#   )
# }
# 
# # DateReportingFormat - SQL Server ----------------------------------
# 
# for (flavor in FLAVOR){
#   message(sprintf("Testing for SQL Flavor: %s\n", flavor))
#   .ready <- READY[flavor]
#   .message <- MESSAGE[flavor]
#   
#   if (.ready){
#     configureReportManager(flavor = flavor)
#   }
#  
#   test_that(
#     "Activate and Deactivate User Roles", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       DateReportingFormatNow <- queryDateReportingFormat(oid = 1)
#       
#       expect_true(DateReportingFormatNow$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "DateReportingFormat", 
#                      event_table_name = "DateReportingFormatEvent", 
#                      parent_field_name = "ParentDateReportingFormat")
#       
#       expect_false(queryDateReportingFormat(oid = 1)$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "DateReportingFormat", 
#                      event_table_name = "DateReportingFormatEvent", 
#                      parent_field_name = "ParentDateReportingFormat")
#       
#       expect_true(queryDateReportingFormat(oid = 1)$IsActive)
#     }
#   )
#   
#   test_that(
#     "Verify events are written for activating and deactivating", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       conn <- connectToReportManager()
#       
#       # Make a new user for the test
#       addEditDateReportingFormat(format_name = "event test", 
#                                  description = "testing that events are written",
#                                  format_code = "%y-%d-%m",
#                                  increment_start = 0, 
#                                  increment_end = 0,
#                                  event_user = 1)
#       
#       oid <- max(queryDateReportingFormat()$OID)
#       
#       activateRecord(oid = oid, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "DateReportingFormat", 
#                      event_table_name = "DateReportingFormatEvent", 
#                      parent_field_name = "ParentDateReportingFormat")
#       
#       activateRecord(oid = oid, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "DateReportingFormat", 
#                      event_table_name = "DateReportingFormatEvent", 
#                      parent_field_name = "ParentDateReportingFormat")
#       
#       DateReportingFormatEvent <- 
#         dbGetQuery(conn, 
#                    sqlInterpolate(
#                      conn, 
#                      switch(flavor,
#                             "sql_server" = "SELECT * FROM dbo.DateReportingFormatEvent WHERE ParentDateReportingFormat = ? AND EventType IN (?, ?)", 
#                             "SELECT * FROM DateReportingFormatEvent WHERE ParentDateReportingFormat = ? AND EventType IN (?, ?)"),
#                      oid, 
#                      "Activate", 
#                      "Deactivate"
#                    ))
#       
#       expect_equal(DateReportingFormatEvent$EventType, 
#                    c("Activate", "Deactivate", "Activate"))
#       
#       dbDisconnect(conn)
#     }
#   )
# }
# 
# # Disclaimer - SQL Server -------------------------------------------
# 
# for (flavor in FLAVOR){
#   message(sprintf("Testing for SQL Flavor: %s\n", flavor))
#   .ready <- READY[flavor]
#   .message <- MESSAGE[flavor]
#   
#   if (.ready){
#     configureReportManager(flavor = flavor)
#   }
#   
#   test_that(
#     "Activate and Deactivate Users", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       ObjectNow <- queryDisclaimer(oid = 1)
#       
#       expect_true(ObjectNow$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "Disclaimer", 
#                      event_table_name = "DisclaimerEvent", 
#                      parent_field_name = "ParentDisclaimer")
#       
#       expect_false(queryDisclaimer(oid = 1)$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "Disclaimer", 
#                      event_table_name = "DisclaimerEvent", 
#                      parent_field_name = "ParentDisclaimer")
#       
#       expect_true(queryDisclaimer(oid = 1)$IsActive)
#     }
#   )
#   
#   test_that(
#     "Verify events are written for activating and deactivating", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       conn <- connectToReportManager()
#       
#       # Make a new user for the test
#       addEditDisclaimer(disclaimer = "Disclaimer for event testing", 
#                         event_user = 1)
#       
#       oid <- max(queryDisclaimer()$OID)
#       
#       activateRecord(oid = oid, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "Disclaimer", 
#                      event_table_name = "DisclaimerEvent", 
#                      parent_field_name = "ParentDisclaimer")
#       
#       activateRecord(oid = oid, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "Disclaimer", 
#                      event_table_name = "DisclaimerEvent", 
#                      parent_field_name = "ParentDisclaimer")
#       
#       EventTable <- 
#         dbGetQuery(conn, 
#                    sqlInterpolate(
#                      conn, 
#                      switch(flavor,
#                             "sql_server" = "SELECT * FROM dbo.DisclaimerEvent WHERE ParentDisclaimer = ? AND EventType IN (?, ?)", 
#                             "SELECT * FROM DisclaimerEvent WHERE ParentDisclaimer = ? AND EventType IN (?, ?)"),
#                      oid, 
#                      "Activate", 
#                      "Deactivate"
#                    ))
#       
#       expect_equal(EventTable$EventType, 
#                    c("Activate", "Deactivate", "Activate"))
#       
#       dbDisconnect(conn)
#     }
#   )
# }
# 
# # Footer - SQL Server -----------------------------------------------
# 
# for (flavor in FLAVOR){
#   message(sprintf("Testing for SQL Flavor: %s\n", flavor))
#   .ready <- READY[flavor]
#   .message <- MESSAGE[flavor]
#   
#   if (.ready){
#     configureReportManager(flavor = flavor)
#   }
#  
#   test_that(
#     "Activate and Deactivate Users", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       ObjectNow <- queryFooter(oid = 1)
#       
#       expect_true(ObjectNow$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "Footer", 
#                      event_table_name = "FooterEvent", 
#                      parent_field_name = "ParentFooter")
#       
#       expect_false(queryFooter(oid = 1)$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "Footer", 
#                      event_table_name = "FooterEvent", 
#                      parent_field_name = "ParentFooter")
#       
#       expect_true(queryFooter(oid = 1)$IsActive)
#     }
#   )
#   
#   test_that(
#     "Verify events are written for activating and deactivating", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       conn <- connectToReportManager()
#       
#       # Make a new user for the test
#       addEditFooter(footer = "Disclaimer for event testing", 
#                     event_user = 1)
#       
#       oid <- max(queryFooter()$OID)
#       
#       activateRecord(oid = oid, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "Footer", 
#                      event_table_name = "FooterEvent", 
#                      parent_field_name = "ParentFooter")
#       
#       activateRecord(oid = oid, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "Footer", 
#                      event_table_name = "FooterEvent", 
#                      parent_field_name = "ParentFooter")
#       
#       EventTable <- 
#         dbGetQuery(conn, 
#                    sqlInterpolate(
#                      conn, 
#                      switch(flavor, 
#                             "sql_server" = "SELECT * FROM dbo.FooterEvent WHERE ParentFooter = ? AND EventType IN (?, ?)",
#                             "SELECT * FROM FooterEvent WHERE ParentFooter = ? AND EventType IN (?, ?)"),
#                      oid, 
#                      "Activate", 
#                      "Deactivate"
#                    ))
#       
#       expect_equal(EventTable$EventType, 
#                    c("Activate", "Deactivate", "Activate"))
#       
#       dbDisconnect(conn)
#     }
#   )
# }
# 
# # ReportTemplate - SQL Server ---------------------------------------
# 
# for (flavor in FLAVOR){
#   message(sprintf("Testing for SQL Flavor: %s\n", flavor))
#   .ready <- READY[flavor]
#   .message <- MESSAGE[flavor]
#   
#   if (.ready){
#     configureReportManager(flavor = flavor)
#   }
#   
#   test_that(
#     "Activate and Deactivate ReportTemplates", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       ObjectNow <- queryReportTemplate(oid = 1)
#       
#       expect_true(ObjectNow$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "ReportTemplate", 
#                      event_table_name = "ReportTemplateEvent", 
#                      parent_field_name = "ParentReportTemplate")
#       
#       expect_false(queryReportTemplate(oid = 1)$IsActive)
#       
#       activateRecord(oid = 1, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "ReportTemplate", 
#                      event_table_name = "ReportTemplateEvent", 
#                      parent_field_name = "ParentReportTemplate")
#       
#       expect_true(queryReportTemplate(oid = 1)$IsActive)
#     }
#   )
#   
#   test_that(
#     "Verify events are written for activating and deactivating", 
#     {
#       skip_if_not(.ready, 
#                   .message)
#       
#       conn <- connectToReportManager()
#       
#       # Make a new user for the test
#       addEditReportTemplate(template_name = "TemplateName", 
#                             template_directory = "EventDirectory", 
#                             template_file = "EventFile",
#                             title = "Testing events are written", 
#                             title_size = "LARGE", 
#                             is_signature_required = TRUE, 
#                             is_active = TRUE,
#                             logo_oid = NA,
#                             event_user = 1)
#       
#       oid <- max(queryReportTemplate()$OID)
#       
#       activateRecord(oid = oid, 
#                      active = FALSE, 
#                      event_user = 1, 
#                      table_name = "ReportTemplate", 
#                      event_table_name = "ReportTemplateEvent", 
#                      parent_field_name = "ParentReportTemplate")
#       
#       activateRecord(oid = oid, 
#                      active = TRUE, 
#                      event_user = 1, 
#                      table_name = "ReportTemplate", 
#                      event_table_name = "ReportTemplateEvent", 
#                      parent_field_name = "ParentReportTemplate")
#       
#       EventTable <- 
#         dbGetQuery(conn, 
#                    sqlInterpolate(
#                      conn, 
#                      switch(flavor, 
#                             "sql_server" = "SELECT * FROM dbo.ReportTemplateEvent WHERE ParentReportTemplate = ? AND EventType IN (?, ?)",
#                             "SELECT * FROM ReportTemplateEvent WHERE ParentReportTemplate = ? AND EventType IN (?, ?)"),
#                      oid, 
#                      "Activate", 
#                      "Deactivate"
#                    ))
#       
#       expect_equal(EventTable$EventType, 
#                    c("Activate", "Deactivate", "Activate"))
#       
#       dbDisconnect(conn)
#     }
#   )
# }
