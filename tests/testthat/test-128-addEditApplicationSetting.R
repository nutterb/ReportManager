# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(
      addEditApplicationSetting(oid = "1", 
                                setting_key = "thisSetting", 
                                setting_value = "thisValue", 
                                event_user = 1), 
      "'oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addEditApplicationSetting(oid = 1:2, 
                                setting_key = "thisSetting", 
                                setting_value = "thisValue", 
                                event_user = 1), 
      "'oid': Must have length [<][=] 1"
    )
  }
)

test_that(
  "Return an error if setting_key is not character(1)", 
  {
    expect_error(
      addEditApplicationSetting(setting_key = 1, 
                                setting_value = "thisValue", 
                                event_user = 1), 
      "'setting_key': Must be of type 'character'"
    )
    
    expect_error(
      addEditApplicationSetting(setting_key = rep("thisSetting", 2),
                                setting_value = "thisValue", 
                                event_user = 1), 
      "'setting_key': Must have length 1"
    )
  }
)

test_that(
  "Return an error if setting_value is not character(1)", 
  {
    expect_error(
      addEditApplicationSetting(setting_key = "thisKey", 
                                setting_value = 1, 
                                event_user = 1), 
      "'setting_value': Must be of type 'character'"
    )
    
    expect_error(
      addEditApplicationSetting(setting_key = "thisKey",
                                setting_value = rep("thisValue", 2),
                                event_user = 1), 
      "'setting_value': Must have length 1"
    )
  }
)

test_that(
  "Return an error if event_user is not integerish(1)", 
  {
    expect_error(
      addEditApplicationSetting(setting_key = "thisKey", 
                                setting_value = "thisValue", 
                                event_user = "1"), 
      "'event_user': Must be of type 'integerish'"
    )
    
    expect_error(
      addEditApplicationSetting(setting_key = "thisKey",
                                setting_value = "thisValue",
                                event_user = 1:2), 
      "'event_user': Must have length 1"
    )
  }
)

# Functionality -----------------------------------------------------

for (flavor in FLAVOR){
  message(sprintf("Testing for SQL Flavor: %s\n", flavor))
  .ready <- READY[flavor]
  .message <- MESSAGE[flavor]
  
  if (.ready){
    configureReportManager(flavor = flavor)
  }
  
  test_that(
    "Add a new AutoDistribution record", 
    {
      skip_if_not(.ready, 
                  .message)
      
      Current <- queryApplicationSetting()
      
      next_oid <- nrow(Current) + 1
      
      addEditApplicationSetting(setting_key = "newSettingKey", 
                                setting_value = "newSettingValue", 
                                event_user = 1)
      
      New <- queryApplicationSetting()
      
      expect_equal(nrow(Current) + 1, 
                   nrow(New))
    }
  )
  
  test_that(
    "Edit an existing record", 
    {
      skip_if_not(.ready, 
                  .message)
      
      SettingEdit <- queryApplicationSettingByKey(setting_key = "newSettingKey")
      
      oid <- SettingEdit$OID
      
      addEditApplicationSetting(oid = oid, 
                                setting_key = "newSettingKey", 
                                setting_value = "changedSettingValue", 
                                event_user = 1)
      
      Updated <- queryApplicationSetting(oid = oid)
      
      expect_equal(Updated$SettingValue, 
                   "changedSettingValue")
    }
  )
  
  test_that(
    "Confirm Events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      oid <- queryApplicationSettingByKey(setting_key = "newSettingKey")$OID
      
      Event <- dbGetQuery(conn,
                          sqlInterpolate(
                            conn,
                            switch(flavor,
                                   "sql_server" = "SELECT * FROM dbo.ApplicationSettingEvent WHERE ParentApplicationSetting = ?",
                                   "SELECT * FROM ApplicationSettingEvent WHERE ParentApplicationSetting = ?"),
                                    oid))
      
      expect_true(
        all(table(Event$EventType) == 
              c("EditKey" = 1, 
                "EditValue" = 2)))
      
      dbDisconnect(conn)
    }
  )
}
