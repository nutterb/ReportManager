# Argument Validation -----------------------------------------------

test_that(
  "Return an error if oid is not integerish(0/1)", 
  {
    expect_error(
      queryApplicationSetting(oid = "1"),
      "'oid': Must be of type 'integerish'"
    )
    
    expect_error(
      queryApplicationSetting(oid = 1:2), 
      "'oid': Must have length <= 1"
    )
  }
)

test_that(
  "Return an error if setting_key is not character(1)", 
  {
    expect_error(
      queryApplicationSettingByKey(setting_key = 1),
      "'setting_key': Must be of type 'character'"
    )
    
    expect_error(
      queryApplicationSettingByKey(setting_key = letters),
      "'setting_key': Must have length 1"
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
    "queryApplicationSetting returns appropriate data", 
    {
      AllSetting <- queryApplicationSetting()
      
      expect_data_frame(AllSetting, 
                        ncols = 3)
      expect_true(nrow(AllSetting) > 1)
      
      OneSetting <- queryApplicationSetting(oid = 1)
      
      expect_data_frame(OneSetting, 
                        ncols = 3, 
                        nrows = 1)
    }
  )
  
  test_that(
    "queryApplicationSettingByKey returns appropriate data", 
    {
      Setting <- queryApplicationSettingByKey("htmlEmbed")
      
      expect_data_frame(Setting, 
                        nrows = 1, 
                        ncols = 3)
      
      expect_equal(Setting$SettingValue, 
                   "embed")
    }
  )
}
