# Argument Validation -----------------------------------------------

test_that(
  "cast an error if oid is not integerish(0/1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = "1", 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'oid': Must be of type 'integerish'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = 1:2, 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'oid': Must have length [<][=] 1"
    )
  }
)

test_that(
  "cast an error if parent_report_template is not integerish(0/1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = "1", 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'parent_report_template': Must be of type 'integerish'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1:2, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'parent_report_template': Must have length [<][=] 1"
    )
  }
)

test_that(
  "cast an error if parent_role is not integerish(0/1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = "1", 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'parent_role': Must be of type 'integerish'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1:2, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'parent_role': Must have length [<][=] 1"
    )
  }
)

test_that(
  "can an error if can_view is not logical(1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = "TRUE", 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_view': Must be of type 'logical'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = c(FALSE, TRUE), 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_view': Must have length 1"
    )
  }
)

test_that(
  "can an error if can_add_notes is not logical(1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = "TRUE", 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_add_notes': Must be of type 'logical'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = c(FALSE, TRUE), 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_add_notes': Must have length 1"
    )
  }
)

test_that(
  "can an error if can_edit_narrative is not logical(1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = "TRUE", 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_edit_narrative': Must be of type 'logical'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = c(FALSE, TRUE), 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_edit_narrative': Must have length 1"
    )
  }
)

test_that(
  "can an error if can_submit is not logical(1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = "TRUE", 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_submit': Must be of type 'logical'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = c(FALSE, TRUE), 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_submit': Must have length 1"
    )
  }
)

test_that(
  "can an error if can_start_revision is not logical(1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = "TRUE", 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_start_revision': Must be of type 'logical'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = c(FALSE, TRUE), 
                                      is_active = TRUE, 
                                      event_user = 1), 
      "'can_start_revision': Must have length 1"
    )
  }
)

test_that(
  "can an error if is_active is not logical(1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = "TRUE", 
                                      event_user = 1), 
      "'is_active': Must be of type 'logical'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = numeric(0), 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = c(FALSE, TRUE), 
                                      event_user = 1), 
      "'is_active': Must have length 1"
    )
  }
)

test_that(
  "cast an error if event_user is not integerish(0/1)", 
  {
    expect_error(
      addEditReportTemplatePermission(oid = 1, 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = "1"), 
      "'event_user': Must be of type 'integerish'"
    )
    
    expect_error(
      addEditReportTemplatePermission(oid = 1, 
                                      parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
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
    purgeReportManagerDatabase()
    initializeUiTestingDatabase(SQL_FILE[flavor], 
                                include = c("User", "Role", "UserRole", 
                                            "ReportTemplate"))
  }
  
  test_that(
    "New Permissions can be added",
    {
      skip_if_not(.ready, 
                  .message)
      
      CurrentPermission <- queryReportTemplatePermission(parent_report_template = 1)
      
      next_oid <- nrow(CurrentPermission) + 1
      
      addEditReportTemplatePermission(parent_report_template = 1, 
                                      parent_role = 1, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1)
      NewPermission <- queryReportTemplatePermission(parent_report_template = 1)
      
      expect_data_frame(NewPermission)
      expect_equal(nrow(NewPermission), nrow(CurrentPermission) + 1)
    }
  )
  
  test_that(
    "Cast an error if a record exists for an existing template-role",
    {
      skip_if_not(.ready, 
                  .message)
      
      expect_error(addEditReportTemplatePermission(parent_report_template = 1, 
                                                   parent_role = 1, 
                                                   can_view = TRUE, 
                                                   can_add_notes = TRUE, 
                                                   can_edit_narrative = TRUE, 
                                                   can_submit = TRUE, 
                                                   can_start_revision = TRUE, 
                                                   is_active = TRUE, 
                                                   event_user = 1), 
                   "ReportTemplatePermission record for ReportTemplate.OID .+ already exists")
    }
  )
  
  test_that(
    "Edit an existing record", 
    {
      skip_if_not(.ready, 
                  .message)
      
      CurrentPermission <- queryReportTemplatePermission(oid = 1)
      
      addEditReportTemplatePermission(oid = 1,
                                      parent_report_template = 1, 
                                      parent_role = 1,
                                      can_view = FALSE, 
                                      can_add_notes = FALSE, 
                                      can_edit_narrative = FALSE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1)
      
      NewPermission <- queryReportTemplatePermission(oid = 1)
      
      expect_equal(CurrentPermission$CanView, 
                   !NewPermission$CanView)
      expect_equal(CurrentPermission$CanAddNotes, 
                   !NewPermission$CanAddNotes)
      expect_equal(CurrentPermission$CanEditNarrative, 
                   !NewPermission$CanEditNarrative)
    }
  )
  
  test_that(
    "Confirm events are recorded correctly", 
    {
      skip_if_not(.ready, 
                  .message)
      
      conn <- connectToReportManager()
      
      last_oid <- max(queryReportTemplatePermission()$OID)
      next_oid <- last_oid + 1
      
      addEditReportTemplatePermission(parent_report_template = 1, 
                                      parent_role = 2, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = TRUE, 
                                      event_user = 1)
      
      PermissionEvent <- 
        dbGetQuery(conn, 
                   sqlInterpolate(
                     conn,
                     switch(flavor, 
                            "sql_server" = "SELECT * FROM dbo.ReportTemplatePermissionEvent WHERE ParentReportTemplatePermission = ?", 
                            "SELECT * FROM ReportTemplatePermissionEvent WHERE ParentReportTemplatePermission = ?"),
                     next_oid))
      
      expect_equal(PermissionEvent$EventType, 
                   c("SetCanView", "SetCanAddNotes", "SetCanEditNarrative", "SetCanSubmit", 
                     "SetCanStartRevision", "Activate", "Add"))
      expect_true(all(table(PermissionEvent$EventType) == 1))
      
      addEditReportTemplatePermission(oid = next_oid,
                                      parent_report_template = 1, 
                                      parent_role = 2, 
                                      can_view = TRUE, 
                                      can_add_notes = TRUE, 
                                      can_edit_narrative = TRUE, 
                                      can_submit = TRUE, 
                                      can_start_revision = TRUE, 
                                      is_active = FALSE, 
                                      event_user = 1)
      
      PermissionEvent2 <- 
        dbGetQuery(conn, 
                   sqlInterpolate(
                     conn,
                     switch(flavor, 
                            "sql_server" = "SELECT * FROM dbo.ReportTemplatePermissionEvent WHERE ParentReportTemplatePermission = ?", 
                            "SELECT * FROM ReportTemplatePermissionEvent WHERE ParentReportTemplatePermission = ?"),
                     next_oid))
      
      expect_true("Deactivate" %in% PermissionEvent2$EventType)
      
      expect_true(
        all(table(PermissionEvent2$EventType) == 1)
      )
    }
  )
  
}