validateScheduleInputs <- function(rv_Schedule, 
                                   input, 
                                   is_edit, 
                                   this_schedule_name){
  schedule_name <- trimws(input$txt_schedule_scheduleName)
  val <- inputValidationCollection()
  RM_validate_unique(val = val, 
                     input_value = schedule_name, 
                     selected_value = this_schedule_name, 
                     database_values = rv_Schedule$Schedule$ScheduleName,
                     is_edit = is_edit, 
                     object_type = "Schedule")
  if (schedule_name == ""){
    val$invalidate("Schedule Name is empty or only whitespace")
  }
  val
}
