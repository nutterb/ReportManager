validateDateFormatInputs <- function(rv_DateFormat, 
                                   input, 
                                   is_edit, 
                                   this_format_name){
  format_name <- trimws(input$txt_dateFormat_formatName)
  description <- trimws(input$txt_dateFormat_description)
  format_code <- trimws(input$txt_dateFormat_formatCode)
  val <- inputValidationCollection()
  RM_validate_unique(val = val, 
                     input_value = format_name, 
                     selected_value = this_format_name, 
                     database_values = rv_DateFormat$DateFormat$FormatName,
                     is_edit = is_edit, 
                     object_type = "DateReportingFormat")
  if (format_name == ""){
    val$invalidate("Format Name is empty or only whitespace.")
  }
  if (description == ""){
    val$invalidate("Description is empty or only whitespace.")
  }
  if (format_code == ""){
    val$invalidate("Format Code is empty or only whitespace.")
  }
  val
}
