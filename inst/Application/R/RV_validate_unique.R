RM_validate_unique <- function(val, 
                               input_value, 
                               selected_value, 
                               database_values,
                               is_edit, 
                               object_type){
  duplicate_message <- 
    sprintf("%s '%s' already exists in the database. Duplicates are not allowed.", 
            object_type,
            input_value)
  
  if (is_edit){
    if ((selected_value != input_value) &&
        input_value %in% database_values){
      val$invalidate(duplicate_message)
    } 
  } else {
    if (isTRUE(input_value %in% database_values)){
      val$invalidate(duplicate_message)
    }
  }
}
