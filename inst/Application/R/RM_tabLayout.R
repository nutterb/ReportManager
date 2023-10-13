RM_tabLayout <- function(id_stem, 
                         add = TRUE, 
                         edit = TRUE, 
                         activate = TRUE){
  add <- 
    if (add){
      disabled(actionButton(inputId = sprintf("btn_%s_add", id_stem), 
                            label = "Add", 
                            style = "float:right"))
    } else {
      NULL
    }
  
  edit <- 
      if (edit) {
        disabled(actionButton(inputId = sprintf("btn_%s_edit", id_stem), 
                              label = "Edit", 
                              style = "float:right"))
      } else {
        NULL
      }
    
  activate <- 
        if (activate) {
          tagList(disabled(actionButton(inputId = sprintf("btn_%s_deactivate", id_stem), 
                                        label = "Deactivate", 
                                        style = "float:right")), 
                  disabled(actionButton(inputId = sprintf("btn_%s_activate", id_stem), 
                                        label = "Activate", 
                                        style = "float:right")))
        } else {
          NULL
        }
    
  tagList(add, 
          edit, 
          activate, 
          radioDataTableOutput(outputId = sprintf("dt_%s", id_stem), 
                               radioId = sprintf("rdo_%s", id_stem))
  )
}
