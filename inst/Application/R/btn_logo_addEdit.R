..btn_logo_addEdit <- function(session, 
                                rv_Logo, 
                                input, 
                                proxy){
  oid <- as.numeric(input$rdo_logo)
  if (rv_Logo$AddEdit == "Add"){
    req(input$file_logo_add)
    addLogo(file_path = input$file_logo_add$datapath, 
            file_name = tools::file_path_sans_ext(input$file_logo_add$name))
  } else {
    editLogo(oid = oid, 
             description = input$txt_logo_add_description, 
             file_name = input$txt_logo_add_fileName)
  }
  RM_replaceData(query_fun = queryLogo, 
                 reactive_list = rv_Logo, 
                 data_slot = "Logo", 
                 selected_slot = "SelectedLogo", 
                 id_variable = "OID", 
                 element_name = "rdo_logo", 
                 oid = oid, 
                 proxy = proxy)
  toggleModal(session = session, 
              modalId = "modal_logo_addEdit", 
              toggle = "close")
}
