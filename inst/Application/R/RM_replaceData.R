RM_replaceData <- function(query_fun, 
                           reactive_list, 
                           data_slot, 
                           selected_slot, 
                           id_variable, 
                           element_name, 
                           oid, 
                           proxy){
  NewData <- query_fun()
  reactive_list[[data_slot]] <- NewData
  reactive_list[[selected_slot]] <- NewData[NewData[[id_variable]] == oid, ]
  
  NewData %>% 
    radioDataTable(id_variable = id_variable, 
                   element_name = element_name, 
                   checked = as.character(oid)) %>% 
    DT::replaceData(proxy = proxy, 
                    data = ., 
                    resetPaging = FALSE, 
                    rownames = FALSE)
  
}
