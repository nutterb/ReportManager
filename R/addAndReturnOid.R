#' @name addAndReturnOid
#' @title Add a Record and Return the OID
#' 
#' @description Add a new record to a table and return the OID of the 
#'   new record. This manages the record creation, statement closing, 
#'   and differences between SQL flavors.
#'   
#' 

addAndReturnOid <- function(conn, statement, arg_list){
  result <- DBI::dbSendStatement(conn, 
                                 statement, 
                                 arg_list)
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    DBI::dbClearResult(result)
    result <- DBI::dbSendStatement(conn, 
                                   "SELECT last_insert_rowid() AS OID")
  }
  
  Inserted <- DBI::dbFetch(result)
  
  DBI::dbClearResult(result)
  
  Inserted
}
