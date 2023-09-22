#' @name insertRecord 
#' @title Insert and Update Records
#' 
#' @description Generalized forms for writing and executing statements
#'   to add new records or update existing records. 
#'   
#' @export

insertRecord <- function(data, 
                         table_name, 
                         return_oid = TRUE,
                         flavor     = getOption("RM_sql_flavor"),
                         id_field   = "OID", 
                         schema     = "dbo"){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertDataFrame(x         = data, 
                             min.rows  = 1, 
                             col.names = "named")
  
  checkmate::assertCharacter(x   = table_name, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertLogical(x   = return_oid, 
                           len = 1, 
                           add = coll)
  
  flavor <- checkmate::matchArg(x       = flavor, 
                                choices = SUPPORTED_SQL_FLAVOR, 
                                add     = coll)
  
  checkmate::assertCharacter(x   = id_field, 
                             len = 1,
                             add = coll)
  
  checkmate::assertCharacter(x   = schema, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  statement <- .insertRecord_makeInsertStatement(data       = data, 
                                                 table_name = table_name, 
                                                 return_oid = return_oid,
                                                 flavor     = flavor,
                                                 id_field   = id_field, 
                                                 schema     = schema)
  
  .insertRecord_insertRecord(statement  = statement, 
                             data       = data, 
                             id_field   = id_field, 
                             return_oid = return_oid)
}

# Unexported --------------------------------------------------------

.insertRecord_makeInsertStatement <- function(data,
                                table_name,
                                return_oid, 
                                flavor,
                                id_field,
                                schema){
  insert <- sprintf("INSERT INTO %s[%s]", 
                    if (flavor == "sql_server") "dbo." else "", 
                    table_name)
  
  property <- sprintf("[%s]", names(data))
  property <- paste0(property, collapse = ", ")
  property <- sprintf("(%s)", 
                      property)
  
  value <- rep("?", length(data))
  value <- paste0(value, collapse = ", ")
  value <- sprintf("(%s)", value)
  
  paste(insert, 
        property, 
        if (flavor == "sql_server" && return_oid == TRUE) sprintf("OUTPUT INSERTED.[%s]", id_field) else character(0), 
        "VALUES", 
        value, 
        sep = "\n")
}


.insertRecord_insertRecord <- function(statement, 
                                       data, 
                                       id_field, 
                                       return_oid){
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  NewID <- vector("list", nrow(data))
  
  for (i in seq_len(nrow(data))){
    result <- DBI::dbSendStatement(conn, 
                                   statement, 
                                   unname(data[i, ]))
    if (getOption("RM_sql_flavor") == "sqlite" && return_oid == TRUE){
      DBI::dbClearResult(result)
      result <- DBI::dbSendStatement(conn, 
                                     sprintf("SELECT last_insert_rowid() AS %s", 
                                             id_field))
    }
    
    if (return_oid == TRUE){
      NewID[[i]] <- DBI::dbFetch(result)
    }
    DBI::dbClearResult(result)
  }
  
  if (return_oid == TRUE){
    do.call("rbind", NewID)
  }
}


# UPDATE ------------------------------------------------------------

#' @rdname insertRecord
#' @export

updateRecord <- function(data, 
                         where_data, 
                         table_name,
                         flavor = getOption("RM_sql_flavor"),
                         schema = "dbo"){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertDataFrame(x         = data, 
                             min.rows  = 1, 
                             col.names = "named", 
                             add       = coll)
  
  checkmate::assertDataFrame(x         = where_data, 
                             min.rows  = 1, 
                             col.names = "named", 
                             add       = coll)
  
  checkmate::assertCharacter(x   = table_name, 
                             len = 1, 
                             add = coll)
  
  flavor <- checkmate::matchArg(x       = flavor, 
                                choices = SUPPORTED_SQL_FLAVOR,
                                add     = coll)
  
  checkmate::assertCharacter(x   = schema, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  name_conflict <- intersect(names(data), 
                             names(where_data))
  
  if (length(name_conflict) > 0){
    coll$push(sprintf("Shared names between `data` and `where_data` are not permitted: %s", 
                      paste0(name_conflict, collapse = ", ")))
  }
  
  if (nrow(data) != nrow(where_data)){
    coll$push("`data` and `where_data` must have the same number of rows.")
  }
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  statement <- .updateRecord_makeUpdateStatement(data       = data, 
                                                 where_data = where_data, 
                                                 table_name = table_name, 
                                                 flavor     = flavor, 
                                                 schema     = schema)

  data <- cbind(data, where_data)
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  for (i in seq_len(nrow(data))){
    result <- DBI::dbSendStatement(conn, 
                                   statement, 
                                   unname(data[i, ]))
    
    DBI::dbClearResult(result)
  }
}

# Unexported --------------------------------------------------------

.updateRecord_makeUpdateStatement <- function(data, 
                                              where_data, 
                                              table_name, 
                                              flavor, 
                                              schema){
  update <- sprintf("UPDATE %s[%s]", 
                    if (flavor == "sql_server") "dbo." else "", 
                    table_name)
  
  set <- sprintf("%s = ?", 
                 names(data))
  set <- paste0(set, collapse = ",\n    ")
  set <- paste0("SET ", set)
  
  where <- sprintf("%s = ?", 
                   names(where_data))
  where <- paste0(where, collapse = ",\n      AND ")
  where <- paste0("WHERE ", where)
  
  paste(update, set, where, sep = "\n")
}
