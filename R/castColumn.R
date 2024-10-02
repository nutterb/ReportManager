#' @name castColumn
#' @title Convert SQL Columns to R Data Types
#' 
#' @description Some SQL drivers do not convert the SQL data types 
#'   to R data types. This function performs that work to provide
#'   a consistent result when retrieving data.
#'   
#' @param data `data.frame`. The data retuned from SQL. 
#' @param table_name `character(1)`. The name of the table 
#' @param sql_flavor `character(1)`. The type of SQL connection.
#' @param conn A database connection object.
#' 

castColumn <- function(data, table_name, sql_flavor, conn, ...){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertDataFrame(x = data, 
                             add = coll)
  
  checkmate::assertCharacter(x = table_name, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = sql_flavor, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  switch(sql_flavor, 
         "sqlite" = .castColumn_sqlite(data, table_name, conn), 
         data)
}

# Unexported --------------------------------------------------------

.castColumn_sqlite <- function(data, table_name, conn){
  types <- DBI::dbGetQuery(conn, 
                           sprintf("PRAGMA table_info(%s)", table_name))
  
  bit <- types$name[grepl("^BIT$", types$type, ignore.case = TRUE)]
  datetime <- types$name[grepl("^DATETIME$", types$type, ignore.case = TRUE)]
  date <- types$name[grepl("^DATE$", types$type, ignore.case = TRUE)]
  
  data[bit] <- lapply(data[bit], 
                      as.logical)
  
  data[datetime] <- lapply(data[datetime], 
                           as.POSIXct, 
                           tz = "UTC")
  
  data[date] <- lapply(data[date], 
                       as.Date)
  
  data
}

