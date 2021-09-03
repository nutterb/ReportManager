# -------------------------------------------------------------------
# File Name: make_database_connection.R
# Description: Wrapper function to make a database connection

#' @name make_database_connection
#' @title Make a Database Connection
#' 
#' @description The reporting application is heavily dependent on 
#'   reading from and writing to its associated database. This
#'   wrapper function is intended to manage those connections.
#'
#' @param db_path \code{character(1)} For demonstration, the path 
#'   to the MS Access database with the Report Manager database.
#' @param conn Database connection to be closed.
#'   
#'   
#' @details Because the Reports Manager is designed to be adaptable
#'   to any database system, this function is one that must be 
#'   modified and customized to the unique environment where it
#'   is being implemented. In the development version on GitHub, 
#'   it connects to the MS Access Database included in the package
#'   files.
#'  
#' @author Benjamin Nutter
#' @export

make_database_connection <- function(db_path = getOption("report_manager_db_path")){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_file_exists(db_path, 
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  tryCatch(
    {
      DBI::dbConnect(
        odbc::odbc(),
        .connection_string = sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=%s;", 
                                     db_path)
      )
    },
    error = function(cond){
      warning("Failed to connect to `db_path` with error: ", cond)
    }
  )
}

#' @rdname make_database_connection
#' @export

close_database_connection <- function(conn){
  tryCatch(
    {
      DBI::dbDisconnect(conn)
    }, 
    error = function(cond){
      message("Failed to close database connection with error: ", cond)
    }
  )
}
