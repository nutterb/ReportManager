#' @name addReportInstanceNote
#' @title Add a Note to a Report Instance
#' 
#' @description Adds a note to the collection of notes associated with a
#'   report instance.
#'   
#' @param report_instance_oid `integerish(1)`. The OID of the report instance.
#' @param parent_user `integerish(1)`. The user adding the note.
#' @param note `character(1)`. The text of the note to be added. If the note 
#'   consists of entirely whitespace, the function will silently skip writing
#'   any record to the database. White space will be trimmed before writing 
#'   to the database.
#' @param note_date_time `POSIXct(1)`. The date and time the note is added.
#' 
#' @export

addReportInstanceNote <- function(report_instance_oid, 
                                  parent_user, 
                                  note,
                                  note_date_time = Sys.time()){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_user, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = note, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertPOSIXct(x = note_date_time, 
                           len = 1, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  if (trimws(note) == "") return(NULL)
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- switch(getOption("RM_sql_flavor"), 
                      "sqlite" = .addReportInstanceNote_sqlite, 
                      "sql_server" = .addReportInstanceNote_sqlServer, 
                      stop(sprintf("Query not defined for SQL flavor '%s'",
                                   getOption("RM_sql_flavor"))))
  
  result <- 
    DBI::dbSendStatement(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        ParentReportInstance = report_instance_oid, 
        ParentUser = parent_user, 
        Note = trimws(note), 
        NoteDateTime = format(note_date_time, 
                              format = "%Y-%m-%d %H:%M:%S")
      )
    )
  
  DBI::dbClearResult(result)
}

# Unexported --------------------------------------------------------

.addReportInstanceNote_sqlite <- "
  INSERT INTO ReportInstanceNote
  (ParentReportInstance, ParentUser, Note, NoteDateTime)
  VALUES
  (?ParentReportInstance, ?ParentUser, ?Note, ?NoteDateTime)
"

.addReportInstanceNote_sqlServer <- "
  INSERT INTO dbo.ReportInstanceNote
  (ParentReportInstance, ParentUser, Note, NoteDateTime)
  VALUES
  (?ParentReportInstance, ?ParentUser, ?Note, ?NoteDateTime)
"
