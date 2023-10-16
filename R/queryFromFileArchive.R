#' @name queryFromFileArchive
#' @title Get a File from the FileArchive
#' 
#' @description This function enables the user to retrieve a file from the 
#'   FileArchive table. 
#'   
#' @param oid `integerish(1)`. The OID of the file to retrieve from the 
#'   FileArchive. 
#' @param file_dir `character(0/1)`. A directory to which the file is to 
#'   be saved. If the length is zero, the save is skipped and only the
#'   data frame with the file BLOB object is returned.
#'
#' @seealso
#' [queryFileArchive()], \cr
#' [queryLogo()]
#'   
#' @export

queryFromFileArchive <- function(oid, 
                                 file_dir = character(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = file_dir, 
                             max.len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })
  
  statement <- 
    switch(getOption("RM_sql_flavor"), 
           "sql_server" = .queryFromFileArchive_statement_sqlServer, 
           "sqlite"     = .queryFromFileArchive_statement_sqlite)
  
  File <- 
    DBI::dbGetQuery(
      conn, 
      DBI::sqlInterpolate(
        conn, 
        statement, 
        oid))
  
  if (getOption("RM_sql_flavor") == "sqlite"){
    File$IsLogo <- as.logical(File$IsLogo)
  }
  
  if (length(file_dir) > 0){
    File$SavedTo <- file.path(file_dir, 
                              sprintf("%s.%s", 
                                      File$FileName, 
                                      File$FileExtension))
    
    writeBin(as.raw(File$FileContent[[1]]), 
             con = File$SavedTo)
  }
  
  File
}

# Unexported --------------------------------------------------------

.queryFromFileArchive_statement_sqlServer <- "
  SELECT [OID], 
         [ParentReportTemplate], 
         [ParentReportInstance], 
         [Description], 
         [CreatedDateTime], 
         [IsLogo], 
         [FileName],
         [FileExtension], 
         [FileSize], 
         [FileContent]
  FROM dbo.[FileArchive]
  WHERE [OID] = ?
"

.queryFromFileArchive_statement_sqlite <- "
  SELECT [OID], 
         [ParentReportTemplate], 
         [ParentReportInstance], 
         [Description], 
         [CreatedDateTime], 
         [IsLogo], 
         [FileName],
         [FileExtension], 
         [FileSize], 
         [FileContent]
  FROM [FileArchive]
  WHERE [OID] = ?
"
