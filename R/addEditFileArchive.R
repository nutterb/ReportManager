#' @name addEditFileArchive
#' @title Add or Edit Files to the FileArchive
#' 
#' @details Enables the user to upload a file to the File Archive. 
#'   Also used to edit the file name or description of an existing 
#'   FileArchive object. Edits to FileArchive objects are 
#'   not event tracked. 
#'   
#' @param oid `integerish(0/1)`. The OID of the FileArchive object to edit.
#'   When this has length 0, a new file is uploaded. 
#' @param parent_report_template `integerish(0/1)`. The ReportTemplate OID
#'   associated with the file. Use `NA` for an unassociated file. Must 
#'   be length 1 for `addFileArchive`. For `editFileArchive`, a length 
#'   1 object is used prompt an update.
#' @param parent_report_instance `integerish(0/1)`. The ReportInstance OID
#'   associated with the file. Use `NA` for an unassociated file. Must 
#'   be length 1 for `addFileArchive`. For `editFileArchive`, a length 
#'   1 object is used prompt an update.
#' @param description `character(0/1)`. An optional description to provide 
#'   for the file. 
#' @param is_logo `logical(0/1)`. When `TRUE`, the file is flagged as a 
#'   logo file. Must be length 1 for `addFileArchive`. For `editFileArchive`, 
#'   a length 1 object is used prompt an update.
#' @param file_path `character(1)`. The path to the file being uploaded. 
#'   The file will be converted to a `raw` vector. 
#' @param file_name `character(0/1)`. The name of the file to store in the
#'   database. If length 0, it will be extracted from `file_path`. Must 
#'   be length 1 for `addFileArchive`. For `editFileArchive`, a length 
#'   1 object is used prompt an update.
#' @param file_extension `character(1)`. The file extension. If length 0, 
#'   it will be extracted from `file_path`.
#'
#' @export

addFileArchive <- function(parent_report_template = NA_real_,
                           parent_report_instance = NA_real_,
                           description = "", 
                           is_logo = FALSE, 
                           file_path,
                           file_name = tools::file_path_sans_ext(basename(file_path)), 
                           file_extension = tools::file_ext(file_path)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              len = 1, 
                              any.missing = TRUE,
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_instance, 
                              len = 1, 
                              any.missing = TRUE,
                              add = coll)
  
  checkmate::assertString(x = description, 
                          max.chars = 250, 
                          add = coll)
  
  checkmate::assertLogical(x = is_logo, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertCharacter(x = file_path, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertString(x = file_name, 
                          max.chars = 250, 
                          add = coll)
  
  checkmate::assertString(x = file_extension, 
                          max.chars = 15, 
                          add = coll)
  
  checkmate::assertCharacter(x = file_extension, 
                             max.len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assertFileExists(x = file_path, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  conn <- connectToReportManager()
  on.exit({ DBI::dbDisconnect(conn) })

  description <- trimws(description)
  file_name <- trimws(file_name)
  file_extension <- trimws(file_extension)
  file_size <- file.info(file_path)[["size"]]
  content <- readBin(file_path, 
                     what = "raw", 
                     n = file_size)
  
  AddData <- 
    data.frame(ParentReportTemplate = parent_report_template, 
               ParentReportInstance = parent_report_instance, 
               Description = description, 
               CreatedDateTime = format(Sys.time(),
                                        format = "%Y-%m-%d %H:%M:%S"), 
               IsLogo = as.numeric(is_logo), 
               FileName = file_name, 
               FileExtension = file_extension, 
               FileSize = file_size, 
               FileContent = I(list(content)))

  insertRecord(AddData, 
               table_name = "FileArchive", 
               return_oid = FALSE)
}

#' @rdname addEditFileArchive
#' @export

editFileArchive <- function(oid, 
                            parent_report_template = numeric(0), 
                            parent_report_instance = numeric(0), 
                            description = character(0), 
                            file_name = character(0), 
                            is_logo = logical(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_template, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertIntegerish(x = parent_report_instance, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = description, 
                             max.len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = file_name, 
                             max.len = 1, 
                             add = coll)
  
  checkmate::assertLogical(x = is_logo, 
                           max.len = 1, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (length(description) > 0){
    checkmate::assertString(x = description, 
                            max.chars = 250, 
                            add = coll)
  }
  
  if (length(file_name) > 0){
    checkmate::assertString(x = file_name, 
                            max.chars = 250, 
                            add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  EditData <- list(ParentReportTemplate = parent_report_template, 
                   ParentReportInstance = parent_report_instance, 
                   Description = description, 
                   FileName = file_name, 
                   IsLogo = is_logo)
  
  EditData <- EditData[lengths(EditData) > 0]
  EditData <- as.data.frame(EditData)
  
  if (nrow(EditData) > 0){
    updateRecord(EditData, 
                 where_data = data.frame(OID = rep(oid, nrow(EditData))), 
                 table_name = "FileArchive")
  }
}
