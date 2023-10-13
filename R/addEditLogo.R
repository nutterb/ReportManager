#' @name addEditLogo
#' @title Add a Logo File or Edit Properties of an Existing Logo
#' 
#' @details Enables the user to upload a file to the File Archive to use
#'   as a logo in report titles. Also used to edit the file name or 
#'   description of an existing logo file. Edits to logo files are 
#'   not event tracked. 
#'   
#' @param oid `integerish(1)`. The OID of the FileArchive object to edit.
#' @param file_path `character(1)`. The path to the file being uploaded. 
#'   The file will be converted to a `raw` vector. 
#' @param description `character(1)`. An optional description to provide 
#'   for the file. 
#' @param file_name `character(1)`. The name of the file to store in the
#'   database. 
#' @param file_extension `character(1)`. The file extension of the file. 
#'   This will typically be drawn from the file being uploaded.
#'
#' @export

addLogo <- function(file_path, 
                    description = "", 
                    file_name = tools::file_path_sans_ext(basename(file_path)), 
                    file_extension = tools::file_ext(file_path)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = file_path, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertString(x = description, 
                          max.chars = 250, 
                          add = coll)
  
  checkmate::assertString(x = file_name, 
                          max.chars = 250, 
                          add = coll)
  
  checkmate::assertString(x = file_extension, 
                          max.chars = 15, 
                          add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assertFileExists(x = file_path, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  addFileArchive(description = description, 
                 is_logo = TRUE, 
                 file_path = file_path, 
                 file_name = file_name, 
                 file_extension = file_extension)
}

#' @rdname addEditLogo
#' @export

editLogo <- function(oid, 
                     description = character(0), 
                     file_name = character(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = oid, 
                              len = 1, 
                              add = coll)
  
  if (length(description) > 0){
    checkmate::assertString(x = description, 
                            max.chars = 250, 
                            add = coll)
  } else {
    checkmate::assertCharacter(x = description, 
                               max.len = 1, 
                               add = coll)
  }
  
  if (length(file_name) > 0){
    checkmate::assertString(x = file_name, 
                            max.chars = 250, 
                            add = coll)
  } else {
    checkmate::assertCharacter(x = file_name, 
                               max.len = 1, 
                               add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  editFileArchive(oid = oid, 
                  description = description, 
                  file_name = file_name)
}
