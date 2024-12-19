#' @name generateReportFiles
#' @title Generate a Report and Supplemental Files
#' 
#' @description Generates the report, supplemental data, and image files
#' that are included in the zip files or attached to an e-mail.
#' 
#' @param report_instance_oid `integerish(1)`. The OID of the report 
#'   instance. 
#' @param report_filename `character(1)`. full path to the file created 
#'   by `rmarkdown::build`. Typically this is used to locate supplemental
#'   files, such as images and supporting data.
#' @param zipfile `character(1)`. The zip file to which the contents of
#'   the report preview will be saved. 
#' @param files_to_zip `character`. The full path to the files to be included
#'   in the zip file. Include the report file, images, and supplemental data.
#' @param include_image `logical(1)`. When `TRUE`, any images that are 
#'   created as part of the report will be included in the supplemental
#'   files with the preview.
#' @param include_data `logical(1)`. When `TRUE`, supporting data generated
#'   by the report will be included with the supplemental files. Note: this
#'   is unrelated to the option to include data with report submissions that
#'   is part of the report template.
#' @param build_dir `character(1)`. The directory into which the report
#'   files will be saved.
#' @param params `list`. Optional parameters to send to the report template.
#'   Note: the report intance OID, report template OID, and `build_dir` are 
#'   included already.
#' @param report_format `character(1)`. One of `c("html", "pdf")`. The 
#'   format of the report being generated.
#' @param is_preview `logical(1)`. When `TRUE`, the report is generated as
#'   a preview. Otherwise it is generated ready for archival.
#' @param is_submission `logical(1)`. When `TRUE`, the report is generated
#'   as a submission (with a revision number).


#' @rdname generateReportFiles
#' @export


makeReportPreview <- function(report_instance_oid, 
                              zipfile,
                              include_image = FALSE, 
                              include_data = FALSE,
                              build_dir = tempdir(), 
                              params = list(), 
                              report_format = c("html", "pdf")){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = zipfile, 
                             len = 1, 
                             null.ok = TRUE,
                             add = coll)
  
  checkmate::assertLogical(x = include_image, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = include_data, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertCharacter(x = build_dir, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertList(x = params, 
                        names = "named", 
                        add = coll)
  
  report_format <- checkmate::matchArg(x = report_format, 
                                       choices = c("html", "pdf"), 
                                       .var.name = "report_format", 
                                       add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  # Initialize values that are conditionally defined.
  image_file <- character(0)
  data_file <- character(0)
  
  report_filename <- 
    generateReportFile(report_instance_oid = report_instance_oid, 
                       is_preview = TRUE,
                       is_submission = FALSE,
                       params = params,
                       build_dir = build_dir, 
                       report_format = report_format)
  
  if (include_image){
    image_file <- getReportImages(report_filename = report_filename, 
                                  report_format = report_format)
  }
  
  if (include_data){
    data_file <- getSupportingData(report_filename = report_filename)
  }
  
  files_to_zip = c(report_filename, 
                   image_file, 
                   data_file)
  
  zipReportFiles(zipfile = zipfile, 
                 files_to_zip = files_to_zip)
} 

#' @rdname generateReportFiles
#' @export


makeReportForArchive <- function(report_instance_oid, 
                                 include_data = FALSE,
                                 is_submission = FALSE,
                                 build_dir = tempdir(), 
                                 params = list(), 
                                 report_format = c("html", "pdf")){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)

  checkmate::assertLogical(x = include_data, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_submission, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertCharacter(x = build_dir, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertList(x = params, 
                        names = "named", 
                        add = coll)
  
  report_format <- checkmate::matchArg(x = report_format, 
                                       choices = c("html", "pdf"), 
                                       .var.name = "report_format", 
                                       add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  # Initialize values that are conditionally defined.
  data_file <- character(0)
  
  report_filename <- 
    generateReportFile(report_instance_oid = report_instance_oid, 
                       is_preview = FALSE,
                       is_submission = is_submission,
                       params = params,
                       build_dir = build_dir, 
                       report_format = report_format)
  
  if (include_data){
    data_file <- getSupportingData(report_filename = report_filename)
  }
  
  c(report_filename, 
    data_file)
}


# Unexported --------------------------------------------------------

generateReportFile <- function(report_instance_oid, 
                               is_preview, 
                               is_submission,
                               params = list(),
                               build_dir = tempdir(),
                               report_format = c("html", "pdf")){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertLogical(x = is_preview, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_submission, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertList(x = params, 
                        names = "named", 
                        add = coll)
  
  checkmate::assertCharacter(x = build_dir, 
                             len = 1, 
                             add = coll)
  
  report_format <- checkmate::matchArg(x = report_format, 
                                       choices = c("html", "pdf"), 
                                       .var.name = "report_format", 
                                       add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  ReportInstance <- queryReportInstance(report_instance_oid = report_instance_oid)
  
  template_dir <- system.file("ReportTemplate", 
                              package = "ReportManager")
  
  report_filename <- 
    makeReportFileName(report_instance_oid = report_instance_oid, 
                       is_preview = is_preview, 
                       is_submission = is_submission, 
                       file_extension = report_format)
  
  rmarkdown::render(
    input = file.path(template_dir, 
                      "ReportLayout.Rmd"), 
    output_format = sprintf("%s_document", 
                            report_format), 
    output_dir = build_dir, 
    output_file = report_filename, 
    intermediates_dir = build_dir, 
    clean = FALSE, 
    params = c(list(report_template_oid = ReportInstance$ParentReportTemplate, 
                    report_instance_oid = ReportInstance$OID, 
                    output_dir = build_dir), 
               params), 
    envir = new.env()
  )
  
  file.path(build_dir, 
            report_filename)
}

# Unexported --------------------------------------------------------

getReportImages <- function(report_filename,
                            report_format = c("html", "pdf")){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = report_filename, 
                             len = 1, 
                             add = coll)
  
  report_format <- checkmate::matchArg(x = report_format, 
                                       choices = c("html", "pdf"), 
                                       .var.name = "report_format", 
                                       add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  if (report_format == "html"){
    image_dir <- basename(report_filename)
    image_dir <- sub("\\.html$", "", image_dir)
    image_dir <- sprintf("%s_files", image_dir)
    image_dir <- file.path(image_dir, 
                           "figure-html")
  } else {
    
  }

  list.files(file.path(dirname(report_filename), 
                       image_dir), 
             pattern = if (report_format == "html") "\\.png$" else "\\.pdf$", 
             full.names = TRUE)
}

# Unexported --------------------------------------------------------

getSupportingData <- function(report_filename, 
                              pattern = "SupportingData.csv"){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = report_filename, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = pattern, 
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  list.files(dirname(report_filename), 
             pattern = pattern, 
             full.names = TRUE)
}

# Unexported --------------------------------------------------------

zipReportFiles <- function(zipfile, 
                           files_to_zip){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = zipfile, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = files_to_zip, 
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  zip(zipfile, 
      files = files_to_zip, 
      extras = "-j")
}


