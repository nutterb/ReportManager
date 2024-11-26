#' @name makeSignatureTable 
#' @title Make the UI Table for Signing Reports
#' 
#' @description Uses the Signature data from ReportInstanceSignature
#'   to produce a table with user controls for signing reports and 
#'   removing the signatures.
#'   
#' @param signature_data `data.frame`. Must have the columns
#'   `c("ParentRole", "RoleName", "IsSigned", "SignatureDateTime", "IsSigned")`.
#'
#' @export

makeSignatureTable <- function(signature_data){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertDataFrame(x = signature_data, 
                             col.names = "named",
                             add = coll)
  
  checkmate::reportAssertions(coll)
  
  required <- c("ParentRole", "RoleName", "IsSigned", "SignatureDateTime", "IsSigned")
  
  not_found <- required[!required %in% names(signature_data)]
  
  if (length(not_found) > 0){
    coll$push(sprintf("The following columns are required but not found in `signature_data`: %s", 
                      paste0(not_found, 
                             collapse = ", ")))
  }
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  signature_data <- signature_data[signature_data$MostRecentSignature == 1, ]
  
  btn_sign <- mapply(.makeSignatureButtons,
                     oid = signature_data$ParentRole, 
                     is_signed = signature_data$IsSigned, 
                     MoreArgs = list(stem = "btn_reportInstanceSignature_sign", 
                                     label = "Sign Report"), 
                     SIMPLIFY = FALSE)
  
  btn_unsign <- mapply(.makeSignatureButtons,
                       oid = signature_data$ParentRole, 
                       is_signed = !signature_data$IsSigned, 
                       MoreArgs = list(stem = "btn_reportInstanceSignature_remove", 
                                       label = "Remove Signature"), 
                       SIMPLIFY = FALSE)
  
  btn_sign <- unlist(btn_sign)
  btn_unsign <- unlist(btn_unsign)
  
  signature_data$Button <- sprintf("%s %s",
                                   btn_sign, 
                                   btn_unsign)
  
  signature_data <- 
    signature_data[c("RoleName", "SignatureName", 
                     "SignatureDateTime", "Button")]
  
  dataframeToHtml(signature_data)
}


.makeSignatureButtons <- function(stem, 
                                  oid, 
                                  is_signed, 
                                  label){
  btn <- shiny::actionButton(inputId = sprintf("%s_%s", 
                                               stem, 
                                               oid), 
                             label = label)
  
  if (is_signed) {
    btn <- shinyjs::hidden(btn)
  }
  
  as.character(btn)
}

