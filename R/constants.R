#' @name constants
#' @title Constants for the ReportManager application
#' 
#' @description Commonly used values to simplify maintenance of the package.

SUPPORTED_SQL_FLAVOR <- c("sql_server", "sqlite")

#' @rdname constants
#' @export

UNIT_OF_TIME <- c("Second", "Minute", "Hour", "Day", "Week", "Month", "Year")

#' @rdname constants
#' @export

UNIT_OF_TIME_WITH_NONE <- c("None", UNIT_OF_TIME)

#' @rdname constants
#' @export

REPORT_TEMPLATE_DISPLAY_PROPERTIES <- c("OID", "Title", "TemplateDirectory", 
                                        "IsSignatureRequired", "IsActive")

#' @rdname constants
#' @export

FONT_SIZES <- 
  c("5pt" = "tiny", 
    "7pt" = "scriptsize", 
    "8pt" = "footnotesize", 
    "9pt" = "small", 
    "10pt" = "normalsize", 
    "12pt" = "large", 
    "14pt" = "Large", 
    "18pt" = "LARGE", 
    "20pt" = "Huge", 
    "24pt" = "HUGE")
