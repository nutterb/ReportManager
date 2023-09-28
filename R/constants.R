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