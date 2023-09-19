#' @name radioDataTable
#' @title Make a Data Table Element with Radio Buttons in the First Column
#'
#' @description Constructs a Data Table element that can be used with
#'   radio buttons to make selections.
#'
#' @param data `data.frame` table of data.
#' @param id_variable `character(1)`. The name of a column in `data`
#'   that will be used to make the selection IDs.
#' @param element_name `character(1)`. The name for the page element. This
#'   acts as the shiny `inputId`.
#' @param checked `integerish(0/1)` or `character(0/1)`. The value
#'   from `id_variable` that should be marked checked.
#'
#' @author Benjamin Nutter
#'
#' @export

radioDataTable <- function(data, 
                           id_variable, 
                           element_name,
                           checked = character(0)){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertDataFrame(x = data,
                               add = coll)
  
  checkmate::assertCharacter(x = id_variable,
                              len = 1,
                              add = coll)
  
  checkmate::assertCharacter(x = element_name,
                              len = 1,
                              add = coll)
  
  checkmate::assert(
    checkmate::testCharacter(x = checked, 
                              max.len = 1), 
    checkmate::testIntegerish(x = checked, 
                                max.len = 1), 
    .var.name = "checked", 
    add = coll,
    combine = "or"
  )
  
  checkmate::reportAssertions(coll)
  
  checked <- as.character(checked)
  
  checkmate::assert_subset(x = id_variable,
                           choices = names(data),
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = checked,
                           choices = as.character(data[[id_variable]]),
                           empty.ok = TRUE,
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Convert ID variable to a radio button ---------------------------
  data[[id_variable]] <- 
    sprintf("<input type = 'radio' name = '%s' value = '%s' %s/>",
            element_name,
            data[[id_variable]],
            ifelse(data[[id_variable]] %in% checked,
                   "checked = 'checked'",
                   ""))
  data
}
