#' @name radioDataTableOutput
#' @title Display a DataTable Object with Radio Buttons
#'
#' @description Creates the User Interface objects for a DataTable object
#'   that includes radio buttons. This is used in the ui.R object to 
#'   opposite server output objects that user [DT::renderDataTable()]
#'   with a call to `makeRadioDataTable`.
#' 
#' @param outputId `character(1)` The name of the output created using 
#'   [DT::renderDataTable()].
#' @param radioId `character(1)` Should match the `element_name`
#'   argument of the call to `makeRadioDataTable`
#' @param height `character(1)` The height of the element.
#' @param width `character(1)` The width of the element.
#' @param horizontal_scroll `logical(1)` Allow horizontal scrolling for wide tables?
#' 
#' @author Benjamin Nutter
#'
#' @export

radioDataTableOutput <- function(outputId, 
                                 radioId, 
                                 height = "1000px", 
                                 width = "100%", 
                                 horizontal_scroll = TRUE)
{
  # Argument validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = outputId, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = radioId, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = height, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = width, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_logical(x = horizontal_scroll, 
                            len = 1, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  
  shiny::div(
    id = radioId,
    class = "shiny-input-radiogroup",
    DT::dataTableOutput(outputId),
    shiny::br(), shiny::br(), shiny::br(), shiny::br(),
    style = paste0(if (horizontal_scroll) "overflow-y:scroll; " else "", 
                   "max-height:", height, 
                   "; width:", width, ";")
  )
  
}
