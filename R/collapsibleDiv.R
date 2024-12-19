#' @name collapsibleDiv
#' @title Add Elements to a Collapsible Div Tag
#'
#' @description Creates a UI element with a title bar that can be clicked
#'   to expand or reduce the content from view.
#'
#' @param title \code{character(1)} The title for the section.
#' @param id \code{character(1)} The id name for the input control that
#'   opens and closes the collapsible.
#' @param ... UI elements to include in the collapsed portion
#' @param style \code{character(1)} Additional CSS styling.
#' @param checked \code{logical(1)} Should the div initialize in it's opened
#'   state?
#' @param div_id \code{character(1)} The id name for the the collapsible div.
#'
#' @author Benjamin Nutter
#'
#' @export

collapsibleDiv <- function(title, id, ..., style = "", checked = FALSE, 
                           div_id = sprintf("colldiv-%s", id))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = title,
                              len = 1, 
                            add = coll)
  checkmate::assertCharacter(x = id,
                              len = 1,
                             add = coll)
  checkmate::assertCharacter(x = style,
                              len = 1, 
                             add = coll)
  checkmate::assertLogical(x = checked,
                            len = 1, 
                            add = coll)
  checkmate::assertCharacter(x = div_id,
                             len = 1, 
                             add = coll)
  
  checkmate::reportAssertions(coll)

  shiny::tagList(
    shiny::div(class = "wrap-collabsible",
               id = div_id, 
               shiny::HTML(sprintf("<input id = '%s' class = 'toggle' type = 'checkbox' style = 'display:none;' %s/>",
                                   id,
                                   if (checked) "checked" else "")),
               shiny::tags$label(`for` = id,
                                 class = "lbl-toggle",
                                 style = style,
                                 title),
               shiny::div(class = "collapsible-content",
                          shiny::div(class = "content-inner",
                                     ...)))
  )
}
