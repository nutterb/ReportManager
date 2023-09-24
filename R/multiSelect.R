#' @name multiSelect
#' @title Shiny Controls for Multi-Select Tools
#' 
#' @description Some settings in the Report Manager are chosen by moving 
#'   items from one selection box to another. For example, selecting users
#'   to assign to a role is done by moving selected users from a box on 
#'   the left side of the UI to a box on the right. 
#'   
#'   `multiSelect` generates the UI components, with an option to include
#'   the "Move Up" and "Move Down" buttons. 
#'   
#'   `updateMultiSelect` manages events for the UI buttons. 
#'   
#'   `replaceMultiSelect` is used to replace the underlying data of the 
#'   `multiSelect` tool. This is done in cases such as when changing the 
#'   role that is in focus for user selection. 
#'   
#' @param inputId `character(1)`. The ID for the input element. 
#' @param label `character(1)`. The label displayed with the element.
#' @param choices `character`. The choices available for selection. 
#' @param selected `character`. The choices to be marked as selected when 
#'   initiating the UI element.
#' @param width `character(1)`. A CSS style for the width of the control.
#' @param up_down `logical(1)`. When `TRUE`, the "Move Up" and "Move Down"
#'   buttons are displayed, allowing for the selections to be ordered.
#' @param ... Other arguments to pass to `selectInput`. 
#' 
#' @details The choices provided in `choices` and `selected` are used to
#'   form a data frame with the columns
#'   
#' * `choices` - The choices available to choose from. These are shown in the order provided.
#' * `selected` - The choices to initiate as selected. These are shown in the order provided.
#' * `order` - The order in which selections will appear in the selected box. This is stored internally to manage moving items up and down.
#' * `display_order` - The order in which the available choices will appear. This is stored to maintain order when moving choices left and right.
#' 
#' After making the data frame, it is converted to a JSON string and placed in 
#' a `textInput` element named `inputId`.
#' 
#' The other elements created in the controls are 
#' 
#' * `[inputId]_unselected` - the `selectInput` on the left giving choices available for selection.
#' * `[inputId]_selected` - the `selectInput` on the right giving the choices selected.
#' * `[inputId]_move_right_all` - an `actionButton` that moves all choices from the left side to the right side.
#' * `[inputId]_move_right` - an `actionButton` that moves the selected items in the left side to the right side.
#' * `[inputId]_move_left` - an `actionButton` that moves the selected items in the right side to the left side. 
#' * `[inputId]_move_right` - an `actionButton` that moves all the items on the right side to the left side. 
#' * `[inputId]_move_up` - an `actionButton` that moves the selected item in the right side up one space.
#' * `[inputId]_move_down` - an `actionButton` that moves the selected item in the right side down one space.
#' 
#' @export

multiSelect <- function(inputId, 
                        label, 
                        choices = NULL, 
                        selected = NULL, 
                        width = "100%",
                        up_down = FALSE,
                        ...){
  # Argument validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = inputId, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = label, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertCharacter(x = choices, 
                             add = coll)
  
  checkmate::assertCharacter(x = selected, 
                             add = coll)
  
  checkmate::assertCharacter(x = width, 
                             len = 1, 
                             add = coll)
  
  checkmate::assertLogical(x = up_down, 
                           len = 1, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functionality ---------------------------------------------------
  
  # Make the Choices Data Frame
  Choices <- data.frame(choices = letters, 
                        selected = rep(c(TRUE, FALSE), c(2, 24)), 
                        order = c(2, 1, rep(NA_real_, 24)), 
                        display_order = seq_along(choices))
  
  # Up-Down tag list
  
  up_down_tag <- 
    shiny::tagList(
      div(style = "display:inline-block;", 
          actionButton(inputId = sprintf("%s_move_up", 
                                         inputId), 
                       label = "Move Up"), 
          br(), 
          actionButton(inputId = sprintf("%s_move_down", 
                                         inputId), 
                       label = "Move Down"))
    )
  
  
  shiny::tagList(
    shiny::tags$label(label),
    shiny::br(),
    shinyjs::hidden(
      shiny::textInput(inputId = inputId, 
                       label = "", 
                       value = as.character(jsonlite::toJSON(Choices)))), 
    div(style = "display:inline-block;", 
        shiny::selectInput(inputId = sprintf("%s_unselected", 
                                             inputId), 
                           label = "Unselected", 
                           choices = Choices$choices[!Choices$selected], 
                           selected = NULL,
                           selectize = FALSE, 
                           multiple = TRUE,
                           ...)), 
    shiny::div(style = "display:inline-block;",
        shiny::actionButton(inputId = sprintf("%s_move_all_right", 
                                              inputId), 
                            label = "Move All Right"), 
        shiny::br(), 
        shiny::actionButton(inputId = sprintf("%s_move_right", 
                                       inputId), 
                     label = "Move Right"), 
        shiny::br(), 
        shiny::actionButton(inputId = sprintf("%s_move_left", 
                                       inputId), 
                     label = "Move Left"), 
        shiny::br(), 
        shiny::actionButton(inputId = sprintf("%s_move_all_left", 
                                       inputId), 
                     label = "Move All Left")), 
    shiny::div(style = "display:inline-block;", 
        shiny::selectInput(inputId = sprintf("%s_selected", 
                                      inputId), 
                    label = "Selected", 
                    choices = .updateMultiSelect_getOrderedSelection(Choices), 
                    selected = NULL,
                    selectize = FALSE, 
                    multiple = TRUE,
                    ...)), 
    
    if (up_down) up_down_tag else NULL
  )
}

#' @rdname multiSelect
#' @export

updateMultiSelect <- function(session, inputId, input, 
                              action = c("move_all_right", 
                                         "move_right", 
                                         "move_all_left", 
                                         "move_left", 
                                         "move_up", 
                                         "move_down")){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(x = inputId, 
                             len = 1, 
                             add = coll)
  
  action <- checkmate::matchArg(x = action, 
                                choices = c("move_all_right", 
                                            "move_right", 
                                            "move_all_left", 
                                            "move_left", 
                                            "move_up", 
                                            "move_down"), 
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Retrieve the choices from the UI --------------------------------
  
  Choices <- input[[inputId]]
  Choices <- jsonlite::fromJSON(Choices)
  
  # Update the `selected` or `order` columns based on the action.
  switch(
    action, 
    "move_all_right" = Choices$selected <- rep(TRUE, nrow(Choices)), 
    "move_right" = Choices$selected[Choices$choices %in% input[[sprintf("%s_unselected", 
                                                                        inputId)]]] <- TRUE,
    "move_left" = Choices$selected[Choices$choices %in% input[[sprintf("%s_selected", 
                                                                       inputId)]]] <- FALSE, 
    "move_all_left" = Choices$selected <- rep(FALSE, nrow(Choices)), 
    "move_up" = Choices$order <- 
      .updateMultiSelect_moveUpDown(Choices = Choices, 
                                    selection_to_move = input[[sprintf("%s_selected", inputId)]], 
                                    up = TRUE),
    "move_down" = Choices$order <- 
      .updateMultiSelect_moveUpDown(Choices = Choices, 
                                    selection_to_move = input[[sprintf("%s_selected", inputId)]], 
                                    up = FALSE)
    ) 

  Choices <- .updateMultiSelect_setChoiceOrder(Choices)
  
  # Send the new choices back to the input element.
  new_choices <- jsonlite::toJSON(Choices, 
                                  auto_unbox = TRUE, 
                                  json_verbatim = TRUE)
  
  shiny::updateTextInput(session = session, 
                         inputId = inputId, 
                         value = as.character(new_choices))
  
  # Update the select input controls.
  shiny::updateSelectInput(session = session, 
                           inputId = sprintf("%s_unselected",
                                             inputId),
                           choices = Choices$choices[!Choices$selected])
  shiny::updateSelectInput(session = session, 
                           inputId = sprintf("%s_selected",
                                             inputId),
                           choices = .updateMultiSelect_getOrderedSelection(Choices), 
                           selected = input$user_selected)
}

# Unexported --------------------------------------------------------

.updateMultiSelect_setChoiceOrder <- function(Choices){
  Choices <- Choices[order(Choices$order, Choices$choices), ]
  Choices$order <- seq_len(nrow(Choices))
  
  Choices[order(Choices$display_order), ]
}

.updateMultiSelect_getOrderedSelection <- function(Choices){
  Selected <- Choices[Choices$selected, ]
  Selected <- Selected[order(Selected$order, Selected$display_order), ]
  Selected$choices
}

.updateMultiSelect_moveUpDown <- function(Choices, 
                                          selection_to_move, 
                                          up = TRUE){
  Choices <- Choices[order(Choices$order), ]
  
  increment <- if (up) -1 else 1
  
  row_to_move <- which(Choices$choices %in% selection_to_move)
  row_adjacent <- row_to_move + increment
  
  order_to_move <- Choices$order[Choices$choices %in% selection_to_move]
  order_adjacent <- order_to_move + increment
  
  iterate_over <- seq_along(order_to_move)
  if (!up) iterate_over <- rev(iterate_over)
  
  for (i in iterate_over){
      Choices$order[row_to_move[i]] <- order_adjacent[i]
      Choices$order[row_adjacent[i]] <- order_to_move[i]
      
      Choices <- Choices[order(Choices$order), ]
  }
  
  Choices <- Choices[order(Choices$display_order), ]
  Choices$order
}
