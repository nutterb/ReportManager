#' @name sendEmail
#' @title Send an Email from the Application
#' 
#' @description Sends and e-mail from the Report Manager Application
#' 
#' @param from_user_oid `intgerish(1)`. The OID of the user initiating the
#'   send action.
#' @param to_address `character(n)`. The email address to which the e-mail 
#'   will be sent.
#' @param message `character(1)`. The message included in the body of the e-mail.
#' @param report_template `data.frame` of the Report Template settings.
#' @param report_instance_oid `integerish(1)`. The OID of the report instance
#'   being sent.
#' @param filename `character(n)`. The files to attach to the e-mail.
#' @param control `list` control parameters to pass to the argument of the 
#'   same name in \code{\link[sendmailR]{sendmail}}.
#' @param embed_html `logical(1)`. Controls if the content is embedded into the 
#'   email or attached as a file.
#' @param is_revision `logical(1)`. Indicates if the e-mail is being sent 
#'   regarding a revision.
#' @param ... Other arguments to pass to \code{\link[sendmailR]{sendmail}}.
#' 
#' @export

sendEmail <- function(from_user_oid, 
                      to_address, 
                      message,
                      report_template, 
                      report_instance_oid,
                      filename = character(0),
                      control = list(smtpServer = getOption("RM_smtpServer")),
                      embed_html = FALSE,
                      is_revision = FALSE,
                      ...){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertIntegerish(x = from_user_oid, 
                               len = 1, 
                               add = coll)
  
  checkmate::assertCharacter(x = to_address, 
                              add = coll)
  
  checkmate::assertCharacter(x = message, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertDataFrame(x = report_template, 
                             add = coll)
  
  checkmate::assertIntegerish(x = report_instance_oid, 
                              len = 1, 
                              add = coll)
  
  checkmate::assertCharacter(x = filename, 
                             add = coll)
  
  checkmate::assertList(x = control, 
                        add = coll)
  
  checkmate::assertLogical(x = embed_html, 
                           len = 1, 
                           add = coll)
  
  checkmate::assertLogical(x = is_revision, 
                           len = 1, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  dots <- list(...)
  attach <- NULL
  
  # Assign files to attach and files to embed -----------------------
  if (embed_html){
    file_to_attach <- filename[!grepl(".html$", filename)]
    file_to_embed <- filename[1]
    if (length(file_to_embed) > 0){
      file_to_embed <- file_to_embed[grepl(".html$", filename)]
    }
  } else {
    file_to_attach <- filename
    file_to_embed <- character(0)
  }
  
  # Set Reference Values --------------------------------------------
  FromUser <- queryUser(oid = from_user_oid)
  
  report_instance <- queryReportInstance(report_instance_oid = report_instance_oid)
  
  message <- gsub("\\n", "<br/>", message)
  
  # Make Attached Files Object --------------------------------------
  if (length(file_to_attach) > 0){
    attach <- lapply(filename, 
                     function(f) sendmailR::mime_part(f, basename(f)))
  } 
  
  # Make the embedded file code and add it to message ---------------
  if (length(file_to_embed)){
    html <- readLines(file_to_embed)
    # Rename and add anchors to sections in a format Outlook will respect
    toc_link <- which(grepl("<li><a", html))
    
    for (i in seq_along(toc_link)){
      tag_link <- html[toc_link[i]]
      anchor <- sub("^.+href[=]\"#", "", tag_link)
      anchor <- sub("\".+$", "", anchor)
      
      html[toc_link[i]] <- sub("(href[=]\").+(\">)", 
                              sprintf("\\1#anchor%s\\2", i), 
                              html[toc_link[i]])
      
      target <- which(grepl(sprintf("div id=\"%s", anchor), html))
      html[target] <- 
        sprintf("<a name=\"%s\"></a>%s", 
                sprintf("anchor%s", i), 
                html[target])
    }
    
    html <- html[trimws(html) != ""]
    which_body <- which(grepl("[<]body", html))
    html <- html[-seq(1, min(which_body) - 1)]
    html <- paste0(html, collapse = "\n")
    
    # Make HTML Line Breaks
    message <- paste0(message, html, sep = "  ")
  }
  
  message <- sendmailR::mime_part(message)
  message[["headers"]][["Content-Type"]] <- "text/html"
  message <- c(list(message), attach)
  
  # Send the Email --------------------------------------------------  
  
  if (length(to_address) > 0){
    sendmailR::sendmail(from = FromUser$EmailAddress, 
                        to = to_address,  
                        subject = .sendEmail_makeSubject(report_template, 
                                                         report_instance, 
                                                         is_revision), 
                        msg = message,
                        control = control, 
                        ...)
  } else {
    message("No recipients listed for the TO field--no e-mail sent.")
  }
}


.sendEmail_makeSubject <- function(report_template, 
                                   report_instance, 
                                   is_revision){
  sprintf("%s%s - %s - %s", 
          if (is_revision) "Revision: " else "",
          report_template$Title, 
          format(report_instance$StartDateTime,
                 format = "%Y-%m-%d %H:%M:%S"),
          format(report_instance$EndDateTime, 
                 format = "%Y-%m-%d %H:%M:%S"))
}

.sendEmail_makeMessage <- function(report_template, 
                                   report_instance){
  msg1 <- "The attached and/or embedded files are being sent from the Report Manager."
  
  instance_msg <- 
    sprintf("Report Title: %s \nStart Date/Time: %s \nEnd Date/Time: %s", 
            report_template$Title, 
            format(report_instance$StartDateTime,
                   format = "%Y-%m-%d %H:%M:%S"),
            format(report_instance$EndDateTime, 
                   format = "%Y-%m-%d %H:%M:%S"))
  
  paste0(c(msg1, 
           instance_msg, 
           report_template$DefaultEmailText), 
         collapse = "\n\n")
}

.sendEmail_makeRevisionMessage <- function(report_template, 
                                           report_instance, 
                                           user_oid, 
                                           reason){
  msg1 <- "A revision has been initiated for a previously submitted report. See the details below."
  
  User <- queryUser(user_oid)
  user_name <- sprintf("%s, %s", 
                       User$LastName, 
                       User$FirstName)
  
  instance_msg <- 
    sprintf("Report Title: %s \nStart Date/Time: %s \nEnd Date/Time: %s\nRevision Initiated By: %s\n\nReason: \n\n", 
            report_template$Title, 
            format(report_instance$StartDateTime,
                   format = "%Y-%m-%d %H:%M:%S"),
            format(report_instance$EndDateTime, 
                   format = "%Y-%m-%d %H:%M:%S"), 
            user_name)
  
  paste0(c(msg1, 
           instance_msg), 
         collapse = "\n\n")
}
