sendEmail <- function(from_user_oid, 
                      to_address, 
                      message,
                      report_template, 
                      report_instance_oid,
                      filename = character(0),
                      control = list(smtpServer = getOption("RM_smtpServer")),
                      embed_html = FALSE,
                      ...){
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
  sendmailR::sendmail(from = FromUser$EmailAddress, 
                      to = to_address,  
                      subject = .sendEmail_makeSubject(report_template, 
                                                       report_instance), 
                      msg = message,
                      control = control, 
                      ...)
}


.sendEmail_makeSubject <- function(report_template, 
                                   report_instance){
  sprintf("%s - %s - %s", 
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
