btn_reportInstancePreview_shiny <- function(report_instance_oid, 
                                            report_template_oid,
                                            output_dir, 
                                            rm_flavor, 
                                            rm_database_file, 
                                            rm_driver, 
                                            rm_server, 
                                            rm_database){
  
  ReportTemplate <- queryReportTemplate(oid = report_template_oid)
  
  shiny_file <- file.path(system.file("ReportTemplate", 
                                      package = "ReportManager"), 
                          ReportTemplate$TemplateDirectory, 
                          "shiny")
  
  cmd_file <- file.path(system.file("ReportTemplate", 
                                    package = "ReportManager"),
                        "ReportAsShiny.R")
  
  # Nothing in the params should have single quotes.  Doing so will
  # break this process and force us to use a save()-load() strategy
  # (which is probably a safer strategy anyway, but we're trying to
  #  avoid writing files)
  
  params <- list(report_template_oid = report_template_oid, 
                 report_instance_oid = report_instance_oid, 
                 output_dir          = output_dir, 
                 rm_flavor           = rm_flavor, 
                 rm_database_file    = rm_database_file, 
                 rm_driver           = rm_driver, 
                 rm_server           = rm_server, 
                 rm_database         = rm_database)
  params <- rjson::toJSON(params)
  params <- gsub("\"", "'", params)
  
  cmd <- paste0("RScript.exe ",
                cmd_file, " ",
                shiny_file, " ",
                # parameters
                shQuote(params))
  print(cmd)
  system(cmd)
}
