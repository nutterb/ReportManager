args <- commandArgs(trailingOnly = TRUE)
app_file <- args[1]
params <- args[2]

# .libPaths(this_lib)

# portable_browser <- function(appUrl){
#   shell(sprintf("start msedge --app=%s --start-maximized --new-window --edge-kiosk-type=fullscreen ", appUrl), wait = FALSE)
# }
# 
# options(browser = portable_browser)

params <- gsub("'", "\"", params)
params <- rjson::fromJSON(params)

params$report_instance_oid <- as.numeric(params$report_instance_oid)
params$report_template_oid <- as.numeric(params$report_template_oid)

shiny::runApp(appDir = app_file,
              launch.browser = TRUE)
