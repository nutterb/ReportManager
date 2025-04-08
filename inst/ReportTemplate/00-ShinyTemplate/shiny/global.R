library(shiny)
library(shinyBS)
library(shinybusy)
library(shinydashboard)
library(shinyjs)
library(DT)
library(magrittr)

# knitr::knit reads `params` from the global environment.
# these are created in ReportTemplate/RunAsShiny.R

tmp <- tempfile()
knitr::knit(input = system.file("ReportTemplate/ReportLayout.Rmd",
                                package = "ReportTemplate"),
            output = tmp)
file.remove(tmp)
