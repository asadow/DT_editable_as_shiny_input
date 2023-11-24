#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(rhandsontable)
library(bslib)

jsCode <- "shinyjs.refreshme = function() { location.reload(); }"
# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("DT Editor Minimal Example"),
  ### This is to adjust the width of pop up "showmodal()" for DT modify table 
  tags$head(tags$style(HTML('

                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions = c("refreshme")),
  actionButton(
    "refresher",
    "Reset",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
  
  helpText("Note: Remember to save any updates!"),
  br(),
  ### tags$head() is to customize the download button
  tags$head(
    tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")
    ),
  downloadButton("Trich_csv", "Download in CSV", class="butt"),
  rHandsontableOutput("hot", width = 350),
  uiOutput("body"), 
  actionButton(inputId = "update",label = "Save"),
  
)
