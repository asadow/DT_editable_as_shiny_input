library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)

rm(list = ls())

wo <- read_rds("wo.rds")
wider_col <- which(names(wo) == "Issue")

shinyServer(function(input, output, session){
  
  ### interactive dataset 
  vals_trich<-reactiveValues()
  vals_trich$Data<-readRDS("wo.rds")
  
  observeEvent(input$refresher, {
    shinyjs::js$refreshme()
  })
  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    fluidPage(
          hr(),
          column(12,dataTableOutput("Main_table_trich"))
      ) 
    })
  output$hot <- renderRHandsontable({
    
  })
  #### render DataTable part ####
  output$Main_table_trich <- renderDataTable({
    DT = vals_trich$Data
    datatable(
      DT, 
      options = list(
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '600px', targets = 8 - 1)
          )
      ),
      editable = TRUE, 
      selection = "none"
      ) 
    }, server = TRUE)
  
  proxy = dataTableProxy('Main_table_trich')
  
  observeEvent(input$Main_table_trich_cell_edit, {
    
    info = input$Main_table_trich_cell_edit
    
    str(info) 
    i = info$row 
    j = info$col 
    v = info$value
    
    vals_trich$Data[i, j] <<- DT::coerceValue(v, vals_trich$Data[i, j]) 
    replaceData(proxy, vals_trich$Data, resetPaging = FALSE) # important
    
  })

  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    saveRDS(vals_trich$Data, "wo.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
 ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
 ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
 
})
