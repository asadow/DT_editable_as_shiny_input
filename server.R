library(shiny)
library(shinyjs)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(readr)
library(tibble)

rm(list = ls())

shinyServer(function(input, output, session){
  
  ### interactive dataset 
  vals <- reactiveValues()
  vals$Data <- readRDS("wo.rds")
  
  observeEvent(input$refresher, {
    shinyjs::js$refreshme()
  })
  #### body is the id of DT table
  output$body <- renderUI({
    fluidPage(
          hr(),
          column(6,
                 offset = 6,
                 HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                 ### tags$head() This is to change the color of "Add a new row" button
                 tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
                 div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
                 tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
                 div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
                 tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
                 div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
                 ### Optional: a html button 
                 # HTML('<input type="submit" name="Add_row_head" value="Add">'),
                 HTML('</div>') ),
          column(12,
                 DTOutput("entry"),
                 tags$script("$(document).on('click', '#entry button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });"))
      ) 
    })
  #### render DataTable part ####
  output$entry <- renderDT({
    DT = vals$Data
    datatable(
      extensions = 'Responsive',
      DT, 
      rownames= FALSE,
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"),
        info = FALSE,
        paging = FALSE,
        searching = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        ordering = FALSE,
        columnDefs = list(
          list(width = '350px', targets = which(names(DT) == "Issue") - 1)
          # ,
          # list(
          #   targets = which(names(DT) == "Issue") - 1,
          #   render = JS(
          #     "function(data, type, row, meta) {",
          #     "return type === 'display' && data.length > 15 ?",
          #     "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
          #     "}")
          # )
        )
      ),
      editable = TRUE, 
      selection = 'single',
      escape = FALSE
      ) %>%
      formatStyle(
        c('Work Order #', "Issue", "Reporter", "Trade Responsible", "Contractor 1",
          "Contractor 2"),
          backgroundColor = 'lightgrey')
    }, server = TRUE)
  
  proxy = dataTableProxy('entry')
  
  observeEvent(input$entry_cell_edit, {
    
    info = input$entry_cell_edit
    
    str(info) 
    i = info$row 
    j = info$col 
    v = info$value
    
    vals$Data[i, j] <<- DT::coerceValue(v, vals$Data[i, j]) 
    replaceData(proxy, vals$Data, resetPaging = FALSE)
    
  })
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          dateInput(paste0("Date_add", input$Add_row_head), "Date:", value = Sys.Date()),
                          textInput(paste0("Description_add", input$Add_row_head), "Description"),
                          textInput(paste0("Names_add", input$Add_row_head), "Name"),
                          numericInput(paste0("Request_add", input$Add_row_head), "Request Number:",0),  
                          selectInput(paste0("Completed_add", input$Add_row_head), "Status:",choices=c("Yes", "On progress")),
                          textInput(paste0("Comments_add", input$Add_row_head), "Comments"), 
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    new_row=data.frame(
      Date=as.character( input[[paste0("Date_add", input$Add_row_head)]] ),
      Description=input[[paste0("Description_add", input$Add_row_head)]],
      Names=input[[paste0("Names_add", input$Add_row_head)]],
      Request=input[[paste0("Request_add", input$Add_row_head)]],
      Completed=input[[paste0("Completed_add", input$Add_row_head)]],
      Comments=input[[paste0("Comments_add", input$Add_row_head)]]
    )
    vals$Data<-rbind(vals$Data, new_row)
    removeModal()
  })
  
  ### save to RDS part 
  observeEvent(input$update, {
    saveRDS(vals$Data, "note.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head,{
    showModal(
      if (length(input$entry_rows_selected) >= 1) {
        modalDialog(
          title = "Warning",
          paste("Are you sure you want to delete", 
                length(input$entry_rows_selected),
                "row(s)?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      } else {
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delete!"),
          easyClose = TRUE
        )
      }
      
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    vals$Data = vals$Data[- input$entry_rows_selected, ]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$mod_row_head,{
    showModal(
      if (length(input$entry_rows_selected)>=1) {
        modalDialog(
          fluidPage(
            h3(strong("Modification"), align="center"),
            hr(),
            DTOutput('row_modif'),
            actionButton("save_changes","Save changes"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), 
          size="l" )
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  #### modify part
  output$row_modif <- renderDT({
    
    selected_row = input$entry_rows_selected
    old_row = vals$Data[selected_row, ]
    row_change = list()
    
    for (i in colnames(old_row)) {
      if (is.numeric(vals$Data[[i]])) {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } else if( is.Date(vals$Data[[i]])){
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      } else 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
    }
    row_change = as.data.table(row_change)
    setnames(row_change, colnames(old_row))
    DT = row_change
    DT 
  },
  escape=F, 
  options=list(dom='t',ordering=F,scrollX = TRUE),
  selection="none" 
  )
  
  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF = tibble(lapply(newValue, function(x) t(tibble(x))))
                 colnames(DF) = colnames(vals$Data)
                 vals$Data[input$entry_rows_selected, ] <- DF
                 
               }
  )

 ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
 ### can download the table in csv
  output$Trich_csv <- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals$Data), file, row.names = F)
    }
  )
  output$hot <- renderRHandsontable({
    DF = vals$Data
    if (!is.null(DF))
      rhandsontable(
        DF, 
        rowHeaders = NULL, 
        width = 3000,
        ## Need overflow visible with no height to prevent 
        ## disabled scroll bar on hover 
        # height = 700,
        overflow = 'visible'
      ) %>%
      hot_cols(
        names(DF),
        allowInvalid = TRUE, 
        strict = FALSE,
        colWidths = c(50, 60, 70, 100, 75, 213, 50, 308, rep(130, 10)),
        # fixedColumnsLeft = 2,
        renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if ([4, 7, 8, 9, 16, 17].includes(col)) {
              td.style.background = 'lightgrey';
             } 
           }",
        halign = "htCenter"
      ) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_validate_numeric(cols = 5, min = 100000, max = 999999) %>%
      hot_validate_numeric(cols = 1, min = 0, max = 12) %>%
      hot_validate_numeric(cols = 2, min = 0, max = 60)  %>%
      hot_col(8, halign = "htLeft")
  })
})
