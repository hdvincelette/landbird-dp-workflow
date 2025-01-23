

# Only display domain Item selection if available

correct.value.app <- function(variable, invalid.value, data, ref.dxnry) {
  runGadget(
    app = shinyApp(
      ui <- 
        bootstrapPage(
        tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
        theme = theme.selection,
        shinyjs::useShinyjs(),
        # input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        br(),
        column(width = 12, uiOutput("dfmsg")),
        br(),
        navset_tab(nav_panel(
          strong("Bulk edit"),
          column(
            width = 12,
            h5(strong(
              paste0(
                "'",
                invalid.value,
                "' is an invalid entry for '",
                variable,
                "'"
              )
            )),
            shinyWidgets::radioGroupButtons(
              "decision",
              h5(strong("Choose an option")),
              choices =  c("keep", "correct","replace"),
              direction = "vertical",
              width = "200px",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            
            conditionalPanel(
              condition = "input.decision == 'replace'",
              shinyWidgets::pickerInput(
                "value.choice",
                h5("Select a dictionary value:"),
                choices = ref.dxnry %>%
                  dplyr::filter(codeName == domainItem_value.table$Variable[b],
                                domainItem_value != "dataField") %>%
                  purrr::pluck("domainItem_value"),
                width = "300px",
                options = list(
                  `live-search` = TRUE,
                  title = "Nothing selected",
                  style = "btn-primary"
                )
              )
            ),
            conditionalPanel(
              condition = "input.decision == 'correct'",
              textInput(
                "value.choice",
                h5("Enter a replacement value:"),
                value = invalid.value,
                width = "300px"
              )
            ),
            span(textOutput("wrn2"), style = "color:red"),
            br(),
            actionButton("action", "Submit"),
            br(),
            br(),
            textOutput("wrn1")
          ),
        ), 
        nav_panel(strong("Manual edit"), 
                  column(
                    width = 12,
                    br(),
                    tags$head(tags$style(
                      HTML(
                        "
                        #DataTables_Table_0_filter {
                        float: left;
                        }
                       table.dataTable tbody tr.selected td,
                       table.dataTable tbody tr.selected td,
                       table.dataTable tbody td.selected {
                       border-top-color: #c4dfcc !important;
                       box-shadow: inset 0 0 0 9999px #c4dfcc !important;
                       color: black;
                       }
                       table.dataTable tbody tr:active td {
                       background-color: #c4dfcc !important;
                       }
                       :root {
                       --dt-row-selected: transparent !important;
                       }
                       table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
                       background-color: #c4dfcc !important;
                        }
                        .dataTables_wrapper .dataTables_length,
                        .dataTables_wrapper .dataTables_filter,
                        .dataTables_wrapper .dataTables_filter label,
                        .dataTables_wrapper .dataTables_info,
                        .dataTables_wrapper .dataTables_processing,
                        .dataTables_wrapper .dataTables_paginate {color:#ffffff;}
                                         thead {color:#ffffff;}
                                         tbody {color:#ffffff;}
                        #refresh{
                        color: white;
                        background-image: none;
                        background-color: grey;
                        -webkit-box-shadow: 0px;
                        box-shadow: 0px;
                        border:0px;
                        }"
                      )
                    )), 
                    DT::DTOutput('df',  height = "325px"),
                    br(),
                    shinyWidgets::actionBttn(
                      'refresh',
                      '',
                      icon = icon('refresh'),
                      style = "jelly",
                      no_outline = TRUE,
                      size = "md"
                    )
                    ))
        )),
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        output$dfmsg <- renderUI(
          HTML(paste("<em>",names(raw.data.list[a]),"</em>"))
        )
      
        decision <- reactive(input$decision)
        value.choice <- reactive(input$value.choice)
        
        
        dfon <- reactiveValues(data = NULL)

        observe({
          dfon$data <- data
        })

        
        output$df = DT::renderDT({
        data
        }, rownames = TRUE, 
        class = "display nowrap",
        editable = list(target = "cell"
                        # , disable = list(columns =c(which(colnames(input.data)!=variable)))
                        ),
        options = list(
          dom = 'ft',
          pageLength = nrow(data),
          autoWidth = TRUE,
          searchHighlight = TRUE,
          scrollX = TRUE,
          language = list(
            search = "<i class='glyphicon glyphicon-search'></i>"
          )
        ))
        
        proxy<- DT::dataTableProxy("df")
        
        observeEvent(input$refresh, {
          DT::reloadData(proxy)
          
        })
        
        observeEvent(input$df_cell_edit, {
          observeEvent(input$action, {
          info <- input$df_cell_edit

          i <- info$row
          j <- info$col
          v <- info$value

          dfon$data[i, j] <- v
          })
        })
        
        output$df.0 = DT::renderDT({
          data
        }, rownames = TRUE, 
        class = "display nowrap",
        editable = list(target = "cell", disable = list(columns =c(which(colnames(input.data)!=variable)))),
        options = list(
          dom = 'ft',
          pageLength = nrow(data),
          autoWidth = TRUE,
          searchHighlight = TRUE,
          scrollX = TRUE
        ))
        

        
        output$wrn1 <- renderText({
          HTML(paste("Please note, all manual edits will be applied before bulk edits"))
        })
        
        output$wrn2 <- renderText({
          if (decision() == "correct" & value.choice() == "") {
            paste("Warning: '", invalid.value, "' will be replaced with NA values")
          }
        })
        
        
        observe({
          if (decision() == "replace" &
              value.choice() == "" ) {
            shinyjs::disable("action")
          }
          else{
            shinyjs::enable("action")
          }
        })
        
        
        observe({
          my_global_env <- globalenv()
          my_global_env$decision<- input$decision
          my_global_env$value.choice <-  input$value.choice
          my_global_env$input.data <- dfon$data
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}

