

add.codeName.app <- function(data, filename, missing.variables) {
  runGadget(
    app = shinyApp(
      ui <- bootstrapPage(
        tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
        theme = theme.selection,
        shinyjs::useShinyjs(),
        input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        br(),
        navset_tab(nav_panel(
          strong("Select"),
          column(
            width = 12,
            h5(strong(
              paste0(
                "'",
                filename,
                "' contains variables not found in the data dictionary"
              )
            )),
            shinyWidgets::prettyCheckboxGroup(
              "codeName.choice",
              h5("Select variables to add to the dictionary"),
              choices =  missing.variables,
              outline = TRUE,
              status = "warning"
            ),
            actionButton("action", "Submit"),
            br(),
            br(),
          ),
        ), 
        nav_panel(strong("Review"), 
                  column(
                    width = 12,
                    br(),
                    tags$head(tags$style(
                      HTML(
                        "#refresh {
                        color: #696969;
                        background-color: white;
                        box-shadow: 3px 3px 3px 3px white;
                        }
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
                        }"
                      )
                    )),
                    DT::DTOutput('df', height = "350px")
                  ))
        )),
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        codeName.choice <- reactive(input$codeName.choice)
        
      
        output$df = DT::renderDT({
          data
        }, rownames = TRUE, 
        class = "display nowrap",
        options = list(
          dom = 'ft',
          pageLength = nrow(data),
          autoWidth = TRUE,
          searchHighlight = TRUE,
          scrollX = TRUE
        ))
      
      
        observe({
          my_global_env <- globalenv()
          my_global_env$codeName.choice <-  input$codeName.choice
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}

