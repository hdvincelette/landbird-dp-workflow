
add.column.app <- function(missing.cols, add.data) {
  runGadget(
    app = shinyApp(
      ui <- 
        bootstrapPage(
        tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
        theme = theme.selection,
        shinyjs::useShinyjs(),
        # input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        tags$style(
          HTML(
            "table.dataTable tbody tr.selected td,
                       table.dataTable tbody tr.selected td,
                       table.dataTable tbody td.selected {
                       border-top-color: #bcbcbc !important;
                       box-shadow: inset 0 0 0 9999px #bcbcbc !important;
                       color: black;
                       }
                       table.dataTable tbody tr:active td {
                       background-color: #bcbcbc !important;
                       }
                       :root {
                       --dt-row-selected: transparent !important;
                       }
                       table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
                       background-color: #bcbcbc !important;
                        }
            .dataTables_wrapper .dataTables_length,
                                         .dataTables_wrapper .dataTables_filter,
                                         .dataTables_wrapper .dataTables_info,
                                         .dataTables_wrapper .dataTables_processing,
                                         .dataTables_wrapper .dataTables_paginate {
                                           color:#ffffff;
                                         }
                                         thead {
                                           color:#ffffff;
                                         }
                                         tbody {
                                           color:#ffffff;
                                         }"
            
          )
        ),
        br(),
        navset_tab(nav_panel(
          strong("Select"),
          column(
            width = 12,
            shinyWidgets::pickerInput(
              "col.choice",
              h5(strong("Select columns to include in the new file")),
              choices =  missing.cols,
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(actionsBox = TRUE)),
            actionButton("action", "Submit"),
            br(),
            br()
          )),
          nav_panel(strong("Review"),
                    layout_sidebar(
                          sidebar = 
                            shinyWidgets::radioGroupButtons(
                                "selected.col",
                                h5(strong("")),
                                choices =  missing.cols,
                                direction = "vertical",
                                checkIcon = list(yes = icon("check"))
                            ),
                    navset_card_pill(
                    full_screen = TRUE,
                        nav_panel(
                        fillable = TRUE,
                        strong("Summarize"),
                        column(width = 12,
                               br(),
                               tableOutput("col.summary"))), 
                      nav_panel(strong("View"),
                                fillable = TRUE,
                                column(width = 12,
                                       br(),
                                       DT::DTOutput('col.view', height = "325px"))
                      ))))
        
        )
      ), 
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        
        selected.col <- reactive(input$selected.col)
        
        output$col.view = DT::renderDataTable({
          add.data %>% dplyr::select(tidyselect::all_of(selected.col()))
        }, rownames = FALSE, options = list(
          dom = 'ft',
          pageLength = nrow(data),
          searchHighlight = TRUE,
          columnDefs = list(list(width = "150px", targets = "_all")),
          scrollX = TRUE,
          language = list(
            search = "<i class='glyphicon glyphicon-search'></i>"
          )
        ))
        
        output$col.summary =
          renderTable({
            tibble::enframe(purrr::flatten(sapply(dataMaid::summarize(add.data[[selected.col()]]),"[","result"))) %>% 
              dplyr::mutate(name = gsub("\\..*", "", name))
          },
          rownames = FALSE,
          colnames = FALSE,
          width = "500px")
        
        col.choice <- reactive(input$col.choice)
        
        
        observe({
          my_global_env <- globalenv()
          my_global_env$col.choice <-  input$col.choice
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}
