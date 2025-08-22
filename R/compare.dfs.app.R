
compare.dfs.app <- function(new.df, old.df, compare.df, compare.stats) {
  runGadget(
    app = shinyApp(
      ui <- 
        page(
        bootstrapPage(
        tags$head(tags$style(
          HTML("pre { overflow: auto; word-wrap: normal; }")
        )),
        theme = theme.selection,
        shinyjs::useShinyjs(),
        br(),
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
                        .dataTables_wrapper .dataTables_filter label,
                        .dataTables_wrapper .dataTables_info,
                        .dataTables_wrapper .dataTables_processing,
                        .dataTables_wrapper .dataTables_paginate {color:#ffffff;}
                                         thead {color:#ffffff;}
                                         tbody {color:#ffffff;}
            .nav-tabs .nav-item .nav-link:hover {color:#77a9a1;}"
            
          )
        ),
        # input_dark_mode(id = "dark_mode", mode = "light"),
        column(width = 12, navset_tab(
          nav_panel(strong("View"),
                    br(),
                    h5(em("New:")),
                    DT::DTOutput('df1'
                                 , height = "300px"
                                 ),
                    br(),
                    h5(em("Old:")),
                    DT::DTOutput('df2'
                                 , height = "300px"
                                 ),
                    br(),
                    actionButton("action", "Done"),
                    br(),
                    br()),
          nav_panel(
            strong("Summary"),
            br(),
            bslib::accordion(
              id = "acc",
              open = FALSE,
              bslib::accordion_panel(title = "Summary of data frames", tableOutput('frame.summary.table')),
              bslib::accordion_panel(title = "Summary of overall comparison", tableOutput('comparison.summary.table')),
              bslib::accordion_panel(title = "Variables not shared", tableOutput('vars.ns.table')),
              bslib::accordion_panel(title = "Observations not shared", tableOutput('obs.table')),
              bslib::accordion_panel(title = "Differences detected by variable", tableOutput('diffs.byvar.table')),
              bslib::accordion_panel(title = "Differences detected", tableOutput('diffs.table')),
              bslib::accordion_panel(title = "Non-identical attributes", tableOutput('attrs.table'))
            ),
            br(),
            actionButton("action", "Done"),
            br()
          ), nav_panel(
            strong("Revisions"),
            br(),
            DT::DTOutput('compare.df', height = "350px"),
            br(),
            actionButton("action", "Done"),
            br(),
          )
        ))
      )
      ),
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        output$df1 = DT::renderDataTable({
          new.df
        },  rownames = FALSE, options = list(
          rowCallback = htmlwidgets::JS("function(r,d) {$(r).attr('height', '30px')}"),
          dom = 'ft',
          pageLength = nrow(new.df),
          searchHighlight = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(width = "150px", targets = "_all")),
          scrollX = TRUE,
          language = list(
            search = "<i class='glyphicon glyphicon-search'></i>"
          )
        ))
        
        output$df2 = DT::renderDataTable({
          old.df
        }, rownames = FALSE, options = list(
          rowCallback = htmlwidgets::JS("function(r,d) {$(r).attr('height', '30px')}"),
          dom = 'ft',
          pageLength = nrow(old.df),
          searchHighlight = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(width = "150px", targets = "_all")),
          scrollX = TRUE,
          language = list(
            search = "<i class='glyphicon glyphicon-search'></i>"
          )
        ))
        
        output$compare.df = DT::renderDataTable({
          compare.df
        }, rownames = FALSE, options = list(
          rowCallback = htmlwidgets::JS("function(r,d) {$(r).attr('height', '30px')}"),
          dom = 'ft',
          pageLength = nrow(data),
          searchHighlight = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(width = "150px", targets = "_all")),
          scrollX = TRUE,
          language = list(
            search = "<i class='glyphicon glyphicon-search'></i>"
          )
        ))
        
        output$frame.summary.table =
          renderTable({
            compare.stats[["frame.summary.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        output$comparison.summary.table =
          renderTable({
            compare.stats[["comparison.summary.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        output$vars.ns.table =
          renderTable({
            compare.stats[["vars.ns.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        output$obs.table =
          renderTable({
            compare.stats[["obs.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        output$diffs.byvar.table =
          renderTable({
            compare.stats[["diffs.byvar.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        output$diffs.table =
          renderTable({
            compare.stats[["diffs.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        output$attrs.table =
          renderTable({
            compare.stats[["attrs.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        
        observe({
          my_global_env <- globalenv()
        })
      }
    ),
    viewer = paneViewer(minHeight = 500)
  )
}

