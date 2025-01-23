
compare.dfs.app <- function(df1, df2, compare.df, compare.stats) {
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
        # input_dark_mode(id = "dark_mode", mode = "light"),
        column(width = 12, navset_tab(
          nav_panel(strong("View")),
          nav_panel(
            strong("Compare"),
            br(), 
            bslib::accordion(
              id = "acc",
              open = FALSE,
              bslib::accordion_panel(title = "Summary of data frames",
                                       tableOutput('summary.df')),
              bslib::accordion_panel(title = "Summary of overall comparison",
                                       tableOutput('summary.compare')),
              bslib::accordion_panel(title = "Variables not shared",
                                       tableOutput('diff.variables')),
            ),
            hr(),
            br(),
            DT::DTOutput('compare.df', height = "375px"),
            actionButton("action", "Done"),
            br(),
            br()
          )
        ))
      )
      ),
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        output$compare.df = DT::renderDataTable({
          compare.df
        }, rownames = FALSE, options = list(
          rowCallback = htmlwidgets::JS("function(r,d) {$(r).attr('height', '30px')}"),
          lengthMenu = c(5, 10, 25, 50, 100),
          pageLength = 100,
          searchHighlight = TRUE,
          columnDefs = list(list(
            width = "500px",
            className = 'dt-left',
            targets = "_all"
          )),
          scrollX = TRUE
        ))
        
        output$summary.df =
          renderTable({
            compare.stats[["frame.summary.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        output$summary.compare =
          renderTable({
            compare.stats[["comparison.summary.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        output$diff.variables =
          renderTable({
            compare.stats[["vars.ns.table"]]
          }, rownames = FALSE, colnames = TRUE, width = "500px")
        
        observe({
          my_global_env <- globalenv()
        })
      }
    ),
    viewer = paneViewer(minHeight = 500)
  )
}
