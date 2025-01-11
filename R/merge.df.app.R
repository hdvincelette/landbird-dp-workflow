


merge.df.app <- function(data.list) {
  runGadget(
    app = shinyApp(
      ui <- bootstrapPage(
        theme = theme.selection,
        input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        br(),
        navset_tab(nav_panel(
          strong("Select"),
          column(
            width = 12,
            shinyWidgets::checkboxGroupButtons(
              "selected.df",
              h5(strong("Select tables to combine.")),
              choices =  names(data.list),
              selected = names(data.list),
              direction = "vertical",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            actionButton("action", "Submit"),
            br(),
            br(),
            span(textOutput("wrn"), style = "color:red")
          )
        ), nav_panel(
          strong("Compare"), 
          column(width = 12, 
                 h5(strong("Multi-class variables.")), 
                 DT::DTOutput('col.compare',
                              height = "375px"
                 ))
        ))
      ),
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        selected.df <- reactive(input$selected.df)
        
        col.compare <- reactive({
          janitor::compare_df_cols(data.list[names(data.list) %in% selected.df()], bind_method  = "bind_rows") %>%
            dplyr::rowwise() %>%
            replace(is.na(.), "N/A") %>%
            dplyr::filter(dplyr::n_distinct(dplyr::c_across(-1)) != 1) %>%
            dplyr::rename(all_of(setNames(1, "variable")))
        })
        
        output$col.compare = DT::renderDataTable({
          if (is.null(selected.df()) == FALSE) {
            col.compare()
          } else {
            tibble::tibble(variable = character())
          }
        }, rownames = FALSE, options = list(
          lengthMenu = c(5, 10, 25, 50, 100),
          pageLength = 100,
          searchHighlight = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(width = "150px", targets = "_all")),
          scrollX = TRUE
        ))
        
        
        output$wrn <- renderText({
          "Warning: variables with incompatible class types will be coerced together."
        })
        
        observe({
          my_global_env <- globalenv()
          my_global_env$selected.df <- input$selected.df
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}