
add.column.app <- function(missing.cols, add.data) {
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
            shinyWidgets::pickerInput(
              "col.choice",
              h5(strong("Select columns to include in the authoritative file.")),
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
                                       DT::DTOutput('col.view', height = "375px"))
                      ))))
        
        )), 
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        
        selected.col <- reactive(input$selected.col)
        
        output$col.view = DT::renderDataTable({
          add.data %>% dplyr::select(tidyselect::all_of(selected.col()))
        }, rownames = FALSE, options = list(
          rowCallback = htmlwidgets::JS("function(r,d) {$(r).attr('height', '30px')}"),
          lengthMenu = c(5, 10, 25, 50, 100),
          pageLength = 100,
          searchHighlight = TRUE,
          columnDefs = list(list(width = "500px", className = 'dt-left', targets = "_all")),
          scrollX = TRUE
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
