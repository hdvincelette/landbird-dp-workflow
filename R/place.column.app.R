
place.column.app <- function(new.col, existing.cols) {
  runGadget(
    app = shinyApp(
      ui <- page(bootstrapPage(
        tags$head(tags$style(
          HTML("pre { overflow: auto; word-wrap: normal; }")
        )),
        theme = theme.selection,
        shinyjs::useShinyjs(),
        # input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        br(),
        column(
          width = 12,
          shinyWidgets::pickerInput(
            "loc.choice",
            h5(strong(
              paste0("Insert '", new.col, "' before which column?")
            )),
            selected = NULL,
            choices =  existing.cols,
            multiple = FALSE,
            options = list(
              `live-search` = TRUE,
              title = "Nothing selected",
              style = "btn-primary"
            )
          ),
          shinyWidgets::materialSwitch("last",
                                       label = "After last column",
                                       value = FALSE,
                                       status = "default"),
          span(textOutput("wrn"), style = "color:red"),
          br(),
          actionButton("action", "Submit"),
          br(),
          br()
        )
        
      )), 
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        loc.choice <- reactive(input$loc.choice)
        
        
        output$wrn <- renderText({
          "Warning: unplaced columns will be omitted."
        })
        
        observeEvent(input$last, {
          if (input$last==TRUE) {
            shinyjs::reset("loc.choice")
            shinyjs::disable("loc.choice")
          } else {
            shinyjs::reset("loc.choice")
            shinyjs::enable("loc.choice")
          }
        })
        
        
        observe({
          my_global_env <- globalenv()
          my_global_env$loc.choice <-  input$loc.choice
          my_global_env$last <-  input$last
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}
