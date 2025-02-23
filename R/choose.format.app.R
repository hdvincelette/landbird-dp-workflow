

choose.format.app <- function(formats, filename) {
  runGadget(
    app = shinyApp(
      ui <-
        bootstrapPage(
          tags$head(tags$style(
            HTML("pre { overflow: auto; word-wrap: normal; }")
          )),
          theme = theme.selection,
          shinyjs::useShinyjs(),
          # input_dark_mode(id = "dark_mode", mode = "light"),
          br(),
          column(width = 12, uiOutput("dfmsg")),
          br(),
          column(
            width = 12,
            h4(strong("Select formats for export")),
            shinyWidgets::checkboxGroupButtons(
              "selected.format",
              h5(strong("")),
              choices =  formats,
              selected = formats,
              direction = "vertical",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            br(),
            actionButton("action", "Submit"),
            br(),
            br()
          )
        ), 
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        output$dfmsg <- renderUI(
          HTML(paste("<em>",filename,"</em>"))
        )
        
        selected.format <- reactive(input$selected.format)
        
        
        observe({
          my_global_env <- globalenv()
          my_global_env$selected.format <-  input$selected.format
        })
        
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}
