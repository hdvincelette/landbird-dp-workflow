

save.file.app <- function(dir, proposed.filename, ref.filename, format) {
  if (missing(dir)) {
    dir = "/*"
  } else {
    dir = paste0(gsub("/$","",dir),"/*")
  }
  if (missing(ref.filename)) {
    ref.filename = ""
  }
  if (missing(format)) {
    format = ""
  } else {
    format<- paste0(" ",format)
  }
  
  runGadget(
    app = shinyApp(
      ui <-
        fluidPage(
          bootstrapPage(
            theme = theme.selection,
            shinyjs::useShinyjs(),
            br(),
            column(width = 12, uiOutput("dfmsg")),
            br(),
            column(
              width = 12,
              h4(strong(paste0("Export the new",format," file"))),
              textInput( 
                "new.name", 
                "Filename:", 
                value = proposed.filename
              ),
              span(textOutput("wrn"), style = "color:red"),
              br(),
              textInput("path", ""),
              div(
                style = "display: inline-block; vertical-align:center; horizontal-align:center",
                class = "row-fluid",
                actionButton("browse", "Choose directory", width = "150px"),
                actionButton("action", "Export", width = "150px")
              ),
              br(),
              br(),
              htmlOutput("msg"),
              br()
            )
          )
        ),
      
      server <- function(input, output, session) {
        
        output$dfmsg <- renderUI(
          HTML(paste("<em>",ref.filename,"</em>"))
        )
        
        path <- reactive(input$path)
        new.name <- reactive(input$new.name)
        selected.format <- reactive(input$selected.format)

        
        shinyjs::hide("path")
        
        observe({
          if (input$browse == 0)
            return()
          
          updateTextInput(session,
                          "path",
                          value = rstudioapi::selectDirectory(path = dir)
                            # utils::choose.dir(default = dir)
                          
                          )
        })
        

        output$wrn <- renderText({
          if (input$browse != 0 & path() != "") {
            if (paste0(tools::file_path_sans_ext(new.name()), ".csv") %in% list.files(path = path())) {
            paste0("Warning: a file with this name already exists in the directory")
            }
          }
        })
      
        output$msg <- renderUI({
          if (input$browse != 0) {
            HTML(paste("<b>Directory: </b>", path(), sep = '<br/>'))
          }
          
        })
        
        
        observeEvent(eventExpr = {
          input$path
        }, handlerExpr = {
          if (path() == "") {
            shinyjs::disable("action")
          } else {
            shinyjs::enable("action")
          }
        })
        
        
        observeEvent(input$action, stopApp())
        
        
        observe({
          if (input$browse == 0)
            return()
          
        })
        
        
        observe({
          my_global_env <- globalenv()
          my_global_env$selected.dir <- path()
          my_global_env$new.name <- new.name()
        })
      }
    ),
    viewer = paneViewer(minHeight  = 500)
  )
  
}


