save.file.app <- function(auth.filename, new.data) {
  runGadget(
    app = shinyApp(
      ui <- bootstrapPage(
        tags$head(tags$style(
          HTML("pre { overflow: auto; word-wrap: normal; }")
        )),
        theme = theme.selection,
        shinyjs::useShinyjs(),
        input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        br(),
        navset_tab(nav_panel(
          strong("Download"),
        column(
          width = 12,
          h5(strong("Download the new data file.")),
          textInput(
            "new.name",
            label =  h5("File name:"),
            value = paste0(
              gsub(
                paste0("\\", vers.suffix, ".*"),
                "",
                auth.filename,
                ignore.case = TRUE
              ),
              "_",
              suppressWarnings(as.numeric(ifelse(
                grepl(paste0("\\", vers.suffix), auth.filename, ignore.case = TRUE),
                sub(paste0(".*\\", vers.suffix), "", auth.filename, ignore.case = TRUE),
                ""
              )))
            ),
            width = "300px"
          ),
          dipsaus::actionButtonStyled("reset", "Reset", width = "100px"
                                      # , type ="danger"
                                      ),
          br(),
          br(),
          br(),
          downloadButton("download", "Download"),
          actionButton("action", "Done", width = "100px", class = "btn-primary"),
          br(),
          br()
        )),
        nav_panel(
          strong("Review"), 
          column(width = 12, 
                 h5(strong("")), 
                 DT::DTOutput('df', height = "350px"))
        ))
      ), 
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        new.name <- reactive(input$new.name)
        
        
        observeEvent(input$reset, {
          shinyjs::reset("new.name")
          shinyjs::reset("new.name")
        })
        
        
        output$df = DT::renderDataTable({
          new.data %>% dplyr::select(tidyselect::all_of(selected.col()))
        }, rownames = FALSE, options = list(
          rowCallback = htmlwidgets::JS("function(r,d) {$(r).attr('height', '30px')}"),
          lengthMenu = c(5, 10, 25, 50, 100),
          pageLength = 100,
          searchHighlight = TRUE,
          columnDefs = list(list(width = "500px", className = 'dt-left', targets = "_all")),
          scrollX = TRUE
        ))
        
        
        output$download <- downloadHandler(
          filename = paste0(new.name(),".csv"),
          content = function(file) {
            write.csv(new.data, file, na = "", row.names = FALSE)
          }
        )
        
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}
