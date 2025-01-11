
select.import.app <- function(path, pattern, multiple) {
  if (missing(path)) {
    path = getwd()
  }
  if (missing(pattern)) {
    pattern = NULL
  } else{
    pattern = paste0(pattern, collapse = "|")
  }
  if (missing(multiple)) {
    multiple = TRUE
  } 
  
  files <-
    sort(list.files(
      path = path,
      pattern = pattern,
      full.names = FALSE,
      recursive = FALSE
    ))
  
  files <- files[grepl(".xlsx$|.xls$|.csv$|.json$|.gpx$|.kml$", files)]
  
  if (length(files >= 1)) {
    runGadget(
      app = shinyApp(
        ui <- bootstrapPage(
          theme = theme.selection,
          shinyjs::useShinyjs(),
          br(),
          column(
            width = 12,
            conditionalPanel(
              condition = "output.multiple==true",
              shinyWidgets::prettyCheckboxGroup(
              "selected.files",
              h5(strong("Select file(s) to import.")),
              choices =  files,
              shape = "square"
            )),
            conditionalPanel(
              condition = "output.multiple == false",
              shinyWidgets::prettyRadioButtons(
                "selected.files",
                h5(strong("Select a file to import.")),
                choices =  files,
                shape = "square",
                status = "default"
              )),
            actionButton("action", "Submit", width = "100px"),
            dipsaus::actionButtonStyled("reset", "Reset", width = "100px", type =
                                          "primary"),
            br(),
            br()
            
          )
        ),
        
        server <- function(input, output, session) {
          
          output$multiple <- reactive({
            multiple
          })
          
          outputOptions(output, 'multiple', suspendWhenHidden=FALSE)
            
          observeEvent(input$action, stopApp())
          
          decision <- reactive(input$decision)
          
          selected.files <- reactive(input$selected.files)
          
          observeEvent(input$reset, {
            shinyjs::reset("selected.files")
          })
          
          observe({
            my_global_env <- globalenv()
            my_global_env$selected.files <- input$selected.files
          })
        }
      ),
      viewer = paneViewer(minHeight  = 500)
    )
  } else {
    selected.files <- character(0)
    message(cat(
      paste0(
        "No compatible files found at '",
        path,
        "'.\nSupported formats include xlsx, .xls, csv, json, gpx, and kml."
      )
    ))
  }
}




