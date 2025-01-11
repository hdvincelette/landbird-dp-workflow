

select.download.app <- function(path, pattern) {
  if (missing(path)) {
    path = getwd()
  }
  if (missing(pattern)) {
    pattern = NULL
  } else{
    pattern = paste0(pattern, collapse = "|")
  }
  
  
  file.paths <-
    sort(list.files(
      path = path,
      pattern = pattern,
      full.names = TRUE,
      recursive = FALSE,
      ignore.case = TRUE
    ))
  
  files<- basename(file.paths)
  
  if (length(files >= 1)) {
    runGadget(
      app = shinyApp(
        ui <- bootstrapPage(
          theme = theme.selection,
          shinyjs::useShinyjs(),
          br(),
          column(
            width = 12,
            shinyWidgets::prettyCheckboxGroup(
              "selected.files",
              h5(strong("Select file(s) to download")),
              choices =  files,
              shape = "square"
            ),
            htmlOutput("selected.dir"),
            br(),
            shinyFiles::shinyDirButton('dir', 'Choose directory', 'Choose directory'),
            actionButton("action", "Download", width = "100px", class = "btn-primary"),
            br(),
            br()
            
          )
        ),
        
        server <- function(input, output, session) {
          
          observeEvent(input$action, stopApp())
          
          selected.files <- reactive(input$selected.files)
          
          
          dir <- reactive(input$dir)
          
          roots <- c(shinyFiles::getVolumes()()[which(stringr::str_detect(fs::path_home(), shinyFiles::getVolumes()()))],
                        'R Project' = dirname(rstudioapi::documentPath()))
          
          shinyFiles::shinyDirChoose(
            input,
            'dir',
            roots = roots,
            defaultRoot = 'R Project',
            filetypes = c('')
          )
          
          
          observe({
            if (typeof(dir()) != "list") {
              shinyjs::disable("action")
          
            }
            else{
              shinyjs::enable("action")
              
              output$selected.dir <- renderUI({
                HTML(paste(
                  "<b>File(s) will be downloaded to:</b><i>",
                  shinyFiles::parseDirPath(roots, input$dir),"</i>",  
                  sep="<br/>"
                ))
              })
              
            }
          })
        
          
          observe({
            my_global_env <- globalenv()
            my_global_env$file.paths <- file.paths
            my_global_env$selected.files <- input$selected.files
            my_global_env$download.dir <- input$dir
          })
          
        }
      ),
      viewer = paneViewer(minHeight  = 500)
    )
  } else {
    selected.files <- character(0)
    message(cat(
      paste0(
        "No matching files found at '",
        path,"."
      )
    ))
  }
}




