project.folder <- dir(
  path = paste0("//ifw7ro-file.fws.doi.net/datamgt/mbm/"),
  pattern = paste0("^mbmlb.*", RDR.keyword),
  full.names = FALSE,
  recursive = FALSE,
  ignore.case = TRUE
)

keywords <- c(data.keyword, 
              template.keyword, 
              dictionary.keyword)
subfolders <- c(data.subfolder, 
                template.subfolder, 
                dictionary.subfolder)

for(x in seq_along(keywords)) {
  if (length(keywords[x]) != 0) {
    if (is.na(keywords[x]) == FALSE) {
      
      select.download.app(
        path = paste0(
          "//ifw7ro-file.fws.doi.net/datamgt/mbm/",
          project.folder,
          "/",
          subfolders[x]
        ),
        pattern = keywords[x]
      )
      
      if (download.dir$root == "R Project") {
        root.dir <- dirname(rstudioapi::documentPath())
      } else {
        root.dir <- shinyFiles::getVolumes()()[which(stringr::str_detect(fs::path_home(), shinyFiles::getVolumes()()))]
      }
      
      download.path <- paste0(root.dir, paste(unlist(download.dir[["path"]]), collapse = "/"))
      
      
      for (y in selected.files) {
        file.path <- file.paths[which(basename(file.paths) %in% y)]
        downloader::download(
          url = paste0("File:", file.path),
          destfile = file.path(download.path, basename(file.path)),
          method = "auto",
          mode = "wb",
          quiet = FALSE
        )
      }
      
    }
  }
}

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
        ui <- 
          page(
          bootstrapPage(
          theme = theme.selection,
          shinyjs::useShinyjs(),
          br(),
          column(
            width = 12,
           checkboxGroupInput(
              "selected.files",
              h5(strong("Select file(s) to download")),
              choices =  files
            ),
            htmlOutput("selected.dir"),
            br(),
            shinyFiles::shinyDirButton('dir', 'Choose directory', 'Choose directory'),
            actionButton("action", "Download", width = "100px", class = "btn-primary"),
            br(),
            br()
            
          )
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




