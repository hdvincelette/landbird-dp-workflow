


select.download.app <- function(dir, ext, multiple) {
  if (missing(dir)) {
    dir = "/*"
  } else {
    dir = paste0(gsub("/$","",dir),"/*")
  }
  if (missing(ext)) {
    ext = ".*"
  } 
  if (missing(multiple)) {
    multiple = TRUE
  } 
  
  runGadget(
    app = shinyApp(
      ui <-
        fluidPage(
          bootstrapPage(
            theme = theme.selection,
            shinyjs::useShinyjs(),
            br(),
            column(
              width = 12,
              h4(strong("Select file(s) to download")),
              htmlOutput("ext"),
              textInput("path", ""),
              textInput("path2", ""),
              div(
                style = "display: inline-block; vertical-align:center; horizontal-align:center",
                class = "row-fluid",
                actionButton("browse", "Browse files", width = "150px"),
                actionButton("browse2", "Choose directory", width = "150px"),
                actionButton("action", "Download", width = "150px")
              ),
              br(),
              br(),
              tags$style(
                HTML(
             ".dataTables_wrapper .dataTables_length,
              .dataTables_wrapper .dataTables_filter,
              .dataTables_wrapper .dataTables_info,
              .dataTables_wrapper .dataTables_processing,
              .dataTables_wrapper .dataTables_paginate {
                                        color:#ffffff;
                                         }
                                         thead {
                                           color:#ffffff;
                                         }
                                         tbody {
                                           color:#ffffff;
                                         }"
                  
                )
              ),
            htmlOutput("msg"),
             br(),
             htmlOutput("msg2"),
              DT::DTOutput('content.df', height = "200px"),
              br(),
              htmlOutput("wrn")
            )
          )
        ),
      
      server <- function(input, output, session) {
        
        pattern <- reactive(input$pattern)
        path <- reactive(input$path)
        path2 <- reactive(input$path2)
        
        shinyjs::hide("path")
        shinyjs::hide("path2")
        
        output$ext <- renderUI({
          str <-
            paste0(ext, collapse = ", ")
          
          if (length(ext) > 1 || ext != ".*") {
            HTML(paste0("Accepted formats: ", str))
          }
        })
        
        
        content <- reactive({
          strsplit(path(), split = ",", fixed = TRUE)[[1]] %>%
            .[seq_along(.) %in%
                which(stringr::str_detect(., stringr::regex(paste(ext, collapse = "|"), 
                                                            ignore_case = T)))] %>%
            fs::file_info() %>%
            dplyr::select(path, size, permissions, modification_time) %>%
            tibble::add_column(filename = basename(.$path), .after = 1) %>%
            dplyr::mutate(path = dirname(.$path)) %>%
            dplyr::mutate(permissions = as.character(fs::fs_perms(permissions)))
        })
        
        observe({
          if (input$browse == 0)
            return()
          
          output$content.df = DT::renderDataTable({
            content()
          }
          , rownames = FALSE, options = list(
            dom = 't',
            rowCallback = htmlwidgets::JS("function(r,d) {$(r).attr('height', '30px')}"),
            lengthMenu = c(100, 200),
            pageLength = 100,
            columnDefs = list(list(
              width = "20px",
              className = 'dt-left',
              targets = "_all"
            )),
            scrollX = TRUE
          )
          )
        })
      
        output$msg <- renderUI({
          if (input$browse2 != 0) {
          HTML(paste("<b>Directory: </b>", path2(), sep = '<br/>'))
        }
        
        })
        
        output$msg2 <- renderUI({
          if (input$browse != 0) {
            HTML(paste("<b>Files: </b>"))
          }
          
        })
        
        output$wrn <- renderUI({
          invalid.paths <- strsplit(path(), 
                                    split = ",", fixed = TRUE)[[1]] %>%
            .[seq_along(.) %in%
                which(stringr::str_detect(., stringr::regex(paste(ext, collapse = "|"), ignore_case = T)) == FALSE)] %>%
            basename(.)
          
          if (input$browse != 0 & 
              length(invalid.paths) >=1) {
            str <-
              paste0(invalid.paths, collapse = "<br>")
            
            HTML(
              paste0(
                "<em>The following files are an incompatible format:<br>",
                str,
                "</em>",
                collapse="<br>"
              )
            )
            
            
          }
        })
        
        observeEvent(eventExpr = {
          input$path
          input$path2
        }, handlerExpr = {
          if (nrow(content()) == 0 | path2() == "") {
            shinyjs::disable("action")
          } else {
            shinyjs::enable("action")
          }
        })
        
        # observeEvent(input$path, {
        #   if (nrow(content()) == 0) {
        #     shinyjs::disable("action")
        #   } else {
        #     shinyjs::enable("action")
        #   }
        # })
        
        
        observeEvent(input$action, stopApp())
        
        observe({
          if (input$browse == 0)
            return()
          
          updateTextInput(session,
                          "path",
                          value = utils::choose.files(multi = multiple, default = dir))
          
        })
        
        observe({
          if (input$browse2 == 0)
            return()
        
          updateTextInput(session,
                          "path2",
                          value = utils::choose.dir())
          
        })
      
        
        observe({
          my_global_env <- globalenv()
          my_global_env$selected.files <- content()
          my_global_env$selected.dir <- path2()
        })
      }
    ),
    viewer = paneViewer(minHeight  = 500)
  )
  
}
