


select.download.app <- function(from.dir, to.dir, ext, multiple, type) {
  if (missing(from.dir)) {
    from.dir = "/*"
  } else {
    from.dir = paste0(gsub("/$","",from.dir),"/*")
  }
  if (missing(to.dir)) {
    to.dir = "/*"
  } else {
    to.dir = paste0(gsub("/$","",to.dir),"/*")
  }
  if (missing(ext)) {
    ext = ".*"
  } 
  if (missing(multiple)) {
    multiple = TRUE
  } 
  if (missing(type)) {
    type = ""
  } else{
    type<- paste0(" ",type)
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
              h4(strong(paste0("Select a", type ," file to download"))),
              htmlOutput("ext"),
              br(),
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
                  "table.dataTable tbody tr.selected td,
                       table.dataTable tbody tr.selected td,
                       table.dataTable tbody td.selected {
                       border-top-color: #bcbcbc !important;
                       box-shadow: inset 0 0 0 9999px #bcbcbc !important;
                       color: black;
                       }
                       table.dataTable tbody tr:active td {
                       background-color: #bcbcbc !important;
                       }
                       :root {
                       --dt-row-selected: transparent !important;
                       }
                       table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
                       background-color: #bcbcbc !important;
                        }
                      .dataTables_wrapper .dataTables_length,
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
            htmlOutput("wrn"),
            br(),
            htmlOutput("msg2"),
            DT::DTOutput('content.df', height = "250px")
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
          }, rownames = FALSE, options = list(
            dom = 'ft',
            pageLength = nrow(data),
            searchHighlight = TRUE,
            columnDefs = list(list(width = "150px", targets = "_all")),
            scrollX = TRUE,
            language = list(
              search = "<i class='glyphicon glyphicon-search'></i>"
            )
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
            HTML(paste("<b>Selected files: </b>"))
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
                "<em><font size='2px'color='#bcbcbc'>The following files are an incompatible format:<br>",
                str,
                "</em></font>",
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
                          value = utils::choose.files(multi = multiple, default = from.dir))
          
        })
        
        observe({
          if (input$browse2 == 0)
            return()
        
          updateTextInput(session,
                          "path2",
                          value = utils::choose.dir(default = to.dir))
          
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
