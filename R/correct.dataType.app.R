

correct.dataType.app<- function(variable, 
                                dxnry.dataType, 
                                detected.dataType, 
                                filename, 
                                data){
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
        navset_tab(
          nav_panel(
            strong("Choice"),
            column(
              width = 12,
              h5(strong(
                paste0(
                  "The data type reported for '", variable ,"' in the data dictionary appears invalid"
                )
              )),
              uiOutput("msg"),
              br(),
              shinyWidgets::radioGroupButtons(
                "decision",
                h5("Choose an option"),
                choices =  c("keep", "update"),
                direction = "vertical",
                width = "200px",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              ),
              conditionalPanel(
                condition = "input.decision == 'update'",
                shinyWidgets::pickerInput(
                  "dataType.choice",
                  h5("Select a data type:"),
                  choices = dataType.vector[!dataType.vector %in% dxnry.dataType],
                  selected = detected.dataType,
                  width = "300px",
                  options = list(
                    `live-search` = TRUE,
                    title = "Nothing selected",
                    style = "btn-primary"
                  )
                )
              ),
              actionButton("action", "Submit"),
              br(),
              br(),
              hr(),
              h5(strong("Data type definitions")),
              br(),
              DT::DTOutput('datatype.rules', height = "500px")
            )
          ),
          nav_panel(strong("Review"), 
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
                      )))
          )
          
        ),
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        output$datatype.rules = DT::renderDT({
          datatype.rules %>%
            dplyr::select("value", "definition")
        }, rownames = FALSE, options = list(
          dom = 't',
          lengthChange = FALSE,
          autoWidth = FALSE,
          headerCallback = htmlwidgets::JS(
            "function(thead, data, start, end, display){",
            "  $(thead).remove();",
            "}"
          )
        ))
        
        output$col.view = DT::renderDataTable({
          data %>% dplyr::select(tidyselect::all_of(variable))
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
            tibble::enframe(purrr::flatten(sapply(dataMaid::summarize(data[[variable]]),"[","result"))) %>% 
              dplyr::mutate(name = gsub("\\..*", "", name))
          },
          rownames = FALSE,
          colnames = FALSE,
          width = "500px")
        
        output$msg <- renderText({
          HTML(
            paste(
              "Current: ",
              dxnry.dataType,
              "<br>",
              "Detected: ",
              detected.dataType,
              sep = ""
            )
          )
        })
      
        decision <- reactive(input$decision)
        dataType.choice <- reactive(input$dataType.choice)
        
        
        observe({
          my_global_env <- globalenv()
          my_global_env$decision <- input$decision
          my_global_env$dataType.choice <-  input$dataType.choice
        })
        
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
  
  
}

