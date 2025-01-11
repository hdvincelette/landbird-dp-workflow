

add.domainItem.app <- function(variable, invalid.values, ref.dxnry) {
  runGadget(
    app = shinyApp(
      ui <- bootstrapPage(
        tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
        theme = theme.selection,
        shinyjs::useShinyjs(),
        input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        br(),
        column(
          width = 12,
          h5(strong(
            paste0("\nThe variable '",
                   variable,
                   "' contains entry values not found in the data dictionary"
            )
          )),
          shinyWidgets::prettyCheckboxGroup(
            "domainItem.choice",
            h5("Select values to add to the dictionary"),
            choices =  invalid.values,
            outline = TRUE,
            status = "warning"
          ),
          actionButton("action", "Submit"),
          br(),
          br(),
          hr(),
          h5(strong("Definition")),
          htmlOutput("defs"),
          br(),
          h5(strong("Entry values")),
          DT::DTOutput('domainItems', height = "375px")
        ),
        
      ), 
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        domainItem.choice <- reactive(input$domainItem.choice)
        
        output$defs <- renderUI({
          HTML(
            paste(
              ref.dxnry %>%
                dplyr::filter(
                  codeName == domainItem_value.table$Variable[b],
                  domainItem_value == "dataField"
                ) %>%
                purrr::pluck("definition")
            )
          )
        })
        
        output$domainItems = DT::renderDT({
          ref.dxnry %>%
            dplyr::filter(codeName == domainItem_value.table$Variable[b],
                          domainItem_value != "dataField") %>%
            dplyr::select("domainItem_value", "definition")
        }, rownames = FALSE, 
        options = list(
          dom = 't',
          lengthChange = FALSE,
          autoWidth = FALSE,
          headerCallback = htmlwidgets::JS(
            "function(thead, data, start, end, display){",
            "  $(thead).remove();",
            "}"
          )
        ))
        
        observe({
          my_global_env <- globalenv()
          my_global_env$domainItem.choice <-  input$domainItem.choice
        })
        
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}


