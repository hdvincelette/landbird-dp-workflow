
theme.selection<- shinythemes::shinytheme("simplex")


correct.value.app <- function(variable, invalid.value, ref.dxnry) {
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
              paste0(
                "The value '",
                invalid.value,
                "' is not listed in the data dictionary."
              )
            )),
            shinyWidgets::radioGroupButtons(
              "decision",
              h5(strong("Choose an option.")),
              choices =  c("keep", "edit","replace"),
              direction = "vertical",
              width = "200px",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            
            conditionalPanel(
              condition = "input.decision == 'replace'",
              shinyWidgets::pickerInput(
                "val.choice",
                h5("Select a dictionary value:"),
                choices = ref.dxnry %>%
                  dplyr::filter(codeName == domainItem_value.table$Variable[b],
                                domainItem_value != "dataField") %>%
                  purrr::pluck("domainItem_value"),
                width = "300px",
                options = list(
                  `live-search` = TRUE,
                  title = "Nothing selected",
                  style = "btn-primary"
                )
              )
            ),
            conditionalPanel(
              condition = "input.decision == 'edit'",
              textInput(
                "val.choice",
                h5("Enter a replacement value:"),
                value = invalid.value,
                width = "300px"
              )
            ),
            actionButton("action", "Submit"),
            br(),
            br()
          ),
        ), 
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
      
        decision <- reactive(input$decision)
        val.choice <- reactive(input$val.choice)
        
        observe({
          if (decision() %in% c("replace","edit") &
              val.choice() == "" ) {
            shinyjs::disable("action")
          }
          else{
            shinyjs::enable("action")
          }
        })
        
        observe({
          my_global_env <- globalenv()
          my_global_env$decision<- input$decision
          my_global_env$val.choice <-  input$val.choice
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}

correct.value.app(variable, invalid.value, ref.dxnry)
