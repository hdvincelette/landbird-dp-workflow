theme.selection<- shinythemes::shinytheme("simplex")


correct.colname.app <- function(data, invalid.col, unused.cols) {
  runGadget(
    app = shinyApp(
      ui <- bootstrapPage(
        tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
        theme = theme.selection,
        shinyjs::useShinyjs(),
        input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        br(),
        navset_tab(nav_panel(
          strong("Choice"),
          column(
            width = 12,
            h5(strong(
              paste0(
                "The variable '",
                invalid.col,
                "' is not listed in the data dictionary."
              )
            )),
            shinyWidgets::radioGroupButtons(
              "decision",
              h5(strong("Choose an option.")),
              choices =  c("keep", "remove", "replace", "rename"),
              direction = "vertical",
              width = "200px",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            
            conditionalPanel(
              condition = "input.decision == 'replace'",
              shinyWidgets::pickerInput(
                "col.choice",
                h5("Select an unused dictionary attribute:"),
                choices = unused.cols,
                width = "300px",
                options = list(
                  `live-search` = TRUE,
                  title = "Nothing selected",
                  style = "btn-primary"
                )
              )
            ),
            conditionalPanel(
              condition = "input.decision == 'rename'",
              textInput(
                "col.choice",
                h5("Enter a new column name:"),
                value = "",
                width = "300px"
              )
            ),
            actionButton("action", "Submit"),
            br(),
            br(),
            span(textOutput("wrn"), style = "color:red"),
            br()
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
        
        )), 
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        output$col.view = DT::renderDataTable({
          data %>% dplyr::select(tidyselect::all_of(invalid.col))
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
            tibble::enframe(purrr::flatten(sapply(dataMaid::summarize(data[[invalid.col]]),"[","result"))) %>% 
              dplyr::mutate(name = gsub("\\..*", "", name))
          },
          rownames = FALSE,
          colnames = FALSE,
          width = "500px")
        
        decision <- reactive(input$decision)
        col.choice <- reactive(input$col.choice)
        
        observe({
          if (decision() %in% "replace" &
              col.choice() == "" |
              decision() %in% "rename" &
              col.choice() %in% colnames(data)) {
            shinyjs::disable("action")
          }
          else{
            shinyjs::enable("action")
          }
        })
        
        
        output$wrn <- renderText({
          if (col.choice() %in%  colnames(data) & decision() %in% "rename") {
            paste0("Error: '", col.choice(), "' is already present in the data.")
          }
        })
        
        observe({
          my_global_env <- globalenv()
          my_global_env$decision<- input$decision
          my_global_env$col.choice <-  input$col.choice
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}

