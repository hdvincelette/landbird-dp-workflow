# Apps!

theme.selection<- shinythemes::shinytheme("simplex")


filter.df.app <- function(species.col, date.col, species, min.date, max.date) {
  if (missing(species.col) |
      species.col == "") {
    species.col = ".ERROR"
  }
  if (missing(date.col) |
      date.col == "") {
    date.col = ".ERROR"
  }
  if (missing(species)) {
    species = character(0)
  }
  if (missing(min.date)) {
    min.date = as.Date("0001-01-01")
  }
  if (missing(max.date)) {
    max.date = as.Date("0001-01-01")
  }
  
  runGadget(
    app = shinyApp(
      ui <- bootstrapPage(
        theme = theme.selection,
        shinyjs::useShinyjs(),
        br(),
        column(
          width = 12,
          if(length(species) != 0){
          shinyWidgets::pickerInput(
            "selected.species",
            h5(strong("Select one or more species.")),
            choices =  species,
            selected = species,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE)
          )},
          
          if(min.date != "0001-01-01" | 
             max.date != "0001-01-01"){
          dateRangeInput(
            "selected.dates",
            h5(strong("Select a date range.")),
            start = min.date,
            end = max.date,
            min = min.date,
            max = max.date,
            format = "yyyy-mm-dd"
          )},
          actionButton("action", "Submit", width = "100px"), 
          dipsaus::actionButtonStyled("reset", "Reset", width = "100px", type="primary"), 
          br(),
          br(),
          hr(),
          br(),
          h5(strong("Number of selected rows")),
          br(),
          tableOutput("selected.nrow"),
          textOutput("selected.rowsum")
          
        )
      ),
      
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        selected.species <- reactive(input$selected.species)
        selected.dates <- reactive(input$selected.dates)
        
        observeEvent(input$reset, {
          shinyjs::reset("selected.species")
          shinyjs::reset("selected.dates")
        })
        
        selected.nrow <- reactive({
          as.data.frame(lapply(
            raw.data.list %>%
              purrr::map(
                .f = ~ .x %>%
                  dplyr::filter(if_any(
                    matches(species.col), ~ .data[[species.col]] %in% selected.species())) %>%
                  dplyr::mutate_at(dplyr::vars(tidyselect::any_of(date.col)), as.Date, format = date.format) %>%
                  dplyr::filter(if_any(
                    matches(date.col),
                    ~ data.table::between(.data[[date.col]], min(selected.dates()), max(selected.dates()), NAbounds =
                                            FALSE)
                  ))
              ),
            nrow
          ))
          
        })
        
        output$selected.nrow <- renderTable({
          selected.nrow()
        })
        
        
        output$selected.rowsum <- renderText({
          paste("Total: ", rowSums(selected.nrow()))
        })
        
        observe({
          if (rowSums(selected.nrow()) == 0) {
            shinyjs::disable("action")
          }
          else{
            shinyjs::enable("action")
          }
        })
        
        observe({
          my_global_env <- globalenv()
          my_global_env$selected.species <- input$selected.species
          my_global_env$selected.dates <- input$selected.dates
        })
      }
    ),
    viewer = paneViewer(minHeight  = 500)
  )
}


merge.df.app <- function(data.list) {
  runGadget(
    app = shinyApp(
      ui <- bootstrapPage(
        theme = theme.selection,
        input_dark_mode(id = "dark_mode", mode = "light"),
        shinyjs::useShinyjs(),
        br(),
        navset_tab(nav_panel(
          strong("Select"),
          column(
            width = 12,
            shinyWidgets::checkboxGroupButtons(
              "selected.df",
              h5(strong("Select one or more to combine.")),
              choices =  names(data.list),
              selected = names(data.list),
              direction = "vertical",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            actionButton("action", "Submit"),
            br(),
            br(),
            span(textOutput("wrn"), style = "color:red")
          )
        ), nav_panel(
          strong("Compare"), 
          column(width = 12, 
                 h5(strong("Multi-class variables.")), 
                 DT::DTOutput('col.compare',
                           height = "375px"
                           ))
        ))
      ),
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())
        
        selected.df <- reactive(input$selected.df)
        
        col.compare <- reactive({
          janitor::compare_df_cols(data.list[names(data.list) %in% selected.df()], bind_method  = "bind_rows") %>%
            dplyr::rowwise() %>%
            replace(is.na(.), "N/A") %>%
            dplyr::filter(dplyr::n_distinct(dplyr::c_across(-1)) != 1) %>%
            dplyr::rename(all_of(setNames(1, "variable")))
        })
       
          output$col.compare = DT::renderDataTable({
            if (is.null(selected.df()) == FALSE) {
              col.compare()
            } else {
              tibble::tibble(variable = character())
            }
          }, rownames = FALSE, options = list(
            lengthMenu = c(5, 10, 25, 50, 100),
            pageLength = 100,
            searchHighlight = TRUE,
            autoWidth = TRUE,
            columnDefs = list(list(width = "150px", targets = "_all")),
            scrollX = TRUE
          ))
        
        
        output$wrn <- renderText({
          "Warning: variables with incompatible class types will be coerced together."
        })
        
        observe({
          my_global_env <- globalenv()
          my_global_env$selected.df <- input$selected.df
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}


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
        
        results <- reactive({
          x.list <- list()
          for (i in 1:10) {
            x.new_draw <- x.new(input$x.qty)
            repeat {
              if (x.new_draw > 20) {
                x.list <- c(x.list, x.new_draw)
                break
              } # end if loop
              x.new_draw <- x.new(x.new_draw)
            } # end repeat
          } # end for loop
          x.list
        })
        
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


select.value.app <- function(variable, invalid.values) {
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
                     "' contains entry values not found in the data dictionary.\n"
              )
            )),
            shinyWidgets::awesomeCheckboxGroup(
              "selected.values",
              h5("Select values to correct in the data."),
              choices =  invalid.values
            ),
            actionButton("action", "Submit"),
            br(),
            br(),
            hr(),
            
          ),
        
        ), 
      
      server <- function(input, output, session) {
        observeEvent(input$action, stopApp())

        
        observe({
          my_global_env <- globalenv()
          my_global_env$selected.values <-  input$selected.values
        })
      }
    ),
    
    viewer = paneViewer(minHeight = 500)
  )
}

select.value.app(variable, invalid.values)

