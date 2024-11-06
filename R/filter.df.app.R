

filter.df.app <- function(data.list, species.col, date.col, species, min.date, max.date) {
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
            data.list %>%
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
