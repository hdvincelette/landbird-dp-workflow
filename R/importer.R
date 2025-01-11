#' Import one or more files into R
#'
#' Supports csv, Excel, json, gpx, and/or kml files.
#' @param path Character string. Directory path where files are located. Default is the working directory, getwd().
#' @param files Character vector. Name(s) of files to import.
#' @param column.format Optional list. Specifies formatting for one or more data frame columns, particularly those of class "Date."
#' @return Returns an R list object (multiple = TRUE or the imported file is json format) or data frame (when multiple = FALSE and the imported file is not json format).
#' @keywords import, csv, xlsx, xls, json
#' @seealso ``` ```
#' @export
#' @examples
#' # e.g.import <- importer(path = "downloads/)
#' 
#' 

## Remove all na/"NA" rows

importer <-
  function(path, 
           files, 
           column.format = list(c(column = NULL, format = NULL))) {
    if (missing(path)) {
      path = getwd()
    }
    if (missing(column.format)) {
      column.format = list(c(column = NULL, format = NULL))
    }
    if(length(files)==0){
      stop(" files names to import.")
    }
    
    file.ext <- tools::file_ext(files)
    
    
    if (endsWith(path, "/") == FALSE) {
      path <- paste0(path, "/")
    }
    if (startsWith(path, "/") == TRUE) {
      path <- gsub("^(/*)*", "", path)
    }
    
    
    file.list <- list()
    
   
    for (a in 1:length(file.ext)) {
      if (grepl(pattern = "xlsx|xls",
                x = file.ext[a],
                ignore.case = TRUE) == TRUE) {
        import.file <-
          readxl::read_excel(paste0(path, files[a]))
        
      } else if (grepl(pattern = "csv",
                       x = file.ext[a],
                       ignore.case = TRUE) == TRUE) {
        import.file <-
          utils::read.csv(paste0(path, files[a]))
        
      } else if (grepl(pattern = "json",
                       x = file.ext[a],
                       ignore.case = TRUE) == TRUE) {
        if (fs::dir_exists(path) == FALSE) {
          full.path <- paste0(getwd(), "/", path)
        } else{
          full.path <- path
        }
        
        import.file <-
          rjson::fromJSON(file = paste0(full.path, files[a]))
        
      } else if (grepl(pattern = "gpx|kml",
                       x = file.ext[a],
                       ignore.case = TRUE) == TRUE) {
        import.file <-
          sf::st_read(paste0(path, files[a]), quiet = TRUE) %>%
          dplyr::mutate(
            Longitude = sf::st_coordinates(.)[, 1],
            Latitude = sf::st_coordinates(.)[, 2]
          ) %>%
          sf::st_drop_geometry(.)
      }
      
      # Fix date/time columns
      if (grepl(pattern = "csv|xlsx|xls",
                x = file.ext[a],
                ignore.case = TRUE) == TRUE) {
        for (x in 1:length(column.format)) {
          column <- column.format[[x]][["column"]]
          format <- column.format[[x]][["format"]]
          
          if (is.null(column) == FALSE) {
            if (column %in% colnames(import.file)) {
              import.file <- import.file %>%
                dplyr::mutate({{column}} := format(.data[[column]], format = format))
            }
          }
        }
      }
      
      
      file.list[[a]] <- import.file
      
    }
    
    
    names(file.list) <- tools::file_path_sans_ext(files)
    return(file.list)
  }
