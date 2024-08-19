#' Import one or more files into R
#'
#' Supports csv, Excel, and/or json files.
#' @param path Character string. Directory path here files are located. Default is the working directory, getwd().
#' @param multiple Logical. Whether to allow multiple file selections. This will always return an R list object. Default is FALSE.
#' @param graphics Logical. Whether to use a graphical widget for file selection. Default is TRUE.
#' @param formats Optional list. Specifies formatting for one or more data frame columns, particularly those of class "Date."
#' @return Returns an R list object (multiple = TRUE or the imported file is json format) or data frame (when multiple = FALSE and the imported file is not json format).
#' @keywords import, csv, xlsx, xls, json
#' @seealso ``` ```
#' @export
#' @examples
#' # e.g.import <- importer(path = "downloads/, multiple = FALSE, graphics = TRUE)
#' 

importer <-
  function(path, 
           multiple, 
           graphics,
           column.format = list(c(column, format))) {
    if (missing(path)) {
      path = getwd()
    }
    if (missing(multiple)) {
      multiple = FALSE
    }
    if (missing(graphics)) {
      graphics = TRUE
    }
    if (missing(graphics)) {
      graphics = TRUE
    }
    
    files <-
      sort(list.files(
        path = path,
        pattern = NULL,
        full.names = FALSE,
        recursive = FALSE
      ))
    
    if (multiple == TRUE) {
      select.message <- "\nSelect one or more files to import.\nAll files should be similarly formatted."
    } else {
      select.message <- "\nSelect a file to import.\n"
    }
    
    
      file.choice <- utils::select.list(
        c(files),
        multiple = multiple,
        graphics = graphics,
        title = cat(paste0(select.message))
      )

      if (length(file.choice) != 0) {
        file.ext <- tools::file_ext(file.choice)
        import.file.name <-
          stringr::str_replace(file.choice, paste0(".", file.ext), "")
        
        
        file.list <- list()
        
        if (endsWith(path, "/") == FALSE) {
          path <- paste0(path, "/")
        }
        if (startsWith(path, "/") == TRUE) {
          path <- gsub("^(/*)*", "", path)
        }
        
        for (a in 1:length(file.ext)) {
          if (file.ext[a] %in% c("xlsx", "xls")) {
            import.file <-
              readxl::read_excel(paste0(path, file.choice[a]))
            
          } else if (file.ext[a] %in% c("csv")) {
            import.file <-
              utils::read.csv(paste0(path, file.choice[a]))
            
          } else if (file.ext[a] %in% c("json")) {
            if (fs::dir_exists(path) == FALSE) {
              full.path <- paste0(getwd(), "/", path)
            } else{
              full.path <- path
            }
            
            import.file <-
              rjson::fromJSON(file = paste0(full.path, file.choice[a]))
            
          }
          
          # Fix date/time columns
          if (file.ext[a] %in% c("csv", "xls", "xlsx")) {
            for (x in 1:length(column.format)) {
              column <- column.format[[x]][["column"]]
              format <- column.format[[x]][["format"]]
              
              if (column %in% colnames(import.file)) {
                import.file <- import.file %>%
                  dplyr::mutate({{column}} := format(.data[[column]], format = format))
              }
            }
          }
          
          
          file.list[[a]] <- import.file
          
        }
        
        if (multiple == TRUE) {
          names(file.list) <- import.file.name
          return(file.list)
        } else {
          return(import.file)
        }
      }
  }
