#' Import Movebank study location and reference data into R
#'
#' Imports location and reference data from one or more Movebank studies managed by a specified Movebank account.
#' @param project Character string. Name of the project folder.
#' @param path Character string. Directory path where the cloned project will be located. Default is the working directory, getwd().
#' @param main Logical. Whether to return results from the main project folder (all subfolders except "incoming"). Default is TRUE.
#' @param incoming Logical. Whether to return results from the "incoming" project subfolder. Default is TRUE.
#' @return Returns a download of the project folder and all its contents.
#' @keywords movebank, import, tracking
#' @seealso ``` ```
#' @export
#' @examples
#' # e.g.import <- get.move.data(location = TRUE, reference = TRUE, remove_movebank_outliers <- FALSE, omit_derived_data <- FALSE)
#' 

# attribute & sensor selection menus


# Study selection
get.move.data <-
  function(location,
           reference,
           remove_movebank_outliers,
           omit_derived_data) {
    if (missing(location)) {
      location <- TRUE
    }
    if (missing(reference)) {
      reference <- TRUE
    }
    if (missing(remove_movebank_outliers)) {
      remove_movebank_outliers <- FALSE
    }
    if (missing(omit_derived_data)) {
      omit_derived_data <- FALSE
    }
    
    if (location == FALSE &
        reference == FALSE) {
      stop(
        "Operation canceled. To conduct a Movebank data import, one or both of the arguments 'location' and 'reference' must be TRUE."
      )
    }
    
    all.studies.df <-
      suppressWarnings(move2::movebank_download_study_info()) %>%
      dplyr::filter(i_am_owner == TRUE,
                    is.na(number_of_deployed_locations) == FALSE)
    
    
    all.studies.vector <- all.studies.df %>%
      dplyr::select(name) %>%
      unlist() %>%
      as.vector %>%
      sort()
    
    if (length(all.studies.vector) == 0) {
      stop("Operation canceled. No studies found")
    }
    
    study.choice <- utils::select.list(
      c(all.studies.vector),
      multiple = TRUE,
      graphics = TRUE,
      title = cat(paste0("\nSelect one or more studies to import."))
    )
    
    if (length(study.choice) == 0) {
      stop("Operation canceled. No studies selected.")
    }
    
    # Time range selection (single study)
    
    start.time <-
      as.POSIXct(strptime("0001-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
    
    end.time <-
      as.POSIXct(strptime("9999-12-31 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'))
    
    
    study.choice.df <- data.frame(
      name = study.choice,
      id = as.numeric(NA),
      timestamp_first_deployed_location = as.POSIXct(NA),
      timestamp_last_deployed_location = as.POSIXct(NA)
    )
    
    for (c in 1:nrow(all.studies.df)) {
      for (d in 1:length(study.choice)) {
        if (study.choice[d] == all.studies.df$name[c]) {
          study.choice.df$timestamp_first_deployed_location[d] <-
            all.studies.df$timestamp_first_deployed_location[c]
          study.choice.df$timestamp_last_deployed_location[d] <-
            all.studies.df$timestamp_last_deployed_location[c]
          study.choice.df$id[d] <-
            as.numeric(all.studies.df$id[c])
        }
      }
    }
    
    
    time.choice <-
      utils::menu(c("Filter", "All data"),
                  title =
                    cat(paste0(
                      "\nFilter by date or import all available data?\n"
                    )))
    
    ## Display study time range
    
    removed.studies <- c()
    
    if (time.choice == 1) {
      time.message <- c()
      
      for (a in 1:nrow(study.choice.df)) {
        min.timestamp <- format(as.POSIXct(
          lubridate::parse_date_time(
            min(study.choice.df$timestamp_first_deployed_location[a]),
            lubridate::guess_formats(
              min(study.choice.df$timestamp_first_deployed_location[a]),
              c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
            )
          )
        ),
        "%Y-%m-%d %H:%M:%S")
        
        max.timestamp <- format(as.POSIXct(
          lubridate::parse_date_time(
            max(study.choice.df$timestamp_last_deployed_location[a]),
            lubridate::guess_formats(
              max(study.choice.df$timestamp_last_deployed_location[a]),
              c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
            )
          )
        ),
        "%Y-%m-%d %H:%M:%S")
        
        
        time.message[a] <-
          paste0(study.choice.df$name[a],
                 ": ",
                 min.timestamp,
                 " - ",
                 max.timestamp)
        
      }
      
      message(cat(
        paste0(
          "\nThe selected study(ies) has/have location data for the following time range(s)."
        ),
        paste0("\n ", time.message)
      ))
      
      ## Get time range input
      start.time <- NA
      
      while (is.na(start.time)) {
        message(
          cat(
            "\nEnter a start date and time for the Movebank attribute 'timestamp-start' (e.g., 2000-01-31 00:00:00):"
          )
        )
        start.time.entry <- as.character(readline(prompt =))
        
        
        if (is.na(as.POSIXct(
          strptime(start.time.entry,
                   format = "%Y-%m-%d %H:%M:%S",
                   tz = 'UTC')
        ))) {
          start.time.entry <- paste0(start.time.entry, " 00:00:00")
          
        }
        
        start.time <-
          as.POSIXct(strptime(start.time.entry,
                              format = "%Y-%m-%d %H:%M:%S",
                              tz = 'UTC'))
        
        if (is.na(start.time)) {
          message(cat("\nError: Timestamp incorrectly formatted."))
          
        }
      }
      
      
      end.time <- NA
      
      while (is.na(end.time)) {
        message(
          cat(
            "\nEnter an end date and time for the Movebank attribute 'timestamp-end' (e.g., 2050-12-31 23:59:59):"
          )
        )
        
        end.time.entry <- as.character(readline(prompt =))
        
        
        if (is.na(as.POSIXct(
          strptime(end.time.entry,
                   format = "%Y-%m-%d %H:%M:%S",
                   tz = 'UTC')
        ))) {
          end.time.entry <- paste0(end.time.entry, " 00:00:00")
          
        }
        
        end.time <-
          as.POSIXct(strptime(end.time.entry,
                              format = "%Y-%m-%d %H:%M:%S",
                              tz = 'UTC'))
        
        if (is.na(end.time)) {
          message(cat("\nError: Timestamp incorrectly formatted."))
          
        }
      }
      
      
      ## Check if study(ies) out of time range
      for (c in 1:nrow(study.choice.df)) {
        if (!study.choice.df$timestamp_first_deployed_location[c] <= end.time |
            !study.choice.df$timestamp_last_deployed_location[c] >= start.time) {
          study.choice <-
            study.choice[!study.choice %in% study.choice.df$name[c]]
          
        }
      }
      if (length(study.choice) == 0) {
        stop(cat(
          paste0(
            "Operation canceled. Time selection out of range for one or more selected studies."
          )
        ))
      } else {
        removed.studies <-
          study.choice.df$name[!study.choice.df$name %in% study.choice]
        
        study.choice.df <- study.choice.df %>%
          dplyr::filter(!name %in% removed.studies)
      }
      
    }
    
    
    # Import options
    
    import.choice <- NA
    
    if (length(study.choice) == 0) {
      stop("Operation canceled. No studies selected.")
    } else if (length(study.choice) > 1) {
      import.choice <-
        utils::menu(c("List of dataframes", "Single dataframe"),
                    title =
                      cat(
                        paste0("\nHow do you want to import location and/or reference data?")
                      ))
    } else {
      import.choice<- 2
    }
    
    message(cat(paste0("\nRetrieving data for")))
    
    # Import data
    
    import.list <- list()
    
    for (a in 1:nrow(study.choice.df)) {
      study.list <- list()
      
      study.name <- study.choice.df$name[a]
      
      study.id <-
        move2::movebank_get_study_id(study_id = study.choice.df$id[a])
      
      message(cat(paste0(" '", study.name, "'...")))
      
      
      ## Location data..
      
      if (location == TRUE) {
        sensor.id <-
          move2::movebank_retrieve(
            entity_type = 'tag_type',
            timestamp_start = start.time,
            timestamp_end  = end.time,
            study_id = study.id
          ) %>%
          dplyr::filter(is_location_sensor == TRUE) %>%
          dplyr::pull("external_id")
        
        
        # attributes.vars <- c()
        #
        # for (b in 1:length(sensor.id)) {
        #   attributes.add <-
        #     move2::movebank_retrieve(
        #       entity_type = "study_attribute",
        #       study_id = study.id,
        #       timestamp_start = start.time,
        #       timestamp_end  = end.time,
        #       sensor_type_id = sensor.id[b]
        #     )$short_name
        #
        #   attributes.vars <- c(attributes.vars, attributes.add)
        # }
        #
        # attributes.vars <-
        #   unique(attributes.vars)
        
        suppressWarnings(
          import.loc <- move2::movebank_retrieve(
            "event",
            study_id = study.id,
            remove_movebank_outliers = FALSE,
            omit_derived_data = FALSE,
            sensor_type_id = sensor.id,
            attributes = "all"
          )
          %>%
            dplyr::filter(timestamp > start.time,
                          timestamp < end.time)
        )
        
        if (nrow(import.loc) == 0) {
          study.choice <-
            study.choice[!study.choice %in% study.choice.df$name[a]]
          
          next
        }
        
        study.list[[length(study.list) + 1]] <-
          import.loc
        
        names(study.list)[[length(study.list)]] <- "location-data"
        
      }
      
      ## Reference data..
      
      if (reference == TRUE) {
        import.ref <- move2::movebank_download_deployment(study.id)
        
        
        study.list[[length(study.list) + 1]] <-
          import.ref
        
        names(study.list)[[length(study.list)]] <- "reference-data"
        
      }
      
      
      import.list[[length(import.list) + 1]] <-
        study.list
      
    }
    
    removed.studies <- c(removed.studies,
                         study.choice.df$name[!study.choice.df$name %in% study.choice])
    
    if (length(removed.studies) != 0) {
      warning(
        paste0(
          "The following studies do not have location data for the provided time range and were excluded."
        ),
        paste0(" \n  ", removed.studies)
      )
    }
    
    
    names(import.list) <- study.choice
    
    
    # Return data
    
    ## Single study import
    
    if (length(study.choice == 1)) {
      if (length(import.list[[1]]) == 1) {
        sub.import.list <-
          sapply(import.list, "[", names(import.list[[1]]))
        
        import <-
          data.table::rbindlist(sub.import.list, fill = TRUE, idcol = "study_name")
        
        assign("output", import)
      } else {
        assign("output", import.list)
      }
    }
    
    ## Multiple study import
      if (import.choice == 2) {
        rbind.import.list <- list()
        
        for (b in 1:length(names(import.list[[1]]))) {
          sub.import.list <-
            sapply(import.list, "[", names(import.list[[1]])[b])
          
          names(sub.import.list) <- study.choice
          
          sub.import.list <-
            data.table::rbindlist(sub.import.list, fill = TRUE, idcol = "study_name")
          
          rbind.import.list[[length(rbind.import.list) + 1]] <-
            sub.import.list
          
          names(rbind.import.list)[[length(rbind.import.list)]] <-
            names(import.list[[1]])[b]
        }
        
        if (length(rbind.import.list) == 1) {
          rbind.import <-
            data.table::rbindlist(rbind.import.list, fill = TRUE)
          
          assign("output", rbind.import)
        } else {
          assign("output", rbind.import.list)
        }
        
      } else if (import.choice == 1) {
        assign("output", import.list)
    }
    
    return(output)
  }
