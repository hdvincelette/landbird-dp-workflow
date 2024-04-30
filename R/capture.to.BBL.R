#' Format capture data for import to Bander Portal
#'
#' Translates capture data of USGS-issued federal bands to the Bird Banding Laboratory (BBL) banding and recapture data entry templates.
#' @param x Data frame. Banding or recapture data.
#' @param type Character string. Type of capture data. Available options include "banding_aux_mark", "recapture_aux_mark"; see ‘Details’.
#' @return Returns a data frame corresponding to a BBL banding or recapture data submission template.
#' @keywords USFWS, BBL, banding, recapture
#' @seealso ``` ```
#' @export
#' @examples
#'
#' 


capture.to.BBL <-
  function(x,
           type = c("banding_aux_mark",
                    "recapture_aux_mark")) {
    
    `%>%` <- magrittr::`%>%`
    
    cap_data <- x
  
    
    
  }




