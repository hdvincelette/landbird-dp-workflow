#' Make a selection from a set of choices
#'
#' @param choices Character vector.
#' @param multiple Logical. Whether to allow multiple selections. Default is FALSE.
#' @param graphics Logical. Whether to use a graphical widget for file selection. Default is TRUE.
#' @param title Character string. Default is NULL.
#' @return Returns character string (multiple = FALSE) or vector (multiple = TRUE) of choice selection.
#' @keywords filter
#' @seealso ``` ```
#' @export
#' @examples
#' # 
#' 

selector <-
  function(choices,
           multiple = FALSE,
           graphics = TRUE,
           title = NULL) {
    if (missing(multiple)) {
      multiple = FALSE
    }
    if (missing(graphics)) {
      graphics = FALSE
    }
    if (missing(title)) {
      title = NULL
    }
    
    choice <- utils::select.list(
      choices = choices,
      multiple = multiple,
      graphics = graphics,
      title = title
    )
    
    
    return(choice)
    
  }
