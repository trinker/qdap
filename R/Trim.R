#' Remove Leading/Trailing White Space
#' 
#' Remove leading/trailing white space.
#' 
#' @param x The text variable.  
#' @return Returns a vector with the leading/trailing white spaces removed.
#' @export
#' @examples
#' \dontrun{
#' (x <- c("  talkstats.com ", "   really? ", " yeah"))
#' Trim(x)
#' }
Trim <-
function (x) gsub("^\\s+|\\s+$", "", x)
