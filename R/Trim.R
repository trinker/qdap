#' Remove Leading and Trailing White Space
#' 
#' Remove leading/trailing white space.
#' 
#' @param x The text variable.  
#' @return Returns a vector with the leading/trailing white spaces removed.
#' @examples
#' \dontrun{
#' (x <- c("  talkstats.com ", "   really? ", " yeah"))
#' Trim(x)
#' }
Trim <-
function (x) gsub("^\\s+|\\s+$", "", x)
