#' Ensure Space After Comma
#' 
#' Adds a space after a comma as strip and many other functions may consider a 
#' comma separated string as one word (i.e., \code{"one,two,three"} becomes 
#' \code{"onetwothree"}  rather than \code{"one two three"}).
#' 
#' @param text.var The text variable.
#' @return Returns a vector of strings with commas that have a space after them.
#' @keywords comma space
#' @export
#' @examples
#' \dontrun{
#' x <- c("the,  dog,went", "I,like,it", "where are you", NA, "why", ",", ",f")
#' comma_spacer(x)
#' }
comma_spacer <- function(text.var) {
    gsub("(,)([^ ])", "\\1 \\2", text.var)
}

