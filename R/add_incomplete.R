#' Detect Incomplete Sentences; Add | Endmark
#' 
#' Automatically detect missing endmarks and replace with the \code{|} endmark 
#' symbol to indicate an incomplete sentence.
#' 
#' @param text.var The text variable.  
#' @param endmarks A reguar expression to check for endmarks.
#' @param silent logical.  If \code{TRUE} messages are not printed out.
#' @return Returns a vector with missing endmarks replaced with \code{|}.
#' @keywords endmark
#' @export
#' @examples 
#' add_incomplete(
#'     c(
#'         "This in a", 
#'         "I am funny!", 
#'         "An ending of sorts%", 
#'         "What do you want?"
#'     )
#' )
add_incomplete <- function(text.var, endmarks = "[.?|!]+$", silent = FALSE){

    text.var <- Trim(text.var)
    
    locs <- which(!grepl(endmarks, text.var))
    text.var[locs] <- paste0(text.var[locs], "|")
   
    if (!silent){
        print(text.var)
        on.exit({message(sprintf(
            "\n%s\nThe following elements were missing endmarks (`|` added):\n%s\n", 
            paste(rep("=", 100), collapse=""), paste(locs, collapse = ", ")
        ));
        print(text.var[locs])})
    }
    
    invisible(text.var)
}

