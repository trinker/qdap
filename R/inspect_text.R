#' Inspect Text Vectors
#' 
#' \code{inspect_text} - Inspect a text vector with adjustable string wrapping; 
#' created a pretty printed named list.
#'
#' @param text.var The text variable or a \code{\link[qdap]{wfm}} object.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param \ldots ignored.
#' @return Returns a named list (prints pretty).
#' @keywords print
#' @export
#' @rdname inspect_text 
#' @examples
#' \dontrun{
#' with(raj, inspect_text(dialogue))
#' with(raj, inspect_text(dialogue, person))
#' with(raj, inspect_text(dialogue, list(paste("Act", act), person)))
#'
#' ## With a tm Corpus object
#' library(tm)
#' data(crude)
#' inspect_text(crude)
#' }
inspect_text <- function(text.var, grouping.var = NULL, ...) {   
    UseMethod("inspect_text")
} 


#' \code{inspect_text.default} - Default method for \code{inspect_text} used to 
#' convert to a vector to a pretty printed named list.
#' @rdname inspect_text
#' @export
#' @method inspect_text default 
inspect_text.default <- function(text.var, grouping.var = NULL, ...){

    if(is.null(grouping.var)){
        grouping <- 1:length(text.var)
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 

    o <- stats::setNames(as.list(text.var), grouping)
    class(o) <- c("inspect_text", "list")
    o
 
}

#' \code{inspect_text.Corpus} - Corpus method for \code{inspect_text} used to 
#' convert to a \code{\link[tm]{Corpus}}.
#' @rdname inspect_text
#' @export
#' @method inspect_text Corpus
inspect_text.Corpus <- function(text.var, ...){
    y <- as.data.frame(text.var)
    inspect_text(y[["text"]], y[["docs"]])
}


#' Prints an inspect_text Object
#' 
#' Prints an inspect_text object.
#' 
#' @param x The inspect_text object.
#' @param file A connection, or a character string naming the file to print to. If \code{""}
#' (the default), prints to the standard output connection, the console unless redirected 
#' by \code{\link[base]{sink}}.
#' @param \ldots Other arguments passed to \code{\link[base]{strwrap}}.
#' @method print inspect_text
#' @export
print.inspect_text <- function(x,  file = "", ...){

    out <- invisible(Map(function(x, y) {
        paste0(
            sprintf("<<%s>>\n", y),
            paste(strwrap(x, ...), collapse="\n")
        )
    }, x, names(x)))
    cat(paste(out, collapse="\n\n"), "\n",  file =  file)

}
