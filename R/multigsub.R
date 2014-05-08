#' Multiple gsub
#' 
#' \code{multigsub} - A wrapper for \code{\link[base]{gsub}} that takes a vector 
#' of search terms and a vector or single value of replacements.
#' 
#' @param pattern Character string to be matched in the given character vector. 
#' @param replacement Character string equal in length to pattern or of length 
#' one which are  a replacement for matched pattern. 
#' @param text.var The text variable.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the 
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the 
#' replacements.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as is. 
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are 
#' removed.
#' @param order.pattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the 
#' \code{pattern} string is sorted by number of characters to prevent substrings 
#' replacing meta strings (e.g., \code{pattern = c("the", "then")} resorts to 
#' search for "then" first).
#' @param \dots Additional arguments passed to \code{\link[base]{gsub}}.
#' @rdname multigsub
#' @return \code{multigsub} - Returns a vector with the pattern replaced.
#' @seealso \code{\link[base]{gsub}}
#' @export
#' @examples
#' \dontrun{
#' multigsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub("[[:punct:]]", "PUNC", DATA$state, fixed = FALSE)
#' 
#' ## `sub_holder` Function
#' (fake_dat <- paste(emoticon[1:11,2], DATA$state))
#' (m <- sub_holder(emoticon[,2], fake_dat))
#' m$unhold(strip(m$output))
#' # With Stemming
#' m$unhold(stemmer(strip(m$output), capitalize = FALSE))
#' }
multigsub <-
function(pattern, replacement = NULL, text.var, leadspace = FALSE, 
    trailspace = FALSE, fixed = TRUE, trim = TRUE, order.pattern = fixed, ...){

    if (leadspace | trailspace) {
        replacement <- spaste(replacement, trailing = trailspace, 
            leading = leadspace)
    }

    ## replaces the larger n character words first
    if (fixed && order.pattern) {
        if (!is.null(replacement) && length(replacement) > 1) {
            replacement <- replacement[rev(order(nchar(pattern)))]
        }
        pattern <- pattern[rev(order(nchar(pattern)))]
    }

    key <- data.frame(pat=pattern, rep=replacement, 
        stringsAsFactors = FALSE)

    msubs <-function(K, x, trim, ...){
        sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, fixed = fixed, ...)
            }
        )
        if (trim) x <- gsub(" +", " ", x)
        return(x)
    }

    if (trim) {
        x <- Trim(msubs(K=key, x=text.var, trim = trim, ...))
    } else {    
        x <- msubs(K=key, x=text.var, trim = trim, ...)
    }

    return(x)
}


#' @rdname multigsub
#' @export
mgsub <- multigsub


#' Multiple gsub
#' 
#' \code{sub_holder} - This function holds the place for particular character 
#' values, allowing the user to manipulate the vector and then revert the place
#' holders back to the original values.
#' 
#' @return \code{sub_holder} - Returns a list with the following:
#' \item{output}{keyed place holder character vector} 
#' \item{unhold}{A function used to revert back to the original values}
#' @rdname multigsub
#' @note The \code{unhold} function for \code{sub_holder} will only work on keys
#' that have not been disturbed by subsequen alterations.  The key follows the 
#' pattern of `qdapplaceholder` followed by lower case letter keys followed by
#' `qdap`.
#' @export
sub_holder <- function(pattern, text.var, ...) {

    if (!is.character(pattern)) pattern <- as.character(pattern)
    x2 <- x <- length(pattern)
    counter <- 0
    while(x > 26) {
        x <- x/26
        counter <- counter + 1
    }
    if (x > 0) counter + 1

    keys <- paste2(expand.grid(lapply(1:counter, function(i) letters)), sep="")
    reps <- paste0("qdapplaceholder", keys, "qdap")

    output <- mgsub(pattern, reps, text.var, ...)


    FUN <- function(text.var, ...) {
        mgsub(reps, pattern, text.var)
    }

    out <- list(output = output, unhold = FUN)

    attributes(out) <- list(
        class = c("sub_holder", "list"), 
        names = names(out),
        pattern = pattern, 
        keys = keys, 
        len = x2
    )
    out

}


#' Prints a sub_holder object
#' 
#' Prints a sub_holder object
#' 
#' @param x The sub_holder object
#' @param \ldots ignored
#' @export
#' @method print sub_holder
print.sub_holder <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    print(x[["output"]])
    options(width=WD)
}

