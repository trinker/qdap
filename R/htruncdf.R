#' Dataframe Viewing
#' 
#' \code{htruncdf} - Convenience function to view the head of a truncated 
#' dataframe.
#' 
#' @param dataframe A data.frame object.
#' @param n Number of rows to display.
#' @param width The width of the columns to be displayed.
#' @param end The last character to be displayed (width).
#' @param begin The first character to be displayed (width).
#' @param \ldots Other arguments passed to \code{\link[qdap]{htruncdf}} 
#' (\code{\link[qdap]{qview}}; \code{\link[qdap]{ltruncdf}}) or 
#' \code{\link[utils]{head}} (\code{\link[qdap]{htruncdf}}).
#' @rdname data_viewing
#' @return \code{htrundf} - returns n number of rows of a truncated dataframe.
#' @seealso \code{\link[utils]{head}}
#' @export
#' @examples
#' \dontrun{
#' truncdf(raj[1:10, ])
#' truncdf(raj[1:10, ], 40)
#' htruncdf(raj)
#' htruncdf(raj, 20)
#' htruncdf(raj, ,20)
#' ltruncdf(rajPOS, width = 4)
#' qview(raj)
#' qview(CO2)
#' lview(question_type(DATA.SPLIT$state, DATA.SPLIT$person))
#' lview(rajPOS)
#' lview(lm(mpg~hp, data = mtcars))
#' }
htruncdf <-
function(dataframe, n=10, width=10, ...) {
    o <- utils::head(truncdf(as.data.frame(dataframe), width), n = n, ...)
    class(o) <- c("trunc", class(o))
    o
}

#' Truncated Dataframe Viewing
#' 
#' \code{truncdf} - Convenience function to view a truncated dataframe. 
#' 
#' @return \code{trundf} - returns a truncated dataframe.
#' @rdname data_viewing
#' @export
truncdf <- 
function(dataframe, end=10, begin=1) {
    x <- as.data.frame(dataframe)
    DF <- data.frame(lapply(x, substr, begin, end), check.names=FALSE)
    names(DF) <- substring(names(DF), begin, end)
    class(DF) <- c("trunc", class(DF))
    DF
}

#' List of Dataframes Viewing
#' 
#' \code{ltruncdf} - Convenience function to view the head of a list of 
#' truncated dataframes.
#' 
#' @param dat.list A list of data.frame objects.
#' @return \code{ltruncdf} - returns a list of n number of rows of a truncated 
#' dataframes.
#' @rdname data_viewing
#' @export
ltruncdf <- function(dat.list, n = 6, width = 10, ...) {
    o <- lapply(dat.list, htruncdf, n = n, width = width, ...)
    class(o) <- c("trunc", class(o))
    o
}

#' Summary Dataframe Viewing
#' 
#' \code{qview} - Convenience function to view a summary and head of a dataframe.
#' 
#' @return \code{qview} - returns a dataframe head with summary statistics.
#' @rdname data_viewing
#' @export
qview <-
function(dataframe, ...){
    x <- as.character(substitute(dataframe))
    y <- paste(rep("=", 72), collapse="")   
    z <- paste("nrow = ",nrow(dataframe), "          ncol = ",
        ncol(dataframe), "           ", x, collapse="")
    o <- htruncdf(dataframe, ...)
    class(o) <- c("trunc", class(o))
    o
    message(paste(y, z, y, sep = "\n"))
    return(o)
}

#' Unclass qdap Object to View List of Dataframes
#' 
#' \code{lview} - Convenience function to view the list (list view) of qdap 
#' objects that have print methods that print a single dataframe.  
#' 
#' @param x A class qdap object that is a list which prints as a dataframe.
#' @param print logical.  If \code{TRUE} prints to the console.
#' @return \code{lview} - prints a list of the qdap object and invisibly returns 
#' the unclassed object.
#' @rdname data_viewing
#' @export
lview <- function(x, print = TRUE) {
    class(x) <- "list"
    if (print) {
        print(x)
    }
    invisible(x)
}


#' Prints a trunc object
#' 
#' Prints a trunc object
#' 
#' @param x The trunc object
#' @param \ldots ignored
#' @export
#' @method print trunc
print.trunc <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)

    if (any(class(x) %in% "data.frame")) {
        class(x) <- "data.frame"
    } else {
        class(x) <- "list"
    }
    print(x)
    options(width=WD)
}
