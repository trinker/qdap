#' ID By Row Number or Sequence Along
#' 
#' Generate a sequence of integers the 
#' \code{\link[base]{length}}/\code{\link[base]{ncol}} of an object.
#' 
#' @param x A dataframe, matrix, vector, or list object.
#' @param prefix logical.  If \code{TRUE} an "X." is place before each id.
#' @param pad logical.  If \code{TRUE} the beginning number will be padded with 
#' zeros.
#' @param \ldots Other arguments passed to \code{link[reports]{pad}}.
#' @return Returns a vector of sequential integers.
#' @keywords id
#' @export
#' @importFrom reports pad
#' @examples
#' id(list(1, 4, 6))
#' id(matrix(1:10, ncol=1))
#' id(mtcars)
#' id(mtcars, TRUE)
#' id("w")
#' question_type(DATA.SPLIT$state, id(DATA.SPLIT, TRUE))
id <- function(x, prefix = FALSE, pad = TRUE, ...) {
  
    test1 <- dim(x)[1] > 1
    if (is.data.frame(x) | (!identical(logical(0), test1) && test1)) {
        ids <- seq_len(nrow(x))
    } else {
        ids <- seq_along(x)
    }
    if (pad) {
        ids <- pad(ids, ...)
    }
    if (prefix) {
        ids <- paste("X", ids, sep=".")
    } 
    ids
}

