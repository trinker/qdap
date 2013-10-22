#' ID By Row Number or Sequence Along
#' 
#' Generate a sequence of integers the 
#' \code{\link[base]{length}}/\code{\link[base]{ncol}} of an object.
#' 
#' @param x A dataframe, matrix, vector, or list object.
#' @param pad logical.  If \code{TRUE} the begining number will be padded with 
#' zeros.
#' @param prefix logical.  If \code{TRUE} an "X." is place before each id.
#' @return Returns a vector of sequential integers.
#' @keywords id
#' @export
#' @importFrom reports pad
#' @examples
#' ID(list(1, 4, 6))
#' ID(matrix(1:10, ncol=1))
#' ID(mtcars)
#' ID(mtcars, TRUE)
#' ID("w")
#' question_type(DATA.SPLIT$state, ID(DATA.SPLIT, TRUE))
ID <- function(x, pad = TRUE, prefix = FALSE) {
  
    test1 <- dim(x)[1] > 1
    if (is.data.frame(x) | (!identical(logical(0), test1) && test1)) {
        id <- seq_len(nrow(x))
    } else {
        id <- seq_along(x)
    }
    if (pad) {
        id <- pad(id)
    }
    if (prefix) {
        id <- paste("X", id, sep=".")
    } 
    id
}

