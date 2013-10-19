#' List to Dataframe
#' 
#' Convert a named list of vectors to a dataframe.
#' 
#' @param list.object A named \code{\link[base]{list}} of vectors..
#' @param col1 Name for column 1 (the vector elements).
#' @param col2 Name for column 2 (the names of the vectors).
#' @return Returns a dataframe with two columns.
#' @details generally an internal function used for reshaping data.
#' @keywords collapse list
#' @export
#' @examples
#' lst1 <- list(x=c("foo", "bar"), y=1:5)
#' list2df(lst1)
#' 
#' lst2 <- list(a=qcv(hello, everybody), b = mtcars[1:6, 1])
#' list2df(lst2, "col 1", "col 2")
list2df <- function(list.object, col1 = "X1", col2 = "X2") {
    dat <- data.frame(x = unlist(list.object, ,FALSE), 
        y = rep(names(list.object), sapply(list.object, length)), 
        stringsAsFactors = FALSE)
    colnames(dat) <- c(col1, col2)
    dat
}
