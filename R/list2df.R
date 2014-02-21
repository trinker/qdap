#' List/Matrix to Dataframe
#' 
#' \code{list2df} - Convert a named list of vectors to a dataframe.
#' 
#' @param list.object A named \code{\link[base]{list}} of vectors..
#' @param col1 Name for column 1 (the vector elements if converting a list or 
#' the rownames if converting a matrix).
#' @param col2 Name for column 2 (the names of the vectors).
#' @return \code{list2df} - Returns a dataframe with two columns.
#' @details generally an internal function used for reshaping data.
#' @keywords collapse list
#' @export
#' @rdname list2df
#' @examples
#' lst1 <- list(x=c("foo", "bar"), y=1:5)
#' list2df(lst1)
#' 
#' lst2 <- list(a=qcv(hello, everybody), b = mtcars[1:6, 1])
#' list2df(lst2, "col 1", "col 2")
#' 
#' matrix2df(mtcars)
#' matrix2df(cor(mtcars))
#' matrix2df(matrix(1:9, ncol=3))
list2df <- function(list.object, col1 = "X1", col2 = "X2") {

    ## Make sure the vectors have names; if not use numbers
    if (is.null(names(list.object))){
        names(list.object) <- seq_along(list.object)
    }

    dat <- data.frame(x = unlist(list.object, ,FALSE), 
        y = rep(names(list.object), sapply(list.object, length)), 
        stringsAsFactors = FALSE)
    colnames(dat) <- c(col1, col2)
    dat
}

#' List/Matrix to Dataframe
#' 
#' \code{matrix2df} - Convert a matrix to a dataframe and convert the rownames 
#' to the first column.
#' 
#' @param matrix.object A matrix object.
#' @rdname list2df
#' @return \code{matrix2df} - Returns a dataframe.
#' @export
matrix2df <- function(matrix.object, col1 = "var1") {

    if (is.null(rownames(matrix.object))) {
        rownames(matrix.object) <- 1:nrow(matrix.object)
    }
    dat <- data.frame(rownames(matrix.object), matrix.object, row.names=NULL)
    colnames(dat)[1] <- col1
    dat
}
