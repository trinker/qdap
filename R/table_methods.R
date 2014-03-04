#' Plots a table_score Object
#' 
#' Plots a table_score object.
#' 
#' @param x The table_score object.
#' @param high The color to be used for higher values.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap. 
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}}.
#' @export
plot.table_score <- function(x, values = TRUE, high = "red", ...){ 

    y <- x
    y[, 2:ncol(y)] <- lapply(y[, 2:ncol(y)], function(z) {
        as.numeric(bracketX(z))
    })

    y[, -c(1:2)] <- y[, -c(1:2)]/y[, 2]
    y[, -c(1:2)] <- lapply(y[, -c(1:2)], function(z) {
        z[is.na(z)] <- 0; z
    })

    if (!values) {
        mat2 <- NULL
    } else {
        mat2 <- x
    }

    qheat(y, values = values, high=high, mat2 = mat2, ...)
}


#' Plots a table_proportion Object
#' 
#' Plots a table_proportion object.
#' 
#' @param x The table_proportion object.
#' @param high The color to be used for higher values.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap. 
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}}.
#' @export
plot.table_proportion <- function(x, values = TRUE, high = "red", ...){ 

    qheat(x, values = values, high=high, ...)

}



#' Plots a table_count Object
#' 
#' Plots a table_count object.
#' 
#' @param x The table_count object.
#' @param high The color to be used for higher values.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap. 
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}}.
#' @export
plot.table_count <- function(x, values = TRUE, high = "red", ...){ 

    qheat(x, values = values, high=high, ...)

}
