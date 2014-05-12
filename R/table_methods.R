#' Plots a table_score Object
#' 
#' Plots a table_score object.
#' 
#' @param x The table_score object.
#' @param high The color to be used for higher values.
#' @param values logical.  If \code{TRUE} the cell values will be included on 
#' the heatmap. 
#' @param \ldots Other arguments passed to \code{\link[qdap]{qheat}}.
#' @method plot table_score 
#' @export
plot.table_score <- function(x, values = TRUE, high = "red", ...){ 

    y <- x
    y[, 2:ncol(y)] <- lapply(y[, 2:ncol(y)], function(z) {
        as.numeric(bracketX(z))
    })

    if (attributes(x)[["type"]] %in% c("character_table_scores")) {
        y[, -c(1)] <- y[, -c(1)]/rowSums(y[, -c(1)])
        y[, -c(1)] <- lapply(y[, -c(1)], function(z) {
            z[is.na(z)] <- 0; z
        })
    } else {
        y[, -c(1:2)] <- y[, -c(1:2)]/y[, 2]
        y[, -c(1:2)] <- lapply(y[, -c(1:2)], function(z) {
            z[is.na(z)] <- 0; z
        })
    }


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
#' @method plot table_proportion
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
#' @method plot table_count
#' @export
plot.table_count <- function(x, values = TRUE, high = "red", ...){ 

    qheat(x, values = values, high=high, digits = 0, ...)

}

#' Prints a table_score object
#' 
#' Prints a table_score object
#' 
#' @param x The table_score object
#' @param \ldots ignored
#' @export
#' @method print table_score
print.table_score <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)
}

#' Prints a table_count object
#' 
#' Prints a table_count object
#' 
#' @param x The table_count object
#' @param \ldots ignored
#' @export
#' @method print table_count
print.table_count <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)
}

#' Prints a table_proportion object
#' 
#' Prints a table_proportion object
#' 
#' @param x The table_proportion object
#' @param \ldots ignored
#' @export
#' @method print table_proportion
print.table_proportion <-
function(x, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)
}


