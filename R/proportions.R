#' Generic Proportions Method
#' 
#' Access the proportions dataframes from select qdap outputs.
#' 
#' @param x A qdap object (list) with a proportions dataframe (e.g., 
#' \code{\link[qdap]{termco}}).
#' @param \ldots Arguments passed to proportions method of other classes.
#' @export
#' @seealso \code{\link[qdap]{scores}},
#' \code{\link[qdap]{counts}},
#' \code{\link[qdap]{preprocessed}},
#' \code{\link[qdap]{visual}}
#' @return Returns a data.frame of proportions.
proportions <-
function(x, ...){
    UseMethod("proportions")
}
