#' Generic Counts Method
#' 
#' Access the count dataframes from select qdap outputs.
#' 
#' @param x A qdap object (list) with a count dataframe (e.g., 
#' \code{\link[qdap]{fry}}).
#' @param \ldots Arguments passed to counts method of other classes.
#' @export
#' @seealso \code{\link[qdap]{scores}}
#' @return Returns a data.frame of counts.
counts <-
function(x, ...){
    UseMethod("counts")
}