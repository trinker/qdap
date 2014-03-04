#' Generic Raw Method
#' 
#' Access the raw dataframes from select qdap outputs.
#' 
#' @param x A qdap object (list) with a raw dataframe (e.g., 
#' \code{\link[qdap]{pos_by}}).
#' @param \ldots Arguments passed to proportions method of other classes.
#' @export
#' @seealso \code{\link[qdap]{scores}}
#' @seealso \code{\link[qdap]{counts}}
#' @seealso \code{\link[qdap]{proportions}}
#' @return Returns a data.frame or list of raw processed data.
raw <-
function(x, ...){
    UseMethod("raw")
}