#' Generic Preprocessed Method
#' 
#' Access the preprocessed dataframes/lists from select qdap outputs.
#' 
#' @param x A qdap object (list) with a dataframe/list of preprocessed data 
#' (e.g., \code{\link[qdap]{pos_by}}).
#' @param \ldots Arguments passed to preprocessed method of other classes.
#' @export
#' @seealso \code{\link[qdap]{counts}}
#' @seealso \code{\link[qdap]{proportions}}
#' @seealso \code{\link[qdap]{scores}}
#' @return Returns a data.frame or list of preprocessed data.
preprocessed <-
function(x, ...){
    UseMethod("preprocessed")
}
