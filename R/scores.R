#' Generic Scores Method
#' 
#' Access the scores dataframes from select qdap outputs.
#' 
#' @param x A qdap object (list) with a dataframe of scores (e.g., 
#' \code{\link[qdap]{fry}}, \code{\link[qdap]{formality}}).
#' @param \ldots Arguments passed to scores method of other classes.
#' @export
#' @seealso \code{\link[qdap]{counts}}
#' @seealso \code{\link[qdap]{proportions}}
#' @seealso \code{\link[qdap]{preprocessed}}
#' @return Returns a data.frame of scores.
scores <-
function(x, ...){
    UseMethod("scores")
}
