#' Generic Animate Method
#' 
#' Animate select qdap objects.
#' 
#' @param x An animatable qdap object (e.g., \code{\link[qdap]{discourse_map}}).
#' @param \ldots Arguments passed to Animate method of other classes.
#' @export
#' @seealso \code{\link[qdap]{scores}},
#' \code{\link[qdap]{counts}},
#' \code{\link[qdap]{preprocessed}},
#' \code{\link[qdap]{proportions}}
#' @return Returns a plot object.
Animate <-
function(x, ...){
    UseMethod("Animate")
}
