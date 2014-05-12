#' Generic visual Method
#' 
#' Access the visual-graph-plot object from select qdap outputs.
#' 
#' @param x A qdap object (list) with a visual-graph-plot object (e.g., 
#' \code{\link[qdap]{discourse_map}}).
#' @param \ldots Arguments passed to visual method of other classes.
#' @export
#' @seealso \code{\link[qdap]{scores}},
#' \code{\link[qdap]{counts}},
#' \code{\link[qdap]{preprocessed}},
#' \code{\link[qdap]{proportions}}
#' @return Returns a plot object.
visual <-
function(x, ...){
    UseMethod("visual")
}
