#' Generic Network Method
#' 
#' Create a network plt for select qdap outputs.
#' 
#' @param x A select qdap object.
#' @param \ldots Arguments passed to Network method of other classes.
#' @export
#' @return Returns a network plot.
Network <-
function(x, ...){
    UseMethod("Network")
}