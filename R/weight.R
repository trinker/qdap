#' Weight a qdap Object
#' 
#' Weight a word_proximity object.
#' 
#' @param x A qdap object with a weight method.
#' @param type A weighting type of: c(\code{"scale_log"}, \code{"scale"}, 
#' \code{"rev_scale"}, \code{"rev_scale_log"}, \code{"log"}, \code{"sqrt"}, 
#' \code{"scale_sqrt"}, \code{"rev_sqrt"}, \code{"rev_scale_sqrt"}).  The 
#' weight type section name (i.e. \code{A_B_C} where \code{A}, \code{B}, and
#' \code{C} are sections) determines what action will occur.  \code{log} will 
#' use \code{\link[base]{log}}, \code{sqrt} will use \code{\link[base]{sqrt}},
#' \code{scale} will standardize the values.  \code{rev} will multiply by -1 to 
#' give the inverse sign.  This enables a comparison similar to correlations 
#' rather than distance.
#' @param \dots ignored.
#' @note A constant of .000000000001 is added to each element when log is used 
#' to deal with the problem of \code{log(0)}.
#' @return Returns a weighted list of matrices.
#' @export
weight <- 
function(x, type = "scale", ...) {
    UseMethod("weight")
}