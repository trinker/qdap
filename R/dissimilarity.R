#' Dissimilarity Statistics
#' 
#' Uses the distance function to calculate dissimilarity statistics by grouping 
#' variables.
#' 
#' @param text.var A text variable or word frequency matrix object.
#' @param grouping.var The grouping variables.  Default NULL generates one word 
#' list for all text.  Also takes a single grouping variable or a list of 1 or 
#' more grouping variables.
#' @param method Distance methods (see \code{\link[stats]{dist}} function).  
#' If \code{"prop"} (the default; the result is 1 - \code{"binary"}.
#' @param diag logical.  If True returns the diagonals of the matrix
#' @param upper logical.  If True returns the upper triangle of the matrix
#' @param p The power of the Minkowski distance
#' @param digits integer indicating the number of decimal places (round) or 
#' significant digits (signif) to be used. Negative values are allowed
#' @return Returns a matrix of dissimilarity values (the agreement between text.
#' @seealso \code{\link[stats]{dist}}
#' @keywords correlation, dissimilarity
#' @export
#' @examples 
#' with(DATA, dissimilarity(state, list(sex, adult)))
#' with(DATA, dissimilarity(state, person, diag = TRUE))
dissimilarity <-
function(text.var, grouping.var= NULL, method = "prop", diag = FALSE, 
    upper = FALSE, p = 2, digits = 3){   
    if(is.null(comment(text.var))){ 
        wfm.object <- wfm(text.var = text.var, grouping.var = grouping.var)
    } else {
        wfm.object <- text.var
    }
    if (comment(wfm.object)!= "true.matrix") {
        warning("not a matrix from wfm function")
    }
    if (comment(wfm.object)!= "true.matrix"){
        wfm.object <- wfm.object[-c(nrow(wfm.object)), -c(1, 
            ncol(wfm.object))]
        wfm.object <- t(wfm.object)
    } else {
        wfm.object <- t(wfm.object)
    }
    meth.check <- FALSE
    if (method == "prop") {
        method <- "binary" 
        meth.check <- TRUE
    }
    #leave this stats::dist b/c other packages use dist
    x <- stats::dist(wfm.object, method = method, diag = diag, upper = upper, p = p)
    if (meth.check) {
      x <- 1 - x
    }    
    return(round(x, digits = digits))
}