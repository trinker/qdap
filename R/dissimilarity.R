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
#' @param diag logical.  If TRUE returns the diagonals of the matrix.  If 
#' \code{method = "prop"} diagonals will not be returned.
#' @param upper logical.  If TRUE returns the upper triangle of the matrix.
#' @param p The power of the Minkowski distance
#' @return Returns a matrix of dissimilarity values (the agreement between text).
#' @seealso \code{\link[stats]{dist}}
#' @keywords dissimilarity
#' @export
#' @examples 
#' \dontrun{
#' with(DATA, dissimilarity(state, list(sex, adult)))
#' with(DATA, dissimilarity(state, person, diag = TRUE))
#' }
dissimilarity <-
function(text.var, grouping.var= NULL, method = "prop", diag = FALSE, 
    upper = FALSE, p = 2){   
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
    if (meth.check) {
        diag <- FALSE
    }
    x <- stats::dist(wfm.object, method = method, diag = diag, upper = upper, 
        p = p)
    if (meth.check) {
      x <- 1 - x
    }   
    class(x) <- c("dissimilarity", "dist")    
    x
}

#' Prints a dissimilarity object
#' 
#' Prints a dissimilarity object.
#' 
#' @param x The dissimilarity object
#' @param digits Number of decimal places to print. 
#' @param \ldots ignored
#' @method print dissimilarity
#' @S3method print dissimilarity
print.dissimilarity <-
function(x, digits = 3, ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "dist"
    if (!is.null(digits)) {
        x <- round(x, digits = digits)
    }
    print(x)
    options(width=WD)  
}
