#' Dissimilarity Statistics
#' 
#' Uses the distance function to calculate dissimilarity statistics by grouping 
#' variables.
#' 
#' @param text.var A text variable or word frequency matrix object.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param method Distance methods (see \code{\link[stats]{dist}} function).  
#' If \code{"prop"} (the default) the result is 1 - \code{"binary"}.
#' @param diag logical.  If \code{TRUE} returns the diagonals of the matrix.  If 
#' \code{method = "prop"} diagonals will not be returned.
#' @param upper logical.  If \code{TRUE} returns the upper triangle of the 
#' matrix.
#' @param p The power of the Minkowski distance.
#' @param \ldots Other arguments passed to \code{\link[qdap]{wfm}}.
#' @rdname Dissimilarity
#' @return Returns a matrix of dissimilarity values (the agreement between text).
#' @seealso \code{\link[stats]{dist}}
#' @keywords dissimilarity
#' @export
#' @examples 
#' \dontrun{
#' with(DATA, Dissimilarity(state, list(sex, adult)))
#' with(DATA, Dissimilarity(state, person, diag = TRUE))
#' 
#' ## Clustering: Dendrogram
#' (x <- with(pres_debates2012, Dissimilarity(dialogue, list(person, time))))
#' fit <- hclust(x)
#' plot(fit)
#' ## draw dendrogram with red borders around the 3 clusters 
#' rect.hclust(fit, k=3, border=c("red", "purple", "seagreen"))
#' 
#' ## Clustering: Dendrogram with p.values
#' library(pvclust)
#' wfm.mod <- with(pres_debates2012, wfm(dialogue, list(person, time)))
#' fit <- suppressMessages(pvclust(wfm.mod, method.hclust="ward",
#'     method.dist="euclidean"))
#' plot(fit) 
#' pvrect(fit, alpha=.95)
#' 
#' ## Multidimentional Scaling
#' ## Based on blog post from Bodong Chen
#' ## http://bodongchen.com/blog/?p=301
#' 
#' ## Fit it: 2-D
#' (diss <- with(pres_debates2012, Dissimilarity(dialogue, list(person, time), 
#'     method = "euclidean")))
#' fit <- cmdscale(diss, eig = TRUE, k = 2)
#' 
#' ## Plot it 2-D
#' points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
#' ggplot(points, aes(x = x, y = y)) + 
#'     geom_point(data = points, aes(x = x, y = y, color = rownames(points))) + 
#'     geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(points)))
#'     
#' ## Fit it: 3-D
#' library(scatterplot3d)
#' fit <- cmdscale(diss, eig = TRUE, k = 3)
#' 
#' points <- data.frame(colSplit(names(fit$points[, 1])))
#' library(qdapTools)
#' points$colors <- points$X1 %l% data.frame(levels(points$X1), 
#'     qcv(yellow, yellow, blue, yellow, red, yellow))
#' points$shape <- points$X2 %l% data.frame(levels(points$X2), c(15, 17, 19))
#' 
#' ## Plot it: 3-D
#' scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], 
#'     color = points$colors, pch = points$shape, 
#'     main = "Semantic Space Scaled to 3D", xlab = "x", ylab = "y", 
#'     zlab = "z", type = "h")
#' 
#' legend("bottomright", title="Person",
#'    qcv(Obama, Romney, Other), fill=qcv(blue, red, yellow))
#' legend("topleft",  paste("Time", 1:3), pch=c(15, 17, 19))
#' 
#' ## Compare to Cosine Similarity
#' cos_sim <- function(x, y) x %*% y / sqrt(x%*%x * y%*%y)
#' mat <- matrix(rbinom(500, 0:1, .45), ncol=10)
#' v_outer(mat, cos_sim)
#' 
#' v_outer(with(DATA, wfm(state, person)), cos_sim)
#' with(DATA, Dissimilarity(state, person))
#' }
Dissimilarity <- 
function(text.var, grouping.var= NULL, method = "prop", diag = FALSE, 
    upper = FALSE, p = 2, ...){   
    if(!methods::is(text.var, "true.matrix")){ 
        wfm.object <- wfm(text.var = text.var, grouping.var = grouping.var, ...)
    } else {
        wfm.object <- text.var
    }
    wfm.object <- t(wfm.object)
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
    class(x) <- c("Dissimilarity", class(x))    
    x
}

#' Prints a Dissimilarity object
#' 
#' Prints a Dissimilarity object.
#' 
#' @param x The Dissimilarity object
#' @param digits Number of decimal places to print. 
#' @param \ldots ignored
#' @method print Dissimilarity
#' @export
print.Dissimilarity <-
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



