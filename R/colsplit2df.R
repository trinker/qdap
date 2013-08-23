#' Wrapper for colSplit that Returns Dataframe(s)
#' 
#' \code{colsplit2df} - Wrapper for \code{\link[qdap]{colSplit}} that returns a 
#' dataframe.
#' 
#' @param dataframe A dataframe with a column that has been pasted together.
#' @param splitcol The name of the column that has been pasted together.
#' @param new.names A character vector of new names to assign to the columns.  
#' Default attempts to extract the original names before the paste.
#' @param sep The character that used in \code{paste2} to paste the columns.
#' @param keep.orig logical.  If \code{TRUE} the original pasted column will be 
#' retained as well.
#' @return \code{colsplit2df} - returns a dataframe with the \code{paste2} 
#' column split into new columns.
#' @seealso \code{\link[qdap]{colSplit}}, 
#' \code{\link{paste2}}
#' @rdname colsplit2df
#' @keywords column-split
#' @export
#' @examples 
#' \dontrun{
#' CO2$`Plant&Type&Treatment` <- paste2(CO2[, 1:3])
#' CO2 <- CO2[, -c(1:3)]
#' head(CO2)
#' head(colsplit2df(CO2, 3))
#' head(colsplit2df(CO2, 3, qcv(A, B, C)))
#' head(colsplit2df(CO2, 3, qcv(A, B, C), keep.orig=TRUE))
#' head(colsplit2df(CO2, "Plant&Type&Treatment"))
#' CO2 <- datasets::CO2
#' 
#' (x <- question_type(DATA.SPLIT$state, list(DATA.SPLIT$sex, DATA.SPLIT$adult)))
#' ltruncdf(x)
#' z <- lcolsplit2df(x)
#' ltruncdf(z)
#' }
colsplit2df <- 
function(dataframe, splitcol = 1, new.names = NULL, sep=".", 
         keep.orig=FALSE){
    WD <- options()$width
    options(width=10000)
    on.exit(options(width=WD))
    classRdf <- c("diversity")
    if (!is.data.frame(dataframe)){
        warning("dataframe object is not of the class data.frame")
    }
    if (is.numeric(dataframe[, splitcol])) {
        stop("splitcol can not be numeric")
    }
    X <- data.frame(do.call(rbind, strsplit(as.vector(
        dataframe[, splitcol]), split = sep, fixed=TRUE)))
    z <- if (!is.numeric(splitcol)) {
        match(splitcol, names(dataframe)) 
    } else {
        splitcol
    }
    if (!is.null(new.names)) {
        colnames(X) <- new.names
    }
    if (z!=1 & ncol(dataframe) > z) {
        w <- cbind(dataframe[, 1:(z-1), drop=FALSE], X, 
            dataframe[, (z + 1):ncol(dataframe), drop=FALSE])
    } else {
        if (z!=1 & ncol(dataframe) == z) {
            w <- cbind(dataframe[, 1:(z-1), drop=FALSE], X)
        } else {
            if (z==1 & ncol(dataframe) > z) {
                w <- cbind(X, dataframe[, (z + 1):ncol(dataframe), drop=FALSE])
            } else {
                w <- X
            }
        }
    }
    if (is.null(new.names) & "&" %in% unlist(strsplit(names(dataframe[, 
        splitcol, drop=FALSE]), split=NULL))) {
        nams <- unlist(strsplit(names(dataframe[, 
            splitcol, drop=FALSE]), split="&"))
        colnames(w)[z:(z + length(nams) - 1)] <- nams
    }
    if(keep.orig) {
        w <- cbind(dataframe[, splitcol, drop=FALSE], w)
    }
    class(w) <- c("colsplit2df", "data.frame")
    w
}
#' Wrapper for qdap lists that Returns Dataframes
#' 
#' \code{lcolsplit2df} - Wrapper for \code{colsplit2df} designed for qdap lists 
#' that returns a list dataframes.
#' @param qdap.list A qdap list object that contains dataframes with a leading 
#' \code{\link[qdap]{paste2}} column.
#' @note \code{\link[qdap]{lcolsplit2df}} is a convenience function that is less 
#' flexible than \code{\link[qdap]{colsplit2df}} but operates on multiple 
#' dataframes at once.
#' @section Warning: This will strip the class of the qdap object.
#' @return \code{lcolsplit2df} - returns a list of dataframes with the 
#' \code{\link[qdap]{paste2}} column split into new columns.
#' @rdname colsplit2df
#' @export
lcolsplit2df <- 
function(qdap.list, keep.orig=FALSE){
    apps <- sapply(qdap.list, is.data.frame)
    nms <- unlist(strsplit(colnames(qdap.list[[1]])[1], "\\&"))
    w <- lapply(qdap.list[apps], colsplit2df)
    return(unlist(list(w, qdap.list[!apps]), recursive = FALSE))
}

#' Prints a colsplit2df Object.
#' 
#' Prints a colsplit2df object.
#' 
#' @param x The colsplit2df object 
#' @param \ldots ignored
#' @method print colsplit2df
#' @S3method print colsplit2df
print.colsplit2df <-
function(x,  ...) {
    WD <- options()[["width"]]
    options(width=3000)
    class(x) <- "data.frame"
    print(x)
    options(width=WD)  
}
