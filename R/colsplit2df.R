#' Wrapper for colSplit that Returns a Dataframe
#' 
#' Wrapper for \code{colSplit} that returns a dataframe.
#' 
#' @param dataframe A dataframe with a column that has been pasted together.
#' @param splitcol The name of the column that has been pasted together.
#' @param new.names A character vector of new names to assign to the columns.  
#' Default attempts to extract the original names before the paste.
#' @param sep The character that used in \code{paste2} to paste the columns.
#' @param keep.orig logical.  If TRUE the original pasted column will be 
#' retained as well.
#' @return Returns a dataframe with the pasted column cplit into new columns.
#' @seealso \code{\link{colSplit}}, 
#' \code{\link{paste2}}
#' @keywords column-split
#' @export
#' @examples 
#' \dontrun{
#' CO2$`Plant&Type&Treatment` <- paste2(CO2[, 1:3])
#' CO2 <- CO2[, -c(1:3)]
#' head(colsplit2df(CO2, 3))
#' head(colsplit2df(CO2, 3, qcv(A, B, C)))
#' head(colsplit2df(CO2, 3, qcv(A, B, C), keep.orig=TRUE))
#' head(colsplit2df(CO2, "Plant&Type&Treatment"))
#' CO2 <- datasets::CO2
#' }
colsplit2df <- 
function(dataframe, splitcol = 1, new.names = NULL, sep=".", 
         keep.orig=FALSE){
    if (!is.data.frame(dataframe)){
        stop("Please supply a data.frame to colsplit2df")
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
    return(w)
}
