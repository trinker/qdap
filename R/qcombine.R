#' Combine Columns
#' 
#' Quickly combine columns (summed) and rename.
#' 
#' @param mat A matrix or dataframe with numeric combine columns.
#' @param combined.columns A list of named vectors of the colnames/indexes 
#' of the numeric columns to be combined (summed).  If a vector is unnamed a 
#' name will be assigned. 
#' @param elim.old logical.  If \code{TRUE} eliminates the columns that are 
#' combined together by the named match.list. \code{TRUE} outputs the table 
#' proportionally (see \code{\link[qdap]{prop}}).
#' @return Returns a dataframe with combines columns.
#' @seealso \code{\link[base]{transform}}
#' @export 
#' @examples
#' \dontrun{
#' A <- list(
#'     a = c(1, 2, 3),
#'     b = qcv(mpg, hp),
#'     c = c("disp", "am")
#' )
#' B <- list(
#'     c(1, 2, 3),
#'     d = qcv(mpg, hp),
#'     c("disp", "am")
#' )
#' 
#' qcombine(head(mtcars), A)
#' qcombine(head(mtcars), B)
#' qcombine(head(mtcars), B, elim.old = FALSE)
#' }
qcombine <- 
function(mat, combined.columns, elim.old = TRUE){

    L1 <- lapply(combined.columns, function(x) {
        if (is.numeric(x)) {
            x <- names(mat)[x]
        }    
        if(all(x %in% colnames(mat))){
            return(unlist(rowSums(mat[, x, drop = FALSE])))
        }
        if(all(!x %in% colnames(mat))){
            return(unlist(rep(NA, nrow(mat))))
        }
        if(sum(x %in% colnames(mat)) == 1){
            return(unlist(mat[colnames(mat) %in% x]))
        }        
        return(unlist(rowSums(mat[colnames(mat) %in% x])))
    })
    DF <- data.frame(do.call(cbind, L1), check.names = FALSE)
    DF <- DF[ !sapply(DF, function(x) all(is.na(x)))]


    if(elim.old) {
        for (i in  seq_len(length(combined.columns))) {
            CC <- sapply(combined.columns[[i]], function(x) {
                which(x == names(mat))[1]
            })
            nms <- colnames(mat)[!1:ncol(mat) %in% CC]
            mat <- mat[ , !1:ncol(mat) %in% CC, drop = FALSE]
            colnames(mat) <- nms
        }
    }

    data.frame(mat, DF, check.names = FALSE)
}

