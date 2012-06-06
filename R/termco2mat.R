#' Convert a termco dataframe to a matrix
#' 
#' Convert a termco dataframe to a matrix for use with visualization functions
#' (e.g. heatmap2 of gplots)
#' 
#' 
#' @param dataframe A termco dataframe
#' @param drop.wc logical.  If TRUE the word count column will be dropped.
#' @param short.colnames logical.  If TRUE the ``term()'' portion of column
#' names will be dropped.
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
termco2mat <-
function (dataframe, drop.wc=TRUE, short.colnames=TRUE) {
    ind <- if(drop.wc) 1:2 else 1
    MAT <- as.matrix(dataframe[, -c(ind)])
    rownames(MAT) <- dataframe[, 1]
    if (short.colnames) {
        w <- do.call(rbind, strsplit(colnames(MAT), " "))[, 2:3]
        colnames(MAT) <- Trim(paste0(w[, 1], gsub(")", "", w[, 2], fixed=TRUE)))
    }
    return(MAT)
}

