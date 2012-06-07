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
termco2mat <-
function (dataframe, drop.wc = TRUE, short.colnames = TRUE, rm.zerocol = FALSE, 
    no.quote = TRUE, transform = TRUE) {
    ind <- if (drop.wc) {
        1:2
    } else {
        1
    }
    MAT <- as.matrix(dataframe[, -c(ind)])
    rownames(MAT) <- dataframe[, 1]
    if (short.colnames) {
        mn <- gsub("(.*)\\)([^\\)]*)", "\\1\\2", colnames(MAT))
        colnames(MAT) <- gsub("term(", "", mn, fixed=TRUE)
    }
    if (rm.zerocol) {
        fun <- function(x) all(ifelse(x == 0, T, F))
        MAT <- MAT[, !apply(MAT, 2, fun)]
    }
    
    OC <- length(grep("(", as.vector(unlist(MAT)), fixed = TRUE)) == 0
    if (OC) {
        z <- rownames(MAT)
        MAT <- apply(MAT, 2, as.numeric)
        rownames(MAT) <- z
    }
    if (no.quote & !OC){ 
        MAT <- noquote(MAT)
    }
    if (transform){
        MAT <- t(MAT)
    }
    return(MAT)
}
