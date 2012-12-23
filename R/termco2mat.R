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
#' @param no.quote logical.  If TRUE the matrix will be printed without quotes
#' if it's character.
#' @param transform logical.  If TRUE the matrix will be transformed.
#' @keywords ~kwd1 ~kwd2
#' @examples
termco2mat <-function (dataframe, drop.wc = TRUE, short.terms = TRUE, 
  rm.zerocol = FALSE, no.quote = TRUE, transform = TRUE, trim.terms = TRUE) {
  if (class(dataframe) %in% c("termco_d", "termco_c")) {
    dataframe <- dataframe[["raw"]]
  }
  if (!is.data.frame(dataframe)) {
    stop("Please supply a data.frame to termco2mat")
  }
  ind <- if (drop.wc) {
    1:2
  } else {
    1
  }
  MAT <- as.matrix(dataframe[, -c(ind), drop = FALSE])
  rownames(MAT) <- dataframe[, 1]
  if (short.terms) {
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
  if (trim.terms) {
    rownames(MAT) <- Trim(rownames(MAT))
  }
  return(MAT)
}