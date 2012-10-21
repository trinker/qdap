#' Generate Word Ffrequency Matrix by Grouping Variable
#' 
#' Generate a word frequency matrix by grouping variable
#' 
#' @aliases word.freq.matrix wfm
#' @param text.var A text variable or word frequency matrix object.
#' @param grouping.var The grouping variables.  Default NULL generates one word list for all text.  Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @param wfdf a word frequency data frame. Default is null.
#' @param output output type (either "proportion", "proportion" or "percent")
#' @param stopwords A vector of stop words to remove.
#' @param digits integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed
#' @return Returns a word frequency of the class matrix
#' @seealso \code{\link[qdap]{word.freq.df}}
#' @examples
word.freq.matrix <-
function(text.var = NULL, grouping.var = NULL, wfdf = NULL,
         output = "raw", stopwords = NULL, digits = 2){
  if (!is.null(wfdf)) {
    if (comment(wfdf) == "t.df") {
      wfdf <- wfdf
    } else {
      if (comment(wfdf) == "m.df") { 
        wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
      } else {
        stop("Object must be a raw word frequency data frame")
      }
    }
    x2 <- wfdf[, -1, drop = FALSE]
    rownames(x2) <- wfdf[, 1]
    x2 <- as.matrix(x2)
  } else {
    if (!is.null(text.var)) {
      wfdf <- word.freq.df(text.var = text.var, grouping.var = grouping.var, 
                           stopwords = stopwords, output = output, digits = digits) 
      x2 <- wfdf[, -1, drop = FALSE]
      rownames(x2) <- wfdf[, 1]
      x2 <- as.matrix(x2)
    } else {
      stop ("must specify both text.var & grouping var or wfdf")
    }
  }
  comment(x2) <- "true.matrix"
  return(x2)
}

wfm <- word.freq.matrix
