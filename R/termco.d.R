#' Convienence Wrapper to Combine termco, termco.p and termco.rnp
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases termco.d print.termco_d
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param match.string %% ~~Describe \code{match.string} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
#' @param ignore.case %% ~~Describe \code{ignore.case} here~~
#' @param zero.replace %% ~~Describe \code{zero.replace} here~~
#' @param output %% ~~Describe \code{output} here~~
#' @param digits %% ~~Describe \code{digits} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (text.var, match.string, grouping.var, ignore.case = FALSE, 
#'     zero.replace = 0, output = "percent", digits = 2) 
#' {
#'     NAME <- if (is.list(grouping.var)) {
#'         m <- unlist(as.character(substitute(grouping.var))[-1])
#'         m <- sapply(strsplit(m, "$", fixed = TRUE), function(x)
#'             x[length(x)])
#'         paste(m, collapse = "&")
#'     }
#'     else {
#'         G <- as.character(substitute(grouping.var))
#'         G[length(G)]
#'     }
#'     x <- termco(text.var = text.var, match.string =
#'         match.string, 
#'         grouping.var = grouping.var)
#'     names(x)[1] <- NAME
#'     y <- termco.p(tco = x, output = output, digits = digits)
#'     if (zero.replace != 0) {
#'         x[, -c(1:2)] <- lapply(x[, -c(1:2)], function(x) 
#'             replacer(x, 0, zero.replace))
#'         y[, -c(1:2)] <- lapply(y[, -c(1:2)], function(x) 
#'             replacer(x, 0, zero.replace))
#'     }
#'     z <- termco.rnp(x, y)
#'     h <- paste(zero.replace, "(", zero.replace, ")", sep = "")
#'     z[, -c(1:2)] <- lapply(z[, -c(1:2)], function(x) replacer(x, 
#'         h, zero.replace))
#'     o <- list(raw = x, prop = y, rnp = z)
#'     class(o) <- "termco_d"
#'     return(o)
#'   }
#' 
termco.d <-
  function (text.var, grouping.var=NULL, match.string, short.term = FALSE,
    ignore.case = FALSE, zero.replace = 0, output = "percent", digits = 2){
  NAME <- if (is.null(grouping.var)) {
    "all"
  } else {
    if (is.list(grouping.var)) {
      m <- unlist(as.character(substitute(grouping.var))[-1])
      m <- sapply(strsplit(m, "$", fixed = TRUE), 
                  function(x) x[length(x)])
      paste(m, collapse = "&")
    } else {
      G <- as.character(substitute(grouping.var))
      G[length(G)]
    }
  }
  x <- termco(text.var = text.var, match.string = match.string, 
              grouping.var = grouping.var, ignore.case = ignore.case)
  names(x)[1] <- NAME
  y <- termco.p(tco = x, output = output, digits = digits)
  if (is.null(grouping.var)){
    z <- termco.rnp(x, y)
    znull <- as.character(z$DF)
    names(znull) <- rownames(z)
    z <- t(as.data.frame(znull))
    z <- replacer(z, "0(0)", with = zero.replace)
    z <- replacer(z, "0(0.00)", with = zero.replace)
    z <- noquote(z)
    rownames(z) <- "all"
    if (zero.replace != 0) {
      x[, -c(1:2)] <- replacer(x[, -c(1:2)], 0, zero.replace)
      y[, -c(1:2)] <- replacer(y[, -c(1:2)], 0, zero.replace)
    }
  } else {
    if (zero.replace != 0) {
      x[, -c(1:2)] <- replacer(x[, -c(1:2), drop = FALSE], 
        0, zero.replace)
      y[, -c(1:2)] <- replacer(y[, -c(1:2), drop = FALSE], 
        0, zero.replace)
    }
    z <- termco.rnp(x, y)
    h <- paste(zero.replace, "(", zero.replace, ")", sep = "")
    z[, -c(1:2)] <- lapply(z[, -c(1:2), drop = FALSE], 
      function(x) replacer(x, h, zero.replace))
  }
  o <- list(raw = x, prop = y, rnp = z, zero_replace = zero.replace,
    output = output, digits = digits)
  class(o) <- "termco_d"
  if (short.term) {
    o <- termco2short.term(o)
  }
  return(o)
}