#' Convienence Wrapper to Combine termco, termco.p and termco.rnp
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases termco.d print.termco_d
#' @param text.var text.var The text variable
#' @param grouping.var The grouping variables.  Default NULL generates one word list for all text.  Also takes a single grouping variable or a list of 1 or more grouping variables.
#' @param match.string a vector of word bases to search for
#' @param ignore.case logical.  If TRUE case is ignored.
#' @param zero.replace value to replace 0 values with
#' @param output Type of proportion output; either "proportion" (decimal format) or "percent".  Default is percent.
#' @param digits  integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
termco.d <-
  function (text.var, grouping.var=NULL, match.string, short.term = FALSE,
    ignore.case = TRUE, zero.replace = 0, output = "percent", digits = 2, 
    lazy.term = TRUE, ...){
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
  x <- termco(text.var = strip(text.var, lower.case = FALSE, ...), match.string = match.string, 
              grouping.var = grouping.var, ignore.case = ignore.case)
  names(x)[1] <- NAME
  y <- termco.p(tco = x, output = output, digits = digits)
  if (is.null(grouping.var) & y[1, 1] != "all"){
    z <- termco.rnp(x, y, output = output)
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
    z <- termco.rnp(x, y, output = output)
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