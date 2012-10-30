#' Replace Cells in a Matrix or DataFrame
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dat %% ~~Describe \code{dat} here~~
#' @param replace %% ~~Describe \code{replace} here~~
#' @param with %% ~~Describe \code{with} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
replacer <- 
function(dat, replace=0, with="-"){ 
  h <- is.vector(dat)
  i <- is.matrix(dat)
  j <- is.data.frame(dat)
  if (is.numeric(replace)){
    NAMES <- colnames(dat)
    if (h) {
      dat <- data.frame(dat)
    }
    not.num <- Negate(is.numeric)
    dat1 <- dat[, sapply(dat, not.num), drop = FALSE]
    inds <- which(sapply(dat, is.numeric))
    subdf <- function(df, ii) {
      do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
    }
    dat2 <- subdf(dat, inds)
  } else {
    dat2 <- dat
  }
  y <- as.matrix(dat2)
  if (is.na(replace)) {
    y[is.na(y)] <- with
  } else { 
    y[y==replace] <- with
  }
  if (is.numeric(replace)){    
    y <- data.frame(dat1, y, check.names = FALSE)
    m <- NAMES[!NAMES%in%names(y)]
    if(length(m) == 1) names(y)[1] <- m
    y <- subdf(y, NAMES)
  }
  if(h) y <- as.vector(y)
  if(i) y <- as.matrix(y)
  if(j) y <- as.data.frame(y, check.names = FALSE)
  return(y)
}