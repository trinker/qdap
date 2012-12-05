#' Split Sentences Into Individual Lines
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dataframe A dataframe that contains the person variable.
#' @param text.var The text variable.
#' @param splitpoint %% ~~Describe \code{splitpoint} here~~
#' @param rnames %% ~~Describe \code{rnames} here~~
#' @param text.place %% ~~Describe \code{text.place} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author Dason Kurkiewicz and Tyler Rinker <tyler.rinker@gmail.com>.
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
sentSplit <-
function(dataframe, text.var, splitpoint = NULL, incomplete.sub = TRUE,  
  rm.bracket = TRUE, stem.col = TRUE, text.place = 'right', ...) {
  DF <- dataframe
  if (is.numeric(text.var)) {
    text.var <- colnames(DF)[text.var]
  }
  if (incomplete.sub) {
    DF[, text.var] <- incomplete.replace(DF[, text.var])
  }
  if (rm.bracket) {
      DF[, text.var] <- bracketX(DF[, text.var])
  }
  if(length(DF) < 3) {
    DF$EXTRA1x2 <-  1:nrow(DF); DF$EXTRA2x2 <-  1:nrow(DF)
  } else {
    DF
  }
  input <- text.var
  re <- ifelse(is.null(splitpoint), "[\\?\\.\\!\\|]", 
    as.character(substitute(splitpoint)))
  TP <- text.place
  breakinput <- function(input, re) {
    j <- gregexpr(re, input)
    lengths <- unlist(lapply(j, length))
    spots <- lapply(j, as.numeric)
    first <- unlist(lapply(spots, function(x) {
          c(1, (x + 1)[-length(x)])
        }
      )
    )
    last <- unlist(spots)
    ans <- substring(rep(input, lengths), first, last)
    return(list(text = ans, lengths = lengths))
  }
  j <- breakinput(DF[, input], re)
  others <- DF[, -which(colnames(DF) %in% input), drop=FALSE]
  idx <- rep(1:dim(others)[1], j$lengths)
  ans <- data.frame(cbind(input = Trim(j$text), others[idx, ]))
  colnames(ans)[1] <- input
  vlen <- sapply(j$lengths, seq_len)
  ans$tot <- unlist(lapply(seq_along(vlen), function(i) paste0(i, ".", vlen[[i]])))
  if(any(na.omit(ans[, input]==""))){
    NAdet <- ans[, input]==""
    NAdet[is.na(NAdet)] <- FALSE
    ans[NAdet, input] <- NA
  }
  if (TP == "original") {
    ans <- ans[, colnames(DF)]
    if (stem.col) {
      ans <- stem2df(ans, which(colnames(ans) %in% text.var), ...)
    } 
  } else {
    if (TP == "right") {
      ans <- data.frame(ans[, -1], ans[, 1])
      totn <- which(names(ans)=="tot")
      ans <- data.frame(ans[, 1, drop= FALSE], ans[, totn, drop = FALSE],
                        ans[, -c(1, totn), drop = FALSE])
      colnames(ans) <- c(colnames(ans)[-ncol(ans)], input)
      if (stem.col) {
        ans <- stem2df(ans, ncol(ans), warn = FALSE, ...)
      }   
    } else {
      if (TP == "left") {
        ans <- ans
        if (stem.col) {
          ans <- stem2df(ans, 1, ...)
        }             
      } else {
        warning("incorrect text.place argument")
      }
    }
  }
  ans$EXTRA1x2 <- NULL; ans$EXTRA2x2 <- NULL
  rownames(ans) <- NULL
  ans[, text.var] <- as.character(ans[, text.var])
  return(ans)
}