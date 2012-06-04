#' Split Sentences Into Individual Lines
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dataframe %% ~~Describe \code{dataframe} here~~
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param splitpoint %% ~~Describe \code{splitpoint} here~~
#' @param rnames %% ~~Describe \code{rnames} here~~
#' @param text.place %% ~~Describe \code{text.place} here~~
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
#' function (dataframe, text.var, splitpoint = NULL, rnames = "numeric", 
#'     text.place = "original") 
#' {
#'     DF <- dataframe
#'     if (length(DF) < 3) {
#'         DF$EXTRA1x2 <- 1:nrow(DF)
#'         DF$EXTRA2x2 <- 1:nrow(DF)
#'     }
#'     else {
#'         DF
#'     }
#'     input <- as.character(substitute(text.var))
#'     re <- ifelse(is.null(splitpoint), "[\?\.\!]", as.character(substitute(splitpoint)))
#'     RN <- as.character(substitute(rnames))
#'     TP <- as.character(substitute(text.place))
#'     TP <- "original"
#'     breakinput <- function(input, re) {
#'         j <- gregexpr(re, input)
#'         lengths <- unlist(lapply(j, length))
#'         spots <- lapply(j, as.numeric)
#'         first <- unlist(lapply(spots, function(x) {
#'             c(1, (x + 1)[-length(x)])
#'         }))
#'         last <- unlist(spots)
#'         ans <- substring(rep(input, lengths), first, last)
#'         return(list(text = ans, lengths = lengths))
#'     }
#'     j <- breakinput(DF[, input], re)
#'     others <- DF[, -which(colnames(DF) %in% input), drop = FALSE]
#'     idx <- rep(1:dim(others)[1], j$lengths)
#'     ans <- data.frame(cbind(input = Trim(j$text), others[idx, 
#'         ]))
#'     colnames(ans)[1] <- input
#'     x <- as.character(rownames(ans))
#'     y <- strsplit(x, "\.")
#'     z <- sapply(y, function(x) as.numeric(x[2]))
#'     z[is.na(z)] <- 0
#'     z <- as.character(z + 1)
#'     a <- sapply(y, function(x) x[1])
#'     x <- paste(a, ".", z, sep = "")
#'     rownames(ans) <- TOT <- x
#'     if (RN == "numeric") {
#'         rownames(ans) <- 1:nrow(ans)
#'     }
#'     if (TP == "original") {
#'         ans <- ans[, colnames(DF)]
#'     }
#'     else {
#'         if (TP == "right") {
#'             ans <- data.frame(ans[, -1], ans[, 1])
#'             colnames(ans) <- c(colnames(ans)[-ncol(ans)], input)
#'         }
#'         else {
#'             if (TP == "left") {
#'                 ans
#'             }
#'         }
#'     }
#'     if (RN == "numeric") {
#'         ans <- data.frame(tot = TOT, ans)
#'     }
#'     ans$EXTRA1x2 <- NULL
#'     ans$EXTRA2x2 <- NULL
#'     return(ans)
#'   }
#' 
sentSplit <-
function(dataframe, text.var, splitpoint = NULL, 
    rnames = 'numeric', text.place = 'original') {
    DF <- dataframe
    if(length(DF) < 3) {
        DF$EXTRA1x2 <-  1:nrow(DF); DF$EXTRA2x2 <-  1:nrow(DF)
    } else {
        DF
    }
    input <- as.character(substitute(text.var))
    re <- ifelse(is.null(splitpoint), "[\\?\\.\\!]", 
        as.character(substitute(splitpoint)))
    RN <- as.character(substitute(rnames))
    TP <- as.character(substitute(text.place))
    TP<-'original'
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
    x<-as.character(rownames(ans))
    y<-strsplit(x, "\\.")
    z<-sapply(y, function(x) as.numeric(x[2]))
    z[is.na(z)] <- 0
    z<-as.character(z+1)
    a<-sapply(y, function(x)x[1])
    x <-paste(a, ".", z, sep="")
    rownames(ans) <- TOT <-x
    if (RN == "numeric") {
        rownames(ans) <- 1:nrow(ans)
    }
    if (TP == "original") {
        ans <- ans[, colnames(DF)]
    } else {
        if (TP == "right") {
            ans <- data.frame(ans[, -1], ans[, 1])
            colnames(ans)<-c(colnames(ans)[-ncol(ans)],input)
        } else {
            if (TP == "left") {
                ans
            }
        }
    }
    if (RN == "numeric") {
        ans <- data.frame(tot=TOT , ans)
    }
    ans$EXTRA1x2 <- NULL; ans$EXTRA2x2 <- NULL
    return(ans)
}
