#' Order a data frame by its columns.
#'
#' This function completes the subsetting, transforming and ordering triad
#' with a function that works in a similar way to \code{\link{subset}} and 
#' \code{\link{transform}} but for reordering a data frame by its columns.
#' This saves a lot of typing!
#'
#' @param df data frame to reorder
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
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
