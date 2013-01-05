#' Multiple gsub
#' 
#' A wrapper for \code{\link[base]{gsub}} that takes a vector of search terms 
#' and a vector or single value of replacements.
#' 
#' @param pattern Character string to be matched in the given character vector. 
#' @param replacement Character string equal in length to pattern or of length 
#' one which are  a replacement for matched pattern. 
#' @param text.var The text variable.
#' @param \dots Additional arguments passed to \code{\link[base]{gsub}}.
#' @rdname multigsub
#' @return Returns a vector with the pattern replaced.
#' @note The replacements occur sequentially rather than all at once.  This 
#' means a previous (first in pattern string) sub could alter a later sub.
#' @seealso \code{\link[base]{gsub}}
#' @export
#' @examples
#' \dontrun{
#' multigsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub("[:punct:]", "PUNC", DATA$state, fixed = FALSE)
#' }
multiscale <-
function(numeric.var, grouping.var, order.by = "original", 
    digits = 2) {
    G <- if(is.null(grouping.var)) {
        "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- apply(data.frame(grouping.var), 1, function(x){
                if (any(is.na(x))){
                        NA
                    } else {
                        paste(x, collapse = ".")
                    }
                }
            )
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    N <- as.character(substitute(numeric.var))
    N <- N[length(N)]
    X <- data.frame(ID = seq_along(numeric.var), group = grouping, 
        num = numeric.var)
    X <- X[order(X$group), ]
    X$scale.by.group <- unlist(aggregate(num ~ group, X, scale)$num)
    X$outlier.by.group <- 
        ifelse(abs(X$scale.by.group) >= 3, "3 sd", 
        ifelse(abs(X$scale.by.group) >= 2 & abs(X$scale.by.group) < 3, 
            "2 sd", 
        ifelse(abs(X$scale.by.group) >= 1.5 & abs(X$scale.by.group) < 2, 
            "1.5 sd", "")
        )
    )
    X$scale.by.all <- scale(X$num)
    X$outlier.for.all <- 
        ifelse(abs(X$scale.by.all) >= 3, "3 sd", 
        ifelse(abs(X$scale.by.all) >= 2 & abs(X$scale.by.all) < 3, "2 sd", 
        ifelse(abs(X$scale.by.all) >= 1.5 & abs(X$scale.by.all) < 2, 
            "1.5 sd", "")
        )
    )
    msd <- function(x, digits) {
        MSD <- c(mean = mean(x), sd = sd(x), n = length(x), total = sum(x))
        round(MSD, digits)
    }
    mean.sd.n <- aggregate(num ~ group, X, function(x) msd(x, 
        digits = digits))
    mean.sd.n$group <- as.character(mean.sd.n$group)
    ALL <- c("ALL", msd(numeric.var, digits = digits))
    mean.sd.n <- rbind(as.matrix(mean.sd.n), ALL)
    rownames(mean.sd.n) <- 1:nrow(mean.sd.n)
    mean.sd.n <- as.data.frame(mean.sd.n)
    X <- switch(order.by, 
        original = X[order(X$ID), ], 
        group = X
    )
    X$ID <- NULL
    colnames(mean.sd.n) <- c(G, "mean", "sd", "n.turns", "total")
    names(X) <- c(G, N, names(X)[-c(1:2)])
    list(SCALED_OBSERVATIONS = X, DESCRIPTIVES_BY_GROUP = mean.sd.n)
}

#' @rdname multigsub
#' @export
mgsub <- multigsub
