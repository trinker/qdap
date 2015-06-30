#' Nested Standardization
#' 
#' Standardize within a subgroup and then within a group.
#' 
#' @param numeric.var A numeric variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param original_order logical.  IF \code{TRUE} orders by the original order.  
#' If \code{FALSE} orders by group.
#' @param digits Integer; number of decimal places to round.
#' @return Returns a list of two:
#' \item{SCALED_OBSERVATIONS}{A dataframe of scaled observations at level one 
#' and two of the nesting with possible outliers.} 
#' \item{DESCRIPTIVES_BY_GROUP}{A data frame of descriptives by group.}
#' @seealso \code{\link[base]{scale}}
#' @keywords scale
#' @export
#' @examples
#' \dontrun{
#' dat <- with(mraja1spl, word_stats(dialogue, list(person, sex, fam.aff)))
#' htruncdf(colsplit2df(dat$ts), ,4)
#' out1 <- with(colsplit2df(dat$ts), multiscale(word.count, person))
#' ltruncdf(out1, 10)
#' out2 <- with(colsplit2df(dat$ts), multiscale(word.count, 
#'     list(fam.aff, sex)))
#' ltruncdf(out2, 10)
#' out3 <- with(colsplit2df(dat$ts), multiscale(word.count, 
#'     list(fam.aff, sex), original_order = FALSE))
#' ltruncdf(out3, 10)
#' }
multiscale <-
function(numeric.var, grouping.var, original_order = TRUE, digits = 2) {
    text.var <- NULL
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
    X$scale.by.group <- unlist(stats::aggregate(num ~ group, X, scale)$num)
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
        MSD <- c(mean = mean(x), sd = stats::sd(x), n = length(x), total = sum(x))
        round(MSD, digits)
    }
    mean.sd.n <- stats::aggregate(num ~ group, X, function(x) msd(x, 
        digits = digits))
    mean.sd.n$group <- as.character(mean.sd.n$group)
    ALL <- c("ALL", msd(numeric.var, digits = digits))
    mean.sd.n <- rbind(as.matrix(mean.sd.n), ALL)
    rownames(mean.sd.n) <- 1:nrow(mean.sd.n)
    mean.sd.n <- as.data.frame(mean.sd.n)
    if (original_order) { 
        X <-  X[order(X$ID), ]
    }
    X$ID <- NULL
    colnames(mean.sd.n) <- c(G, "mean", "sd", "n.turns", "total")
    names(X) <- c(G, N, names(X)[-c(1:2)])
    list(SCALED_OBSERVATIONS = X, DESCRIPTIVES_BY_GROUP = mean.sd.n)
}
