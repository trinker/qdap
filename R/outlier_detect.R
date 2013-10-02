#' Detect Outliers in Text
#' 
#' Locate possible outliers for text variables given numeric word function.
#' 
#' @param text.var The text variable.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param FUN A word function with a numeric vector output (e.g., 
#' \code{syllable_sum}, \code{character_count} or \code{word_count}).
#' @param scale.by A character string indicating which dimensions to scale by. 
#' One of \code{"all"}, \code{"grouping"}, or \code{"both"}.  Default NULL scales 
#' by all.
#' @return Returns a dataframe with possible outliers.
#' @export
#' @examples
#' \dontrun{
#' with(DATA, outlier_detect(state))
#' with(DATA, outlier_detect(state, FUN = character_count))
#' with(DATA, outlier_detect(state, person, FUN = character_count))
#' with(DATA, outlier_detect(state, list(sex, adult), FUN = character_count))
#' with(DATA, outlier_detect(state, FUN = syllable_sum))
#' htruncdf(with(raj, outlier_detect(dialogue, person)), 15, 45)
#' }   
outlier_detect <-
function(text.var, grouping.var = NULL, FUN = word_count, 
    scale.by="grouping") {
    if(is.null(grouping.var)) {
        G <- "all"
    } else {
        if (is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), function(x) {
                    x[length(x)]
                }
            )
            G <- paste(m, collapse="&")
        } else {
            G <- as.character(substitute(grouping.var))
            G <- G[length(G)]
        }
    }
    if(is.null(grouping.var)){
        grouping <- rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            grouping <- paste2(grouping.var)
        } else {
            grouping <- unlist(grouping.var)
        } 
    } 
    fun <- match.fun(FUN)
    z <- fun(text.var)
    names(z) <- NULL
    if (!is.null(grouping.var) & scale.by %in% c("grouping", "both")) {
        z <- split(data.frame(id=seq_along(z), z), grouping)
        z <- lapply(z, function(x) {
                cbind(x[, 1, drop=FALSE], scale(x[, 2]))
             }
        )
        z <- do.call("rbind", z)
        z <- z[order(z[, 1]), ]
        names(z)[2] <-"scale"
        z <- z[order(z[, 1]), 2]
    }
    if ((!is.null(grouping.var) & scale.by %in% c("all", "both"))|
        is.null(grouping.var)) {    
        z <- scale(z)
    }
    w <- data.frame(X=grouping, scale=z, 
        label=outlier_labeler(z), text.var=text.var)
    names(w)[1] <- c(G)
    if(is.null(grouping.var)) w[, 1] <- NULL
    return(w)
}
