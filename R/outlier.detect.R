#' Transcript Apply Detection Outliers in Text
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
#' @param FUN %% ~~Describe \code{FUN} here~~
#' @param scale.by %% ~~Describe \code{scale.by} here~~
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
#' function (text.var, grouping.var = NULL, FUN = word.count, scale.by = "grouping") 
#' {
#'     if (is.null(grouping.var)) 
#'         scale.by <- "all"
#'     G <- if (is.null(grouping.var)) {
#'         "all"
#'     }
#'     else {
#'         if (is.list(grouping.var)) {
#'             m <- unlist(as.character(substitute(grouping.var))[-1])
#'             m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) {
#'                 x[length(x)]
#'             })
#'             paste(m, collapse = "&")
#'         }
#'         else {
#'             G <- as.character(substitute(grouping.var))
#'             G[length(G)]
#'         }
#'     }
#'     grouping <- if (is.null(grouping.var)) {
#'         rep("all", length(text.var))
#'     }
#'     else {
#'         if (is.list(grouping.var) & length(grouping.var) > 1) {
#'             apply(data.frame(grouping.var), 1, function(x) {
#'                 if (any(is.na(x))) {
#'                   NA
#'                 }
#'                 else {
#'                   paste(x, collapse = ".")
#'                 }
#'             })
#'         }
#'         else {
#'             unlist(grouping.var)
#'         }
#'     }
#'     fun <- match.fun(FUN)
#'     z <- fun(text.var)
#'     names(z) <- NULL
#'     if (!is.null(grouping.var) & scale.by %in% c("grouping", 
#'         "both")) {
#'         z <- split(data.frame(id = seq_along(z), z), grouping)
#'         z <- lapply(z, function(x) {
#'             cbind(x[, 1, drop = FALSE], scale(x[, 2]))
#'         })
#'         z <- do.call("rbind", z)
#'         z <- z[order(z[, 1]), ]
#'         names(z)[2] <- "scale"
#'         p <- which(tabulate(z[, 1]) == 0)
#'         z <- rbind(data.frame(id = p, scale = NA), z)
#'         z <- z[order(z[, 1]), 2]
#'     }
#'     if ((!is.null(grouping.var) & scale.by %in% c("all", "both")) | 
#'         is.null(grouping.var)) {
#'         z <- scale(z)
#'     }
#'     w <- data.frame(X = grouping, scale = z, label = outlier.labeler(z), 
#'         text.var = text.var)
#'     names(w)[1] <- c(G)
#'     if (is.null(grouping.var)) 
#'         w[, 1] <- NULL
#'     return(w)
#'   }
#' 
outlier.detect <-
function(text.var, grouping.var = NULL, FUN = word.count, 
    scale.by="grouping") {
    if (is.null(grouping.var)) scale.by <- "all"

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
    grouping <- if(is.null(grouping.var)){
        rep("all", length(text.var))
    } else {
        if (is.list(grouping.var) & length(grouping.var)>1) {
            apply(data.frame(grouping.var), 1, function(x){
                    if (any(is.na(x))){
                        NA
                    } else {
                        paste(x, collapse = ".")
                    }
                }
            )
        } else {
            unlist(grouping.var)
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
        p <- which(tabulate(z[, 1])==0)
        z <- rbind(data.frame(id=p, scale=NA), z)
        z <- z[order(z[, 1]), 2]
    }
    if ((!is.null(grouping.var) & scale.by %in% c("all", "both"))|
        is.null(grouping.var)) {    
        z <- scale(z)
    }
    w <- data.frame(X=grouping, scale=z, 
        label=outlier.labeler(z), text.var=text.var)
    names(w)[1] <- c(G)
    if(is.null(grouping.var)) w[, 1] <- NULL
    return(w)
}
