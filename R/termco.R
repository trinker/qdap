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
termco <-
function(text.var, match.string, grouping.var=NULL, 
    ignore.case=FALSE, zero.replace = 0){
    group.var <- grouping.var
    x <- if (ignore.case) {
        lapply(text.var, function(x) term.count(tolower(x), 
            mat=tolower(match.string)))
    } else {
        lapply(text.var, function(x) term.count(x, mat=match.string))
    }
    group.var <- if(is.list(group.var) & length(group.var)>1) {
        apply(data.frame(group.var), 1, function(x){
            if (any(is.na(x))){
                    NA
                } else {
                    paste(x, collapse = ".")
                }
             }
         )
    } else {
        grouping.var
    }                
    y <- data.frame(text.var, do.call("rbind", x))
    if(is.null(group.var)){
        names(y) <- c(as.character(substitute(text.var))[3], 
            paste("term(", match.string, ")", sep=""))
        return(y)
    } else {
        y <- data.frame(group.var, y[, -1])
        X <- data.frame(Y=word.count(text.var), G=group.var)
        Z <- aggregate(Y~G, X, sum)
        z <- lapply(2:length(y), function(x) {
                aggregate(y[, x]~group.var, y, sum)
            }
        )
        w <- data.frame(z[[1]][1], Z[, 2], 
            lapply(seq_along(z), function(x) z[[x]][, 2]))
        NAME <- if(is.list(grouping.var)) {
            m <- unlist(as.character(substitute(grouping.var))[-1])
            m <- sapply(strsplit(m, "$", fixed=TRUE), 
                function(x) x[length(x)]) 
            paste(m, collapse="&")
        }else{
            G <- as.character(substitute(grouping.var))
            G[length(G)]
        }
        names(w) <- c(NAME, "word.count", 
            paste("term(", match.string, ")", sep=""))
        w[, -c(1:2)] <- replacer(w[, -c(1:2)], replace=0, with=zero.replace) 
        return(w)
    }
}
