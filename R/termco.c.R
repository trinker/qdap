#' Combine Columns from a termco Object
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases termco.c print.termco_c
#' @param termco.d.object %% ~~Describe \code{termco.d.object} here~~
#' @param combined.columns %% ~~Describe \code{combined.columns} here~~
#' @param new.name %% ~~Describe \code{new.name} here~~
#' @param zero.replace %% ~~Describe \code{zero.replace} here~~
#' @param elim.old %% ~~Describe \code{elim.old} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples





#' Combine Columns from a termco Object
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases termco.c print.termco_c
#' @param termco.d.object %% ~~Describe \code{termco.d.object} here~~
#' @param combined.columns %% ~~Describe \code{combined.columns} here~~
#' @param new.name %% ~~Describe \code{new.name} here~~
#' @param zero.replace %% ~~Describe \code{zero.replace} here~~
#' @param elim.old %% ~~Describe \code{elim.old} here~~
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
#' function (termco.d.object, combined.columns, new.name, zero.replace = 0, 
#'     elim.old = TRUE) 
#' {
#'     x <- termco.d.object$raw
#'     y <- termco.d.object$prop
#'     if (!is.numeric(combined.columns)) {
#'         combined.columns <- which(names(x) %in%
#'         combined.columns)
#'     }
#'     x <- transform(x, new.name = rowSums(x[, combined.columns]), 
#'         check.names = FALSE)
#'     names(x)[length(x)] <- names(y)[length(y)] <- new.name
#'     y <- transform(y, new.name = rowSums(y[, combined.columns]), 
#'         check.names = FALSE)
#'     z <- if (elim.old) {
#'         seq_along(x)[!seq_along(x) %in% combined.columns]
#'     }
#'     else {
#'         seq_along(x)
#'     }
#'     x2 <- replacer(x, with = zero.replace)[, z]
#'     y2 <- replacer(y, with = zero.replace)[, z]
#'     DF <- replacer(termco.rnp(x, y), "0(0)", with = 
#'         zero.replace)[, z]
#'     o <- list(raw = x2, prop = y2, rnp = DF)
#'     class(o) <- "termco_c"
#'     return(o)
#'   }
#' 
termco.c <-
function(termco.d.object, combined.columns, new.name, 
         zero.replace = 0, elim.old = TRUE){ 
    if (!class(termco.d.object) %in% c("termco_d", "termco_c")){
        stop("termco.d.object must be a termco.d.object or termco.c.object")
    }
    x <- termco.d.object$raw
    xcheck <- names(x)
    y <- termco.d.object$prop
    if (!is.numeric(unlist(combined.columns))){
        cc <- function(X, Y){
            which(names(Y) %in% X)
        }
        if (!is.list(combined.columns)){
            combined.columns2 <- cc(combined.columns, x)
        } else {
            combined.columns2 <- lapply(combined.columns, function(x2) 
                cc(X=x2, Y=x)) 
        }
    } else {
        combined.columns2 <- combined.columns
        cc <- function(X, Y) names(Y)[X]
        if (!is.list(combined.columns)){
            combined.columns <- cc(combined.columns, x)            
        } else {
            combined.columns <- lapply(combined.columns, function(x2) 
                cc(X=x2, x)) 
        }
    }
    if (is.list(combined.columns)){
        trx <- function(i) {  
            x <- transform(x, new.name2 = rowSums(x[, combined.columns[[i]]]), 
                           check.names=FALSE)
            names(x)[ncol(x)] <- new.name[i]
            return(x)
        }
        invisible(lapply(seq_along(combined.columns), function(inp) {
            x <<- trx(inp)
        }
        )) 
    } else {
        x <- transform(x, new.name = rowSums(x[, combined.columns]), 
                       check.names=FALSE)
        names(x) [length(x)] <- new.name
    } 
    if (elim.old) {
        x <- x[, seq_along(x)[!seq_along(x) %in% unlist(combined.columns2)]]
    } 
    x2 <- replacer(x, with = zero.replace)
    cm <- comment(termco.d.object)
    y2 <- termco.p(x, output = cm[1], digits=as.numeric(cm[2]))
    if (any(new.name %in% xcheck) & elim.old){
        #   names(x)[(ncol(x) - length(new.name) + 1):ncol(x)] <- new.name
        names(x2)[(ncol(x2) - length(new.name) + 1):ncol(x2)] <- new.name
        names(y2)[(ncol(y2) - length(new.name) + 1):ncol(y2)] <- new.name
    } else {
        if (any(new.name %in% xcheck) & !elim.old){
            new.name <- paste0(new.name, ".2")
            #       names(x)[(ncol(x) - length(new.name) + 1):ncol(x)] <- new.name
            names(x2)[(ncol(x2) - length(new.name) + 1):ncol(x2)] <- new.name
            names(y2)[(ncol(y2) - length(new.name) + 1):ncol(y2)] <- new.name
        }
    }
    p <- data.frame(sapply(x2[, -1], function(x) 
        as.numeric(as.character(x))), check.names=FALSE)
    x2 <- data.frame(x2[, 1, drop=FALSE], p, check.names=FALSE)
    p <- data.frame(sapply(y2[, -1], function(x) 
        as.numeric(as.character(x))), check.names=FALSE)
    y2 <- data.frame(y2[, 1, drop=FALSE], p, check.names=FALSE)
    trnp <- termco.rnp(x2, y2)
    DF <- data.frame(sapply(trnp, Trim), check.names=FALSE)
    DF <- replacer(DF, "0(0)", with = zero.replace)
    DF <- replacer(DF, "0(0.00)", with = zero.replace)
    o <- list(raw = x2, prop = y2, rnp = DF)
    comment(o) <- cm
    class(o) <- "termco_c"
    return(o)
}

    


