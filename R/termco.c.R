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
    # browser ()
    #find out if cc and nn are a list and vector or vector and single
    #make fun here 
    x <- termco.d.object$raw
    y <- termco.d.object$prop
    if (!is.numeric(combined.columns)){
        combined.columns <- which(names(x) %in% combined.columns)
    }
    x <- transform(x, new.name = rowSums(x[, combined.columns]), 
                   check.names=FALSE)
    y <- transform(y, new.name = rowSums(y[, combined.columns]), 
                   check.names=FALSE)
    z <- if (elim.old) {
        seq_along(x)[!seq_along(x) %in% combined.columns] 
    } else {
        seq_along(x)
    }
    names(x)[length(x)] <- names(y)[length(y)] <- new.name
    x2 <- replacer(x, with = zero.replace)[, z]
    y2 <- replacer(y, with = zero.replace)[, z]
    if (new.name %in% names(x)[combined.columns] & elim.old){
        names(x2)[ncol(x2)] <- new.name
        names(y2)[ncol(y2)] <- new.name
    }
    
    p <- data.frame(sapply(x2[, -1], function(x) 
        as.numeric(as.character(x))), check.names=FALSE)
    x2 <- data.frame(x2[, 1, drop=FALSE], p, check.names=FALSE)
    p <- data.frame(sapply(y2[, -1], function(x) 
        as.numeric(as.character(x))), check.names=FALSE)
    y2 <- data.frame(y2[, 1, drop=FALSE], p, check.names=FALSE)
    
    d <- as.numeric(gsub("digits=", "", comment(termco.d.object)))
    trnp <- termco.rnp(x2, y2)
    ####end of fun now lapply it  or single depending in input
    DF <- data.frame(sapply(trnp, Trim), check.names=FALSE)
    DF <- replacer(DF, "0(0)", with = zero.replace)
    DF <- replacer(DF, "0(0.00)", with = zero.replace)
    o <- list(raw = x2, prop = y2, rnp = DF)
    class(o) <- "termco_c"
    return(o)
}


