#' Transcript Apply Parts of Speech by Grouping Variable
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
#' @param digits %% ~~Describe \code{digits} here~~
#' @param return.pos %% ~~Describe \code{return.pos} here~~
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
#' function (text.var, grouping.var = NULL, digits = 2, return.pos = FALSE) 
#' {
#'     COM <- comment(text.var)
#'     G <- if (is.null(grouping.var)) {
#'         "all"
#'     }
#'     else {
#'         if (is.list(grouping.var)) {
#'             m <- unlist(as.character(substitute(grouping.var))[-1])
#'             m <- sapply(strsplit(m, "$", fixed = TRUE), function(x) x[length(x)])
#'             paste(m, collapse = "&")
#'         }
#'         else {
#'             G <- as.character(substitute(grouping.var))
#'             G[length(G)]
#'         }
#'     }
#'     if (is.null(comment(text.var))) {
#'         pos.list <- pos(text.var, digits = digits)
#'         text.var <- pos.list[[3]]
#'     }
#'     else {
#'         com <- comment(text.var)
#'         text.var <- switch(com, POS = text.var[[3]], POStagged = {
#'             o <- text.var[, 2]
#'             lev <- sort(unique(unlist(o)))
#'             G4 <- do.call(rbind, lapply(o, function(x, lev) {
#'                 tabulate(factor(x, levels = lev, ordered = TRUE), 
#'                   nbins = length(lev))
#'             }, lev = lev))
#'             colnames(G4) <- sort(lev)
#'             G4 <- data.frame(wrd.cnt = text.var[, 3], G4, check.names = FALSE)
#'             if (any(is.na(G4$wrd.cnt))) {
#'                 nas <- which(is.na(G4$wrd.cnt))
#'                 G4[nas, 2:ncol(G4)] <- NA
#'             }
#'             G4
#'         }, POSprop = {
#'             text.var <- data.frame(text.var[, 1], round(sapply(text.var[, 
#'                 -1], function(x) x * text.var[, 1])), check.names = FALSE)
#'             names(text.var) <- gsub("prop", "", names(text.var))
#'             names(text.var)[1] <- "wrd.cnt"
#'             text.var
#'         }, POSfreq = text.var)
#'     }
#'     grouping <- if (is.null(grouping.var)) {
#'         rep("all", nrow(text.var))
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
#'     DF1 <- data.frame(grouping, text.var, check.names = FALSE)
#'     L1 <- split(DF1, DF1$grouping)
#'     L2 <- lapply(L1, function(x) colSums(x[, -1], na.rm = TRUE))
#'     DF2 <- data.frame(do.call("rbind", L2), check.names = FALSE)
#'     DF2 <- data.frame(replace = rownames(DF2), DF2, check.names = FALSE)
#'     rownames(DF2) <- 1:nrow(DF2)
#'     colnames(DF2)[1] <- G
#'     if (is.null(COM) & return.pos) {
#'         return(list(p = pos.list, outcome = DF2))
#'     }
#'     else {
#'         return(DF2)
#'     }
#'     if (!is.null(COM)) {
#'         if (COM == "POSprop") {
#'             W1 <- "Supplied text.var is based on proportions."
#'             W2 <- "  Outcomes may be inaccurate."
#'             warning(paste0(W1, W2))
#'         }
#'     }
#'   }
#' 
pos.by <-
function(text.var, grouping.var = NULL, digits = 2, ...){
    G <- if(is.null(grouping.var)) {
             "all"
         } else {
             if (is.list(grouping.var)) {
                 m <- unlist(as.character(substitute(grouping.var))[-1])
                 m <- sapply(strsplit(m, "$", fixed=TRUE), 
                     function(x) x[length(x)])
                 paste(m, collapse="&")
             } else {
                  G <- as.character(substitute(grouping.var))
                  G[length(G)]
             }
         }
    if (!class(text.var) %in% c("POS", "POSby", "formality.measure")) {
        pos.list <- pos(text.var, digits = digits, ...)
        text.var <- pos.list[["POSfreq"]]
    } else {
        pos.list <- text.var
        text.var <- text.var[["POSfreq"]]
    }
    grouping <- if(is.null(grouping.var)){
                     rep("all", nrow(text.var))
                 } else {
                     if(is.list(grouping.var) & length(grouping.var)>1) {
                         apply(data.frame(grouping.var), 1, function(x){
                             if(any(is.na(x))){NA}else{paste(x, collapse = ".")
                                 }
                             }
                         )
                     } else {
                        unlist(grouping.var)
                     } 
                 } 
    DF1 <- data.frame(grouping, text.var, check.names = FALSE)
    L1 <- split(DF1, DF1$grouping)
    L2 <- lapply(L1, function(x) colSums(x[, -1], na.rm = TRUE))
    DF2 <- data.frame(do.call("rbind", L2), check.names = FALSE)
    DF2 <- data.frame(replace = rownames(DF2), DF2, check.names = FALSE)
    rownames(DF2) <- 1:nrow(DF2)
    colnames(DF2)[1] <- G
    o <- unclass(pos.list)
    o[["pos.by.freq"]] <- DF2
    propby <- lapply(1:nrow(DF2), function(i) DF2[i, -c(1:2)]/rowSums(DF2[, -c(1:2)])[i])
    propby <- sapply(do.call(rbind, propby), round, digits = digits)
    propby[is.nan(propby)] <- 0
    o[["pos.by.prop"]] <- data.frame(DF2[, 1:2], propby, check.names = FALSE)
    class(o) <- "POSby"
    return(o)
}