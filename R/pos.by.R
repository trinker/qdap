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
pos.by <-
function(text.var, grouping.var = NULL, digits = 2, 
    return.pos = FALSE){
    COM <- comment(text.var)
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
    if (is.null(comment(text.var))){
        pos.list <- pos(text.var, digits = digits)
        text.var <- pos.list[[3]]
    } else {
        com <- comment(text.var)
        text.var <- switch(com,
            POS = text.var[[3]],
            POStagged = {
                o <- text.var[, 2]
                lev <- sort(unique(unlist(o)))
                G4 <- do.call(rbind,lapply(o, function(x,lev){ 
                            tabulate(factor(x,levels = lev,
                            ordered = TRUE),nbins = length(lev))
                        },
                    lev = lev))
                colnames(G4) <-sort(lev)
                G4 <- data.frame(wrd.cnt = text.var[, 3], G4, 
                    check.names = FALSE)
                    if (any(is.na(G4$wrd.cnt))) {
                        nas <- which(is.na(G4$wrd.cnt))
                        G4[nas, 2:ncol(G4)] <- NA
                    }
                G4
            },
        POSprop = {
                text.var <- data.frame(text.var[, 1], 
                    round(sapply(text.var[, -1], 
                    function(x) x*text.var[, 1])), 
                    check.names = FALSE)
                names(text.var) <- gsub("prop", "", names(text.var))
                names(text.var)[1] <- "wrd.cnt"
                text.var
            }, 
        POSfreq = text.var
        )
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
    if (is.null(COM) & return.pos) {
        return(list("p" = pos.list, "outcome" = DF2))
    } else {
        return(DF2)
    }
    if (!is.null(COM)){
        if (COM=="POSprop"){
            W1 <- "Supplied text.var is based on proportions."  
            W2 <- "  Outcomes may be inaccurate."
            warning(paste0(W1, W2))
        }
    }
}
