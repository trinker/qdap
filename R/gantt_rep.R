#' Generate Unit Spans for Repeated Measures
#' 
#' Produces start and end times for occurrences for each repeated measure 
#' condition.
#' 
#' @param rm.var An optional single vector or list of 1 or 2 of repeated 
#' measures to facet by.   
#' @param text.var The text variable.    
#' @param grouping.var The grouping variables. Also takes a single grouping 
#' variable or a list of 1 or more grouping variables.
#' @param units The unit of measurement to analyze.  One of the strings 
#' \code{"character"}, \code{"syllable"}, \code{"word"}, or \code{"sentence"}.
#' @param col.sep The character string to use to separate pasted variables in the 
#' merged grouping variable header/name.
#' @return Returns a data frame of start and end times by repeated measure and 
#' grouping variable(s)
#' @note For non repeated measures data use \code{\link[qdap]{gantt}}; for a 
#' flexible gantt plot that words with code matrix functions (cm) use 
#' \code{\link[qdap]{gantt_wrap}}.
#' @seealso \code{\link[qdap]{gantt}},
#' \code{\link[qdap]{gantt_wrap}},
#' \code{\link[qdap]{gantt_plot}} 
#' @references Clark, W. & Gantt, H. (1922) The Gantt chart, a working 
#' tool of management. New York, Ronald Press.
#' @keywords Gantt
#' @export
#' @examples
#' \dontrun{
#' dat <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex), 
#'     units = "words", col.sep = "_"))
#' head(dat, 20)   
#' gantt_wrap(dat,  "fam.aff_sex",  facet.vars = "act", 
#'     title = "Repeated Measures Gantt Plot",
#'     minor.line.freq = 25, major.line.freq = 100)
#' }
gantt_rep <-
function(rm.var, text.var, grouping.var, units = "words", col.sep = "_"){
    g <- grouping.var
    r <- rm.var
    if (is.list(grouping.var)) {
        m <- unlist(as.character(substitute(grouping.var))[-1])
        m <- sapply(strsplit(m, "$", fixed=TRUE), 
            function(x) x[length(x)])
        #m <- gsub(".", "", m, fixed = TRUE)
        NAME <- paste(m, collapse="&")
    } else {
        G1 <- as.character(substitute(grouping.var))
        NAME <- G1[length(G1)]
    }
    if (is.list(rm.var)) {
        m2 <- unlist(as.character(substitute(rm.var))[-1])
        m2 <- sapply(strsplit(m2, "$", fixed=TRUE), 
            function(x) x[length(x)])
        #m2 <- gsub(".", "", m2, fixed = TRUE)
        NAME2 <- paste(m2, collapse="&")
    } else {
        G2 <- as.character(substitute(rm.var))
        NAME2 <- G2[length(G2)]
    }
    
##     rm.var <- if (is.list(rm.var) & length(rm.var)>1) {
##         apply(data.frame(rm.var), 1, function(x){
##                 if (any(is.na(x))) {
##                     NA 
##                 } else {
##                     paste(x, collapse = ".")
##                 }
##             }
##         )
##     } else {
##         rm.var
##     }  
    
    if (is.list(rm.var) & length(rm.var)>1) {
        rm.var <- paste2(rm.var, sep = col.sep)
    } else {
        rm.var <- unlist(rm.var)
    }     
    
##     grouping.var <- if (is.list(grouping.var) & length(grouping.var)>1) {
##         apply(data.frame(grouping.var), 1, function(x){
##             if (any(is.na(x)))NA else paste(x, collapse = ".")
##             }
##         )
##     } else {
##         grouping.var
##     } 
    
    if (is.list(grouping.var) & length(grouping.var)>1) {
        grouping.var <- paste2(grouping.var, sep = col.sep)
    } else {
        grouping.var <- unlist(grouping.var)
    } 
    
    DAT <- data.frame(rm.var, grouping.var, text.var)
    DAT2 <- split(DAT, rm.var)

    DAT3 <- lapply(seq_along(DAT2), function(i) {
            rm1 <- DAT2[[i]][, 1]
            gn <- DAT2[[i]][, -1]
            gn2 <- gantt(gn[, "text.var"], gn[, "grouping.var"], units = units)
            gn3 <- data.frame(rm.var = rm1[nrow(gn2)], gn2)
            return(gn3)
        }
    )
    DAT3 <- lapply(DAT3, function(x) {
        colnames(x)[ncol(x) - 3] <- NAME
        x
    })
    DAT3 <- do.call("rbind", DAT3)
    names(DAT3)[1:2] <- c(NAME2, NAME)
    if (col.sep != "&") {
        colnames(DAT3) <- gsub("&", col.sep, colnames(DAT3), fixed = TRUE)
    }
    row.names(DAT3) <- 1:nrow(DAT3)
    nrf2 <- sum(gregexpr("col.sep", names(DAT3[, 1, drop = FALSE]))[[1]] < 0)
        if (nrf2==0) RMV <- colSplit(DAT3[, 1, drop = FALSE])
    nrf <- sum(gregexpr("col.sep", names(DAT3[, 2, drop = FALSE]))[[1]] < 0)
        if (nrf==0) GV <- colSplit(DAT3[, 2, drop = FALSE])
    DAT4 <- if (nrf==0){
        data.frame(DAT3[, 2, drop =FALSE], GV, DAT3[, -c(1:2)])
    } else {
        data.frame(DAT3[, 2, drop =FALSE], DAT3[, -c(1:2)])
    }
    DAT3 <- if (nrf2==0){
        data.frame(DAT3[, 1, drop =FALSE], RMV, DAT4)
    } else {
        data.frame(DAT3[, 1, drop =FALSE], DAT4)
    }
    return(DAT3)
}
